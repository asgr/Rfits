#A parquet index of the search metadata of a directory (or S3 prefix) of Zarr stores.
#
#Rfits_cutout_zarr_dir decides which stores are worth opening from their headers alone,
#which costs one small read per store. That is a cheap read but not a free one: a release
#of a few thousand tiles asks for a few thousand requests before the first pixel is
#fetched, and every user pays for it again in a new session. The index removes that. It
#holds once what a scan extracts from each store -- the reference position, pixel scale
#and shape that the coarse filter reads, and the full keywords a lazy pointer is built
#from -- in a single parquet file, which can sit beside the tiles it describes so that
#every user queries the same copy instead of rebuilding one.
#
#A few thousand rows is small enough to download whole, but small enough to load is not
#the same as cheap to query, so the rows are asked for in the order a search needs them
#and with arrow's own pushdown rather than by reading a table and subsetting it. First
#the columns the cone filter uses, which are all scalar, so arrow never decodes the
#keyword blob of the thousands of stores nowhere near the position. Then the keywords of
#the handful that survived, selected by store key. arrow also skips whole row groups
#whose statistics cannot match, which is why the file is written in sorted store order.
#
#The file holds three kinds of row, named by the kind column. Store rows carry the
#metadata of one array of one store. Walk rows carry the list of stores found under a
#remote prefix, kept beside the headers because finding them costs a request per directory
#as well. Meta rows are scalars about the file itself, of which the version is the only
#one anyone else reads.

#The format of the file. An index written by a later version is refused rather than half
#read, because a search that quietly found nothing in a file whose columns had moved would
#look exactly like a directory of tiles that did not overlap.
.zarr_index_version = 1L

#The payload column: the whole keyword header of a store, serialised. It is one column
#rather than a wide table of keywords because a search needs the header intact (to rebuild
#the trimmed WCS it filters with, and to hand to a pointer), and no keyword is common
#enough across a mixed release to be worth a column of its own. Splitting it off is what
#makes the light pass light.
.zarr_index_heavy_col = 'keyvalues'

.zarr_index_num_cols = function(){
  return(c('ra', 'dec', 'crpix1', 'crpix2', 'naxis1', 'naxis2', 'pixscale_x',
           'pixscale_y', 'created'))
}

.zarr_index_list_cols = function(){
  return(c('array_dim', 'stores', 'keyvalues'))
}

#The columns, in the order they are written. kind separates the three sorts of row;
#cache_key is what they are looked up by, and is a quoted column name rather than key
#because data.table() has an argument of that name.
.zarr_index_cols = function(){
  return(c('kind', 'cache_key', 'store', 'extname', 'label', 'status', 'error', 'type',
           'stamp', .zarr_index_num_cols(), 'has_keyvalues', 'array_dim', 'stores',
           'text_value', .zarr_index_heavy_col))
}

#Everything but the keywords, which is what a search filters with
.zarr_index_light_cols = function(){
  return(setdiff(.zarr_index_cols(), .zarr_index_heavy_col))
}

#The empty value of a payload column. NULL cannot be used, because arrow works out a list
#column's type from the values it is given and the first row of an index is often one with
#nothing in it (a store that could not be read, or a file with no walk yet).
.zarr_index_empty = function(col){
  if(identical(col, 'keyvalues')){
    return(raw(0))
  }
  if(identical(col, 'array_dim')){
    return(integer(0))
  }
  return(character(0))
}

#The types, pinned rather than inferred for the same reason. A parquet file written by
#hand (or by another tool) is still readable, since nothing here depends on the writer's
#type choices beyond these columns.
#The value of one payload cell, in the form arrow wants: a bare atomic vector, never a
#list of one and never NULL. Rows are built with the wrapped spelling because that is what
#keeps an empty cell typed through data.frame(), while a value read back from parquet
#arrives bare, and a single row subset can arrive as the vector itself. The rule differs
#per column because the three hold different things: keyvalues and array_dim are one
#vector each so a list wrapper is peeled, whereas stores is a vector whose length varies
#per row and so must be flattened rather than peeled.
.zarr_index_cell = function(col, val){
  if(col == 'stores'){
    if(is.null(val)){
      return(character(0))
    }
    if(is.list(val)){
      val = unlist(val, use.names = FALSE)
    }
    if(length(val) == 0){
      return(character(0))
    }
    return(as.character(val))
  }
  if(is.list(val)){
    val = if(length(val) == 0) NULL else val[[1]]
  }
  if(is.null(val)){
    return(.zarr_index_empty(col))
  }
  if(col == 'array_dim'){
    return(as.integer(val))
  }
  return(as.raw(val))
}

.zarr_index_schema = function(){
  return(arrow::schema(
    kind = arrow::string(), cache_key = arrow::string(), store = arrow::string(),
    extname = arrow::string(), label = arrow::string(), status = arrow::string(),
    error = arrow::string(), type = arrow::string(), stamp = arrow::string(),
    ra = arrow::float64(), dec = arrow::float64(), crpix1 = arrow::float64(),
    crpix2 = arrow::float64(), naxis1 = arrow::float64(), naxis2 = arrow::float64(),
    pixscale_x = arrow::float64(), pixscale_y = arrow::float64(),
    created = arrow::float64(), has_keyvalues = arrow::bool(),
    array_dim = arrow::list_of(arrow::int32()), stores = arrow::list_of(arrow::string()),
    text_value = arrow::string(), keyvalues = arrow::binary()))
}

#arrow rather than nanoparquet, the other CRAN parquet reader. arrow is the one that can
#push both a row filter and a column projection into the scan, which is the point of
#querying an index rather than loading a table.
.zarr_index_require_arrow = function(what = 'to read or write a Zarr index'){
  if(!requireNamespace('arrow', quietly = TRUE)){
    stop('The arrow package is needed ', what, '. Please install it from CRAN.',
         call. = FALSE)
  }
}

.zarr_index_blank_row = function(){
  row = lapply(.zarr_index_cols(), function(col){
    if(col %in% .zarr_index_list_cols()){
      return(list(.zarr_index_empty(col)))
    }
    if(col %in% .zarr_index_num_cols()){
      return(NA_real_)
    }
    if(identical(col, 'has_keyvalues')){
      return(FALSE)
    }
    return(NA_character_)
  })
  names(row) = .zarr_index_cols()
  return(row)
}

#The key of a store row: which store, and which array of it. One store may hold several
#extensions and they do not share a header, so both are needed. The label is the store as
#the search names it (a path, or an s3:// URI), so a local mirror of a bucket and the
#bucket itself cannot overwrite each other's rows.
.zarr_index_store_key = function(label, extname){
  return(paste0(label, '|', extname))
}

#The key of a walk row. max_dirs belongs to the spec because a walk that stopped early
#holds a partial list, and serving a broad search from one would hide stores silently.
.zarr_index_walk_key = function(bucket, prefix, recursive, pattern, max_dirs){
  return(paste0('WALK|', bucket, '|', prefix, '|', isTRUE(recursive), '|',
                paste0(pattern, collapse = ','), '|', max_dirs))
}

.zarr_index_meta_key = function(name){
  return(paste0('META|', name))
}

#A data.table of index rows from a list of one row per store. Built column by column
#rather than with data.frame(), which would turn the list columns into characters, and
#with the payload columns forced to their empty value where a row has nothing.
.zarr_index_rows = function(rows){
  #A data.frame in already, which is what the readers hand back; a list of one row list
  #per store, which is what the scan builds
  if(is.data.frame(rows)){
    return(.zarr_index_rows_clean(rows))
  }
  cols = .zarr_index_cols()
  rows = rows[!vapply(rows, is.null, logical(1))]
  if(length(rows) == 0){
    out = lapply(cols, function(col){
      if(col %in% .zarr_index_list_cols()){
        return(list())
      }
      if(col %in% .zarr_index_num_cols()){
        return(numeric(0))
      }
      if(identical(col, 'has_keyvalues')){
        return(logical(0))
      }
      return(character(0))
    })
    names(out) = cols
    return(data.table::as.data.table(out))
  }
  out = lapply(cols, function(col){
    if(col %in% .zarr_index_list_cols()){
      return(lapply(rows, function(r) .zarr_index_cell(col, r[[col]])))
    }
    if(col %in% .zarr_index_num_cols()){
      return(vapply(rows, function(r){
        val = r[[col]]
        if(is.null(val) || length(val) == 0){
          return(NA_real_)
        }
        return(as.numeric(val)[1])
      }, numeric(1)))
    }
    if(identical(col, 'has_keyvalues')){
      return(vapply(rows, function(r) isTRUE(r[[col]]), logical(1)))
    }
    return(vapply(rows, function(r){
      val = r[[col]]
      if(is.null(val) || length(val) == 0){
        return(NA_character_)
      }
      return(as.character(val)[1])
    }, character(1)))
  })
  names(out) = cols
  return(data.table::as.data.table(out))
}

#The row one searched store contributes. info is what .zarr_cutout_tile_info makes of a
#store's header, which holds both the two axis view the search filters with and the full
#keywords and shape a pointer needs. Only the keywords and the shape are kept beside the
#scalars: the trimmed header is rebuilt from the keywords on read, so the copy a pointer
#carries can never have lost its third axis.
.zarr_index_store_row = function(label, store, extname, info, stamp = NA_character_){
  if(is.null(info)){
    return(NULL)
  }
  row = .zarr_index_blank_row()
  row$kind = 'store'
  row$cache_key = .zarr_index_store_key(label, extname)
  row$store = store
  row$extname = extname
  row$label = label
  row$status = info$status
  if(!is.null(info$error)){
    row$error = info$error
  }
  row$stamp = stamp
  row$created = as.numeric(Sys.time())

  #A store that could not be searched is recorded all the same, so the next run knows it
  #was asked and what the answer was. Without these rows a directory whose extname had
  #been mistyped would be re-read in full every time, which is the cost the index exists
  #to remove.
  if(!identical(info$status, 'ok')){
    return(row)
  }

  full = info$full_keyvalues
  if(is.null(full)){
    full = info$keyvalues
  }
  dim = info$dim
  if(is.null(dim) || length(dim) == 0 || is.null(full)){
    #Nothing usable to write, so this is a miss rather than a row: an empty ok row would
    #make a later search believe the store had already been read
    return(NULL)
  }
  row$type = info$type
  row$ra = as.numeric(info$ra)
  row$dec = as.numeric(info$dec)
  row$crpix1 = as.numeric(info$crpix[1])
  row$crpix2 = as.numeric(info$crpix[2])
  row$naxis1 = as.numeric(info$naxis[1])
  row$naxis2 = as.numeric(info$naxis[2])
  row$pixscale_x = as.numeric(info$scale$x)
  row$pixscale_y = as.numeric(info$scale$y)
  row$array_dim = list(as.integer(dim))
  row$keyvalues = list(serialize(full, connection = NULL, version = 2))
  row$has_keyvalues = TRUE
  return(row)
}

.zarr_index_walk_row = function(cache_key, stores){
  row = .zarr_index_blank_row()
  row$kind = 'walk'
  row$cache_key = cache_key
  row$stores = list(as.character(stores))
  row$created = as.numeric(Sys.time())
  return(row)
}

.zarr_index_meta_row = function(name, value){
  row = .zarr_index_blank_row()
  row$kind = 'meta'
  row$cache_key = .zarr_index_meta_key(name)
  row$text_value = as.character(value)
  row$created = as.numeric(Sys.time())
  return(row)
}

#Turn the index row of one store back into the tile record the search works with. Without
#the keywords a tile can be filtered with but not pointed at, which is exactly the state
#the cheap stage wants; with them it is the same list .zarr_cutout_tile_info returns.
.zarr_index_tile = function(row, keyvalues = NULL){
  #The cell helpers are needed rather than the raw column, because a single row pulled out
  #of a data.table gives a one element list for a payload column
  dim = .zarr_index_cell('array_dim', row$array_dim)
  if(length(dim) == 0){
    dim = c(row$naxis1, row$naxis2)
  }
  tile = list(status = row$status, keyvalues = NULL, full_keyvalues = NULL,
              naxis = c(row$naxis1, row$naxis2),
              scale = list(x = row$pixscale_x, y = row$pixscale_y),
              ra = row$ra, dec = row$dec, crpix = c(row$crpix1, row$crpix2),
              store_shape = as.integer(dim), dim = as.integer(dim), type = row$type)
  if(is.null(keyvalues) || length(keyvalues) == 0){
    return(tile)
  }
  full = tryCatch(unserialize(keyvalues), error = function(e) NULL)
  if(is.null(full)){
    return(list(status = 'error', error = 'the index keywords could not be unserialised'))
  }
  #Only the trimmed copy may be given to Rwcs (see .wcs2_axes), and it is rebuilt here
  #rather than stored so that the two cannot drift apart
  tile$keyvalues = .wcs2_axes(full)$keyvalues
  tile$full_keyvalues = full
  return(tile)
}

#Can a row be pointed at? Only 'ok' stores are, and those must carry the full keywords and
#the array type. A row written without them is treated as a miss and re-read, rather than
#handed back as a pointer whose header is quietly missing an axis.
.zarr_index_row_usable = function(row){
  if(is.na(row$status) || !identical(row$status, 'ok')){
    return(TRUE)
  }
  return(isTRUE(row$has_keyvalues) && !is.na(row$type) && !is.na(row$naxis1))
}

#The stamp a local store's metadata file carries, which is what its index row is checked
#against. Size and mtime cost a stat rather than a read, so a header that has genuinely
#changed is noticed on its own rather than waiting for someone to pass refresh. Over S3
#the only check available is the request the index exists to avoid, so remote rows are
#trusted and refresh is the documented way to ignore them.
.zarr_index_stamp = function(store, extname){
  info = file.info(file.path(store, .zarr_meta_key(extname)))
  if(is.na(info$size) || isTRUE(info$isdir) || info$size <= 0){
    return(NA_character_)
  }
  return(paste0(info$size, '-', as.numeric(info$mtime)))
}

#arrow expressions, built through arrow's own namespace so that nothing here depends on
#which of the several packages that export read_parquet happens to be attached
.zarr_idx_field = function(col){
  return(arrow::Expression$field_ref(col))
}

.zarr_idx_is = function(col, val){
  if(length(val) == 1 && is.na(val)){
    return(arrow::Expression$create('is_null', .zarr_idx_field(col)))
  }
  return(.zarr_idx_field(col) == arrow::Expression$scalar(val))
}

.zarr_idx_and = function(a, b){
  if(is.null(a)){
    return(b)
  }
  if(is.null(b)){
    return(a)
  }
  return(arrow::Expression$create('and', a, b))
}

.zarr_idx_or = function(a, b){
  if(is.null(a)){
    return(b)
  }
  if(is.null(b)){
    return(a)
  }
  return(arrow::Expression$create('or', a, b))
}

#A filter matching any of a set of exact keys. arrow's is_in cannot be built from R in the
#version this was written against, so the keys are or-ed, which is what arrow's own dplyr
#backend does for %in% as well. The set is the survivors of the cheap stage and so is
#small; a search that needed thousands of keys would be better off reading the whole light
#pass, which it has already done.
.zarr_idx_keys = function(col, keys){
  ex = NULL
  for(k in keys){
    ex = .zarr_idx_or(ex, .zarr_idx_is(col, k))
  }
  return(ex)
}

#Query an index file. Every read of an index goes through the scanner rather than through
#read_parquet and [ so that unqueried columns are never decoded and row groups whose
#statistics cannot match are never read; a file with nothing to return is the ordinary
#case for a search over one corner of a big release, not an error.
.zarr_index_scan = function(path, cols, filter = NULL){
  .zarr_index_require_arrow('to read a Zarr index')
  tab = arrow::Scanner$create(arrow::open_dataset(path), projection = cols,
                              filter = filter)$ToTable()
  out = as.data.frame(tab, stringsAsFactors = FALSE)
  #A zero row result drops the list columns to something unusable, so they are restored
  #to the shape the callers index by
  for(col in .zarr_index_list_cols()){
    if(col %in% cols){
      out[[col]] = if(nrow(out) == 0){
        list()
      }else{
        as.list(out[[col]])
      }
    }
  }
  return(out)
}

#The rows a search filters with, which is every column but the keywords.
.zarr_index_light_rows = function(path){
  return(.zarr_index_scan(path, .zarr_index_light_cols(), .zarr_idx_is('kind', 'store')))
}

#The keywords of specific stores, by key. Kept apart from the light pass so the blobs of
#the stores that were filtered out are never read.
.zarr_index_payload_rows = function(path, keys){
  cols = c('cache_key', .zarr_index_heavy_col)
  if(length(keys) == 0){
    #The same columns, no rows. Filtering on an impossible key keeps the shape of the
    #result the same as a query that matched, which is what the callers index by.
    return(.zarr_index_scan(path, cols, .zarr_idx_is('cache_key', NA_character_)))
  }
  return(.zarr_index_scan(path, cols, .zarr_idx_keys('cache_key', keys)))
}

.zarr_index_walk_rows = function(path, keys = NULL){
  ex = .zarr_idx_is('kind', 'walk')
  if(!is.null(keys)){
    ex = .zarr_idx_and(ex, .zarr_idx_keys('cache_key', keys))
  }
  return(.zarr_index_scan(path, c('kind', 'cache_key', 'stores', 'created'), ex))
}

.zarr_index_meta_rows = function(path){
  return(.zarr_index_scan(path, c('kind', 'cache_key', 'text_value', 'created'),
                          .zarr_idx_is('kind', 'meta')))
}

.zarr_index_version_of = function(path){
  rows = .zarr_index_meta_rows(path)
  at = which(rows$cache_key == .zarr_index_meta_key('index_version'))
  if(length(at) == 0){
    return(NA_integer_)
  }
  return(suppressWarnings(as.integer(rows$text_value[at[1]])))
}

#How many index rows go in a parquet row group. arrow's default puts the whole table in
#one group when the file is small, which is right for most data and wrong here: the
#keywords dominate the file by volume, and a group that holds all of them cannot be skipped,
#so selecting the keywords of twelve stores decodes the headers of all five thousand.
#Groups of a few hundred rows let the key filter read only the groups that hold the keys it
#wants, and cost little elsewhere because the light pass reads none of that column anyway.
.zarr_index_row_group = 500L

#Write rows out as parquet. Sorted by cache key so the statistics arrow tests before
#reading a group at all describe a contiguous slice of the directory rather than a
#permutation of it.
.zarr_index_write = function(dt, path){
  .zarr_index_require_arrow('to write a Zarr index')
  dt = data.table::copy(data.table::as.data.table(dt))
  cols = .zarr_index_cols()
  for(col in setdiff(cols, names(dt))){
    n = nrow(dt)
    dt[[col]] = if(col %in% .zarr_index_list_cols()){
      rep(list(.zarr_index_empty(col)), n)
    }else if(col %in% .zarr_index_num_cols()){
      rep(NA_real_, n)
    }else if(identical(col, 'has_keyvalues')){
      rep(FALSE, n)
    }else{
      rep(NA_character_, n)
    }
  }
  for(col in .zarr_index_list_cols()){
    dt[[col]] = lapply(dt[[col]], function(val){
      if(is.null(val)){
        val = .zarr_index_empty(col)
      }
      val
    })
  }
  dt = dt[order(dt$cache_key), cols, with = FALSE]
  #The types come from the schema rather than the data, because the first row of an index
  #is often one with an empty payload and an inferred column would take its type from that
  tab = arrow::Table$create(dt, schema = .zarr_index_schema())
  arrow::write_parquet(tab, path, chunk_size = .zarr_index_row_group)
  return(invisible(path))
}

#Merge new rows over the ones an index already holds. Untouched rows carry over, so a run
#narrowed by pattern does not discard the work every other run put into the file.
.zarr_index_merge = function(old, new){
  if(is.null(old) || nrow(old) == 0){
    return(new)
  }
  if(is.null(new) || nrow(new) == 0){
    return(old)
  }
  keep = old[!(old$cache_key %in% new$cache_key), ]
  if(nrow(keep) == 0){
    return(new)
  }
  return(rbind(keep, new))
}

#The full contents of an index, every column. Only used when an index is about to be
#rewritten, because a merge that carried over rows without their keywords would quietly
#strip the stores this run did not visit. For that it is the one place where reading the
#whole file is right, and the file is a few hundred kB.
.zarr_index_load_all = function(path){
  .zarr_index_require_arrow('to read a Zarr index')
  dt = tryCatch(arrow::read_parquet(path), error = function(e) NULL)
  if(is.null(dt)){
    return(.zarr_index_rows(list()))
  }
  return(.zarr_index_rows_clean(dt))
}

#Bring a data.table read back from parquet into the shape the writers and readers here
#expect: the columns that were queried, in the written order, list columns as plain lists,
#and no row of a payload column holding NULL where an empty vector was written. Columns
#that were not queried are left absent rather than filled in, so that a light row cannot be
#mistaken for a store whose keywords are known to be missing.
.zarr_index_rows_clean = function(dt){
  keep = intersect(.zarr_index_cols(), names(dt))
  for(col in intersect(.zarr_index_list_cols(), keep)){
    dt[[col]] = lapply(dt[[col]], function(val) .zarr_index_cell(col, val))
  }
  return(dt[, keep, with = FALSE])
}

#The object key of an index. A bare name is put under the prefix the tiles live under, so
#that the index and the data it describes are found together and share one set of
#credentials. A name that already carries a path is taken as the key itself, since that is
#how an index is addressed once it has been published.
.zarr_index_object_key = function(index, prefix = NULL){
  name = sub('\\.rds$', '', index)
  if(!grepl('\\.parquet$', name)){
    name = paste0(name, '.parquet')
  }
  if(grepl('/', name) || is.null(prefix) || !nzchar(prefix)){
    return(sub('^/+', '', name))
  }
  return(paste0(.zarr_s3_prefix(prefix), name))
}

#Where an index is. A bare name with no bucket is a local path. A bare name with a bucket
#is an object under the prefix being searched, which is how an index comes to sit beside
#the tiles it describes and be reachable by anyone who can reach them. An s3:// URI says
#both, and is the form a user of a published index is handed.
.zarr_index_resolve = function(index, bucket = NULL, prefix = NULL){
  if(grepl('^s3://', index)){
    rest = sub('^s3://', '', index)
    if(!grepl('/', rest)){
      stop('An s3:// index path must name an object, e.g. ',
           "'s3://my-bucket/survey/index.parquet'!", call. = FALSE)
    }
    return(list(index = sub('^[^/]+/', '', rest),
                bucket = sub('/.*$', '', rest), prefix = NULL))
  }
  return(list(index = index, bucket = bucket, prefix = prefix))
}

#The local file an index argument names, with the extension the format wants. An index is
#named the way a store is, by its stem, so that the extension can be left off.
.zarr_index_local_path = function(index){
  index = path.expand(index)
  #Caches were .rds files before this format, and a caller who has kept passing the same
  #path should find the index where they expect rather than beside an old cache
  index = sub('\\.rds$', '', index)
  if(!grepl('\\.parquet$', index)){
    index = paste0(index, '.parquet')
  }
  return(index)
}

#Reach an index, local or remote. A remote one is downloaded rather than opened in place
#because the queries here are arrow scans, and an arrow scan needs a seekable file: the
#alternative is one GET per page of a file that is a few hundred kB anyway, which is what
#the pushdown was supposed to avoid.
.zarr_index_open = function(index, bucket = NULL, prefix = NULL, client = NULL,
                           region = NULL, endpoint = NULL, access_key = NULL,
                           secret_key = NULL, session_token = NULL){
  where = .zarr_index_resolve(index, bucket = bucket, prefix = prefix)
  index = where$index
  bucket = where$bucket
  prefix = where$prefix
  if(is.null(bucket)){
    path = .zarr_index_local_path(index)
    return(list(path = if(file.exists(path)) path else NULL, shown = path,
                remote = FALSE, tempfile = FALSE))
  }
  key = .zarr_index_object_key(index, prefix)
  if(is.null(client)){
    client = .zarr_index_client(bucket = bucket, region = region, endpoint = endpoint,
                                access_key = access_key, secret_key = secret_key,
                                session_token = session_token)
  }
  shown = paste0('s3://', bucket, '/', key)
  got = tryCatch(client$get_object(Bucket = bucket, Key = key), error = function(e) e)
  if(inherits(got, 'error')){
    if(.zarr_s3_not_found(conditionMessage(got))){
      #Absent is ordinary: the first run against a release that has no index yet is how
      #every index starts. Anything else (notably AccessDenied) is not, and must not be
      #taken for permission to go and build one from scratch
      return(list(path = NULL, shown = shown, remote = TRUE, tempfile = FALSE,
                  missing = TRUE))
    }
    stop('Cannot read the Zarr index at ', shown, ': ', conditionMessage(got),
         call. = FALSE)
  }
  body = got$Body
  if(is.character(body)){
    body = charToRaw(body)
  }
  if(!(is.raw(body) && length(body) > 0)){
    return(list(path = NULL, shown = shown, remote = TRUE, tempfile = FALSE,
                missing = TRUE))
  }
  dest = tempfile(fileext = '.parquet')
  writeBin(body, dest)
  return(list(path = dest, shown = shown, remote = TRUE, tempfile = TRUE,
              missing = FALSE))
}

.zarr_index_close = function(src){
  if(isTRUE(src$tempfile) && !is.null(src$path)){
    unlink(src$path)
  }
  return(invisible(NULL))
}

.zarr_index_client = function(bucket, region = NULL, endpoint = NULL, access_key = NULL,
                              secret_key = NULL, session_token = NULL){
  if(!requireNamespace('paws.storage', quietly = TRUE)){
    stop('The paws.storage package is needed to reach a Zarr index over S3. Please ',
         'install it from CRAN.', call. = FALSE)
  }
  creds = .zarr_s3_env_defaults(region = region, endpoint = endpoint,
                               access_key = access_key, secret_key = secret_key,
                               session_token = session_token)
  if(is.null(creds$access_key) || is.null(creds$secret_key)){
    stop('access_key and secret_key are required to reach a Zarr index over S3, whether ',
         'given directly or by RFITS_S3_ACCESS_KEY and RFITS_S3_SECRET_KEY.',
         call. = FALSE)
  }
  return(paws.storage::s3(config = .zarr_s3_config(region = creds$region,
                                                  endpoint = creds$endpoint,
                                                  access_key = creds$access_key,
                                                  secret_key = creds$secret_key,
                                                  session_token = creds$session_token)))
}

#Write an index out, locally or to a bucket. Remotely the bytes are built in a temporary
#file and put as one object, because parquet writes are not something an S3 client can be
#handed a stream to in pieces here, and a half written index is worse than none: it is the
#one artefact every later search reads.
.zarr_index_put = function(dt, index, bucket = NULL, prefix = NULL, client = NULL,
                           region = NULL, endpoint = NULL, access_key = NULL,
                           secret_key = NULL, session_token = NULL){
  where = .zarr_index_resolve(index, bucket = bucket, prefix = prefix)
  index = where$index
  bucket = where$bucket
  prefix = where$prefix
  tmp = tempfile(fileext = '.parquet')
  .zarr_index_write(dt, tmp)
  if(is.null(bucket)){
    dest = .zarr_index_local_path(index)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    moved = file.rename(tmp, dest)
    if(!moved){
      file.copy(tmp, dest, overwrite = TRUE)
      unlink(tmp)
    }
    return(invisible(dest))
  }
  key = .zarr_index_object_key(index, prefix)
  if(is.null(client)){
    client = .zarr_index_client(bucket = bucket, region = region, endpoint = endpoint,
                                access_key = access_key, secret_key = secret_key,
                                session_token = session_token)
  }
  size = file.info(tmp)$size
  body = readBin(tmp, what = 'raw', n = as.integer(size))
  unlink(tmp)
  put = tryCatch(client$put_object(Bucket = bucket, Key = key, Body = body),
                 error = function(e) e)
  if(inherits(put, 'error')){
    stop('Cannot write the Zarr index to s3://', bucket, '/', key, ': ',
         conditionMessage(put), call. = FALSE)
  }
  return(invisible(paste0('s3://', bucket, '/', key)))
}

#Extract the search metadata of a directory of Zarr stores into a data.table, optionally
#writing it as the parquet file Rfits_cutout_zarr_dir reads. This is the same extraction
#the search performs on the fly, done once and kept, so that a release can publish one
#file beside its tiles and every user's first query then costs that file rather than one
#small read per store.
#
#The rows returned are the light rows: every column the search filters with, plus a
#has_keyvalues flag, but without the serialised keywords. Those are the bulk of the file
#and are only wanted for the stores a search actually cuts, so they are fetched separately
#(by Rfits_cutout_zarr_dir, or by Rfits_zarr_index_query with keywords = TRUE).
Rfits_zarr_index = function(dir = NULL, filelist = NULL, pattern = NULL, recursive = TRUE,
                            bucket = NULL, prefix = '',
                            region = NULL, endpoint = NULL,
                            access_key = NULL, secret_key = NULL, session_token = NULL,
                            extname = 'data1',
                            index = NULL, refresh = FALSE,
                            data.table = TRUE, verbose = TRUE, ...){
  .zarr_require()
  assertString(dir, null.ok = TRUE)
  assertCharacter(filelist, null.ok = TRUE)
  assertCharacter(pattern, null.ok = TRUE)
  assertFlag(recursive)
  assertFlag(verbose)
  assertString(bucket, null.ok = TRUE)
  assertString(prefix)
  assertString(region, null.ok = TRUE)
  assertString(endpoint, null.ok = TRUE)
  assertString(access_key, null.ok = TRUE)
  assertString(secret_key, null.ok = TRUE)
  assertString(session_token, null.ok = TRUE)
  assertCharacter(extname, min.len = 1)
  assertString(index, null.ok = TRUE)
  assertFlag(refresh)
  assertFlag(data.table)

  if(is.null(bucket) && is.null(dir) && is.null(filelist) && is.null(index)){
    stop('One of dir, filelist, bucket, or index is required!', call. = FALSE)
  }
  if(!is.null(bucket) && (!is.null(dir) || !is.null(filelist))){
    stop('Give bucket (a remote prefix) or dir/filelist (local stores), not both!',
         call. = FALSE)
  }

  #An existing index answers the question without the stores being read at all, which is
  #the point of having built one. refresh is what says otherwise.
  if(!is.null(index) && !isTRUE(refresh)){
    src = .zarr_index_open(index, bucket = bucket, prefix = prefix, region = region,
                           endpoint = endpoint, access_key = access_key,
                           secret_key = secret_key, session_token = session_token)
    if(!is.null(src$path)){
      on.exit(.zarr_index_close(src))
      ver = .zarr_index_version_of(src$path)
      if(is.na(ver)){
        stop('The Zarr index at ', src$shown, ' has no version and cannot be read. ',
             'Rebuild it with refresh = TRUE.', call. = FALSE)
      }
      if(ver > .zarr_index_version){
        stop('The Zarr index at ', src$shown, ' was written by a newer version of ',
             'Rfits (', ver, ' > ', .zarr_index_version, ') and cannot be read safely. ',
             'Update the package, or rebuild it with refresh = TRUE.', call. = FALSE)
      }
      rows = .zarr_index_light_rows(src$path)
      if(verbose){
        message('Read ', nrow(rows), ' store row(s) from ', src$shown)
      }
      out = .zarr_index_rows(as.data.frame(rows, stringsAsFactors = FALSE))
      if(!data.table){
        out = as.data.frame(out, stringsAsFactors = FALSE)
      }
      return(invisible(out))
    }
    .zarr_index_close(src)
  }

  stores = .zarr_index_find_stores(dir = dir, filelist = filelist, pattern = pattern,
                                   recursive = recursive, bucket = bucket, prefix = prefix,
                                   region = region, endpoint = endpoint,
                                   access_key = access_key, secret_key = secret_key,
                                   session_token = session_token, ...)
  if(verbose){
    message('Indexing ', length(stores$store), ' Zarr store(s)')
  }

  rows = .zarr_index_scan_stores(stores, extname = extname)
  ok = sum(vapply(rows, function(r) !is.null(r), logical(1)))
  if(verbose){
    message('Recorded ', ok, ' store(s) with readable metadata')
  }
  dt = .zarr_index_rows(rows)
  dt = dt[order(dt$label, dt$extname), ]

  if(!is.null(index)){
    #The index is written whether or not stores were found this time, because the walk
    #list is one of the things it holds and a search that found nothing under a prefix is
    #still an answer worth keeping. Existing rows are merged rather than replaced, so an
    #index narrowed by pattern cannot throw away the work of a broader run.
    old = NULL
    src = NULL
    if(!isTRUE(refresh)){
      src = .zarr_index_open(index, bucket = bucket, prefix = prefix, region = region,
                             endpoint = endpoint, access_key = access_key,
                             secret_key = secret_key, session_token = session_token)
      if(!is.null(src$path)){
        old = .zarr_index_load_all(src$path)
      }
      .zarr_index_close(src)
    }
    #The walk is recorded too, since a remote listing costs a request per directory and
    #is otherwise repeated on the next search. Only a walk this run completed is kept:
    #one that stopped at max_dirs holds a partial list.
    rows = list(.zarr_index_meta_row('index_version', .zarr_index_version))
    if(!is.null(stores$walked)){
      rows = c(rows, list(.zarr_index_walk_row(stores$walk_key, stores$walked)))
    }
    out = .zarr_index_merge(.zarr_index_merge(old, dt), .zarr_index_rows(rows))
    written = .zarr_index_put(out, index, bucket = bucket, prefix = prefix,
                              region = region, endpoint = endpoint,
                              access_key = access_key, secret_key = secret_key,
                              session_token = session_token)
    #The remote write goes through paws, so a client built here has to be discarded with
    #the ones the walk used; both are local objects and neither holds a connection
    if(verbose){
      message('Wrote Zarr index: ', written, ' (', nrow(out), ' row(s))')
    }
  }

  if(!data.table){
    dt = as.data.frame(dt, stringsAsFactors = FALSE)
  }
  return(invisible(dt))
}

#The stores to index, and the labels they are searched under. Shared with
#Rfits_cutout_zarr_dir so that the two cannot disagree about which store is which.
.zarr_index_find_stores = function(dir = NULL, filelist = NULL, pattern = NULL,
                                   recursive = TRUE, bucket = NULL, prefix = '',
                                   region = NULL, endpoint = NULL, access_key = NULL,
                                   secret_key = NULL, session_token = NULL,
                                   max_dirs = 1000, index_path = NULL, refresh = FALSE,
                                   client = NULL, verbose = FALSE){
  remote = !is.null(bucket)
  walked = NULL
  if(remote){
    if(is.null(client)){
      client = .zarr_index_client(bucket = bucket, region = region, endpoint = endpoint,
                                  access_key = access_key, secret_key = secret_key,
                                  session_token = session_token)
    }
    walk_key = .zarr_index_walk_key(bucket, prefix, recursive, pattern, max_dirs)
    cached = NULL
    #index_path is the local copy of an already downloaded index, not the spec of where it
    #is. A remote search reads the object once and passes that copy to both lookups, so a
    #search never pays twice for the same download.
    if(!is.null(index_path) && !isTRUE(refresh)){
      walks = .zarr_index_walk_rows(index_path, keys = walk_key)
      if(nrow(walks) > 0 && length(walks$stores[[1]]) > 0){
        cached = walks$stores[[1]]
      }
    }
    if(!is.null(cached)){
      found = as.character(cached)
      if(verbose){
        message('Using cached store list for s3://', bucket, '/',
                .zarr_s3_prefix(prefix), ' (', length(found),
                ' stores); refresh = TRUE to re-list')
      }
    }else{
      #A walk that gave up at max_dirs holds a partial list, and caching that would hide
      #stores from every later search without saying so
      truncated = FALSE
      found = withCallingHandlers(
        .zarr_s3_find_stores(client, bucket, prefix, recursive = recursive,
                             pattern = pattern, max_dirs = max_dirs),
        warning = function(w){
          #No muffleRestart, so the warning still reaches the caller: it is their only
          #notice that the search saw part of the directory
          if(grepl('directories; use a narrower prefix', conditionMessage(w))){
            truncated <<- TRUE
          }
        })
      if(!truncated){
        walked = found
      }
    }
    if(length(found) == 0){
      stop('No Zarr stores found under s3://', bucket, '/', .zarr_s3_prefix(prefix),
           if(length(pattern) > 0) ' with the given pattern' else '', '!', call. = FALSE)
    }
    return(list(store = found, label = paste0('s3://', bucket, '/', found),
                remote = TRUE, bucket = bucket, client = client, walked = walked,
                walk_key = walk_key))
  }

  if(is.null(filelist)){
    dir = path.expand(dir)
    if(!dir.exists(dir)){
      stop('Directory does not exist: ', dir, call. = FALSE)
    }
    #A Zarr store is a directory, so the search is for directories, not files.
    #list.files reports what is below dir and never dir itself, so a directory named
    #*.zarr is offered as its own store here, matching the remote path which probes the
    #prefix before listing it.
    found = character(0)
    if(grepl('\\.zarr$', dir)){
      found = dir
    }
    below = list.files(dir, full.names = TRUE, include.dirs = TRUE, recursive = recursive)
    found = c(found, grep('\\.zarr$', below, value = TRUE))
    found = found[dir.exists(found)]
  }else{
    found = path.expand(filelist)
    found = found[dir.exists(found)]
  }
  if(length(pattern) > 0){
    for(p in pattern){
      found = grep(p, found, value = TRUE)
    }
  }
  found = sort(unique(found))
  if(length(found) == 0){
    stop('No Zarr stores found', if(!is.null(dir)) paste0(' in ', dir) else '',
         if(length(pattern) > 0) ' with the given pattern' else '', '!', call. = FALSE)
  }
  return(list(store = found, label = found, remote = FALSE, walked = NULL,
              walk_key = NULL))
}

#Read the search metadata of each store, in the order asked for, trying each extension
#candidate in turn. Returns one row list per store (or NULL where nothing could be read),
#which both the standalone builder and the search consume.
.zarr_index_scan_stores = function(stores, extname){
  out = vector(mode = 'list', length = length(stores$store))
  for(i in seq_along(stores$store)){
    read = .zarr_store_meta(stores$store[i], stores$label[i], extname = extname,
                           remote = stores$remote, client = stores$client,
                           bucket = stores$bucket)
    stamp = NA_character_
    if(!stores$remote && !is.null(read$extname)){
      stamp = .zarr_index_stamp(stores$store[i], read$extname)
    }
    out[[i]] = .zarr_index_store_row(label = stores$label[i], store = stores$store[i],
                                     extname = read$extname, info = read$info,
                                     stamp = stamp)
  }
  return(out)
}

#The metadata of one store, and which of the candidate extensions it came from. Reads the
#array document directly where it can, since opening a store costs a probe and a hierarchy
#walk before a single keyword is in hand.
.zarr_store_meta = function(store, label = store, extname = 'data1', remote = FALSE,
                            client = NULL, bucket = NULL){
  for(name in extname){
    if(remote){
      meta = .zarr_meta_from_s3(client, bucket, store, name)
      if(is.null(meta)){
        #nothing at that key, so try the next candidate
        next
      }
      if(!is.null(attr(meta, 'error'))){
        return(list(info = list(status = 'error', error = attr(meta, 'error')),
                    extname = NULL))
      }
      return(list(info = .zarr_cutout_tile_info(meta$keyvalues, meta$shape),
                  extname = name))
    }
    meta = .zarr_meta_from_dir(store, name)
    if(is.null(meta)){
      #Either a v2 store or one whose array is not named as asked. Only now is it worth
      #opening the store properly, which also gives a better message than 'not found'
      #when the store is genuinely unreadable.
      opened = tryCatch(Rfits_point_zarr(store, extname = name, header = TRUE),
                        error = function(e) e)
      if(inherits(opened, 'error')){
        msg = conditionMessage(opened)
        if(grepl('does not exist in the Zarr store', msg)){
          next
        }
        return(list(info = list(status = 'error', error = msg), extname = NULL))
      }
      return(list(info = .zarr_cutout_tile_info(opened$keyvalues, opened$dim),
                  extname = name))
    }
    return(list(info = .zarr_cutout_tile_info(meta$keyvalues, meta$shape),
                extname = name))
  }
  return(list(info = NULL, extname = NULL))
}

#Ask an index which stores could overlap a set of positions, without reading any store.
#The light columns are small enough to take as a whole (a few tens of bytes per store, so
#a five thousand tile release is a couple of hundred kB), which is what lets the cone test
#be plain vectorised R rather than an expression arrow has to interpret. What is not read
#here is the keywords, which are over 99% of the file by volume; those are fetched per
#surviving store by key, and it is that fetch which the sorted row groups prune.
#
#The generous bounds are deliberate. Each store's reference position is only one point in
#it, so a store is a candidate when its reference position is within its own half diagonal
#plus the box half diagonal plus a margin of the requested position. Anything tighter would
#risk dropping a store the precise test would have kept, which is the one mistake this
#stage must not make.
Rfits_zarr_index_query = function(index, RA = NULL, Dec = NULL, loc = NULL,
                                  box = 101, box.unit = c('pix', 'arcsec'),
                                  bucket = NULL, prefix = NULL,
                                  region = NULL, endpoint = NULL,
                                  access_key = NULL, secret_key = NULL,
                                  session_token = NULL,
                                  buffer = 0, safety = 0.1,
                                  keywords = FALSE, data.table = TRUE,
                                  verbose = TRUE){
  assertString(index, null.ok = TRUE)
  assertFlag(verbose)
  assertFlag(keywords)
  assertFlag(data.table)
  assertNumeric(buffer, len = 1, lower = 0)
  assertNumeric(safety, len = 1, lower = 0)
  box.unit = match.arg(box.unit)
  assertNumeric(box, min.len = 1, max.len = 2)

  if(is.null(index)){
    stop('index is required!', call. = FALSE)
  }

  pos = .zarr_cutout_positions(RA = RA, Dec = Dec, loc = loc)
  #A single number is the same box in both axes, which is what the search does; indexing
  #box[2] without this gives NA and every bound below silently follows it
  if(length(box) == 1){
    box = c(box, box)
  }

  src = .zarr_index_open(index, bucket = bucket, prefix = prefix, region = region,
                         endpoint = endpoint, access_key = access_key,
                         secret_key = secret_key, session_token = session_token)
  if(is.null(src$path)){
    .zarr_index_close(src)
    stop('No Zarr index at ', src$shown, call. = FALSE)
  }
  on.exit(.zarr_index_close(src))

  rows = .zarr_index_light_rows(src$path)
  rows = rows[rows$status == 'ok', ]
  n_all = nrow(rows)
  if(n_all == 0){
    out = rows[0, ]
    if(!data.table){
      out = as.data.frame(out, stringsAsFactors = FALSE)
    }
    if(verbose){
      message('No index row holds usable WCS metadata')
    }
    return(invisible(out))
  }

  #The radius is per store and depends only on the store, so it is worked out in R rather
  #than pushed into an expression: the test is then a separation, which needs trigonometry
  #arrow does not have. It is the search's own .zarr_cutout_max_sep, called per store, so
  #that the two cannot disagree about what counts as a candidate. That includes the offset
  #from the reference point to the tile centre, which a hand written version of this
  #formula left out and which would let a store with an off centre CRPIX be rejected here
  #even though the search would have taken it.
  scale = Map(list, x = rows$pixscale_x, y = rows$pixscale_y)
  limit = vapply(seq_len(nrow(rows)), function(i){
    this_box = tryCatch(.zarr_cutout_box_pix(box, box.unit, scale[[i]]),
                        error = function(e) NULL)
    if(is.null(this_box)){
      #A box this store cannot express is kept rather than dropped: the search decides
      #that, and reports it as its own status rather than silently matching nothing here
      this_box = suppressWarnings(ceiling(if(box.unit == 'arcsec'){
        box/scale[[i]]
      }else{
        box
      }))
    }
    test_box = .zarr_cutout_test_box(this_box, buffer = buffer, scale = scale[[i]])
    naxis = c(rows$naxis1[i], rows$naxis2[i])
    crpix = c(rows$crpix1[i], rows$crpix2[i])
    .zarr_cutout_max_sep(naxis, crpix, scale[[i]], test_box, safety = safety)
  }, numeric(1))
  #A store whose bound could not be worked out is kept, since this stage may be generous
  #but must never reject what the precise test would have taken
  limit[is.finite(rows$ra) & is.na(limit)] = Inf

  #The degrees box is only a pre-filter to keep the trigonometry off rows that cannot
  #match, so it is built from the widest bound in the file rather than per row. The RA
  #half width divides by cos(Dec), which grows without bound near the poles; a store is
  #never further than a full turn away, so the width is capped at the whole sky and the
  #test falls back to declination alone up there rather than keeping every row.
  keep = rep(FALSE, nrow(rows))
  widest = max(limit, na.rm = TRUE)
  for(k in seq_len(nrow(pos))){
    ddec = (widest + 3600) / 3600
    dra = min(360, (widest + 3600) / 3600 / max(abs(cos(pos[k, 2] * pi/180)), 1e-6))
    near = (abs(((rows$ra - pos[k, 1] + 540) %% 360) - 180) <= dra) &
             (abs(rows$dec - pos[k, 2]) <= ddec)
    near[is.na(near)] = TRUE
    if(any(near)){
      at = which(near)
      sep = .zarr_ang_sep_arcsec(pos[k, 1], pos[k, 2], rows$ra[at], rows$dec[at])
      keep[at[sep <= limit[at]]] = TRUE
    }
  }
  hit = rows[keep, ]

  if(isTRUE(keywords)){
    pay = .zarr_index_payload_rows(src$path, hit$cache_key)
    blobs = stats::setNames(as.list(pay$keyvalues), pay$cache_key)
    hit$keyvalues = blobs[hit$cache_key]
    #A store the payload query did not return has had its blob dropped since the light
    #pass; the empty vector is what both .zarr_index_tile and a caller testing with
    #length() understand as 'no keywords here'
    hit$keyvalues = lapply(hit$keyvalues, function(val){
      if(is.null(val)){
        raw(0)
      }else{
        .zarr_index_cell('keyvalues', val)
      }
    })
  }

  if(verbose){
    message('Index holds ', n_all, ' searchable store(s); ', nrow(hit),
            ' could overlap the requested position(s)')
  }
  if(!data.table){
    hit = as.data.frame(hit, stringsAsFactors = FALSE)
  }else{
    hit = data.table::as.data.table(hit)
  }
  return(invisible(hit))
}

#What an index holds, without reading its rows. This is the thing to run before deciding
#whether an index is current, since the answer comes from the columns only.
Rfits_zarr_index_info = function(index, bucket = NULL, prefix = NULL,
                                 region = NULL, endpoint = NULL,
                                 access_key = NULL, secret_key = NULL,
                                 session_token = NULL){
  assertString(index, null.ok = TRUE)
  if(is.null(index)){
    stop('index is required!', call. = FALSE)
  }
  src = .zarr_index_open(index, bucket = bucket, prefix = prefix, region = region,
                         endpoint = endpoint, access_key = access_key,
                         secret_key = secret_key, session_token = session_token)
  if(is.null(src$path)){
    .zarr_index_close(src)
    stop('No Zarr index at ', src$shown, call. = FALSE)
  }
  on.exit(.zarr_index_close(src))

  rows = .zarr_index_light_rows(src$path)
  walks = .zarr_index_walk_rows(src$path)
  meta = .zarr_index_meta_rows(src$path)
  ver = NA_integer_
  at = which(meta$cache_key == .zarr_index_meta_key('index_version'))
  if(length(at) > 0){
    ver = suppressWarnings(as.integer(meta$text_value[at[1]]))
  }
  status = rows$status
  counts = data.frame(status = character(0), n = integer(0), stringsAsFactors = FALSE)
  if(nrow(rows) > 0){
    counts = as.data.frame(table(status = status), stringsAsFactors = FALSE)
    names(counts) = c('status', 'n')
    counts = counts[order(-counts$n), ]
  }
  output = list(path = src$shown,
                version = ver,
                n_rows = nrow(rows),
                extnames = sort(unique(rows$extname)),
                stores = unique(rows$store),
                status = counts,
                n_walks = nrow(walks),
                created = if(nrow(rows) > 0 && any(is.finite(rows$created))){
                  as.POSIXct(max(rows$created, na.rm = TRUE), origin = '1970-01-01')
                }else{
                  as.POSIXct(NA_real_, origin = '1970-01-01')
                })
  class(output) = 'Rfits_zarr_index_info'
  return(invisible(output))
}

print.Rfits_zarr_index_info = function(x, ...){
  cat('Zarr index: ', x$path, '\n', sep = '')
  cat('  format version: ', if(is.na(x$version)) 'unknown' else x$version, '\n', sep = '')
  cat('  stores:         ', x$n_rows, ' row(s)',
      if(length(x$extnames) > 0) paste0(', extname(s): ',
                                        paste(x$extnames, collapse = ', ')) else '',
      '\n', sep = '')
  if(nrow(x$status) > 0){
    for(i in seq_len(nrow(x$status))){
      cat('    ', x$status$status[i], ': ', x$status$n[i], '\n', sep = '')
    }
  }
  cat('  store lists:    ', x$n_walks, '\n', sep = '')
  if(!is.null(x$created) && !is.na(x$created)){
    cat('  last written:   ', format(x$created), '\n', sep = '')
  }
  return(invisible(x))
}




