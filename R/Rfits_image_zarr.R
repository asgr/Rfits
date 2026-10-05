#Zarr backed FITS style image readers and writers.
#
#A Zarr "file" is a directory (a store), with each extension stored as an array
#node inside it. FITS style metadata is kept in the attributes of each array:
#
#  header      character vector of FITS 80 column card images (authoritative)
#  keyvalues   named list of keyword values (structured mirror)
#  keycomments named list of keyword comments (structured mirror)
#  comment     character vector of COMMENT lines
#  history     character vector of HISTORY lines
#
#The card images in "header" are what get read back and parsed, exactly as the
#HDF5 back-end does, so files stay readable by any tool that can open a Zarr
#store (the structured mirrors are there for convenience of non R consumers).

.zarr_require = function(){
  if(!requireNamespace("zarr", quietly = TRUE)){
    stop('The zarr package is needed for this to work. Please install from CRAN.', call. = FALSE)
  }
}

.zarr_store_new = function(filename){
  zarr::create_zarr(filename)
}

#A store object (zarr_s3store, zarr_localstore, ...) may be given wherever a
#path to a store is normally expected, which is the only way to reach stores
#that need credentials, since open_zarr() builds the store itself and cannot be
#passed one. A zarr object wrapping a store is accepted too, so that methods can
#hand back what they were given.
.zarr_is_store = function(filename){
  return(inherits(filename, c('zarr_store', 'zarr')))
}

#The store behind either a store object or an open zarr object
.zarr_store_of = function(filename){
  if(inherits(filename, 'zarr')){
    return(filename$store)
  }
  return(filename)
}

#A store has no local path, but the filename field of every output is expected to
#say where the data came from, so use the store URI where there is one. Stores
#with no URI (e.g. a memory store) fall back to a type name. Short circuit
#operators are required here, since some stores return NULL rather than a string
.zarr_store_label = function(filename){
  store = .zarr_store_of(filename)
  uri = tryCatch(store$uri, error = function(e) NULL)
  if(is.character(uri) && length(uri) == 1 && nzchar(uri)){
    return(uri)
  }
  name = tryCatch(store$friendlyClassName, error = function(e) NULL)
  if(is.character(name) && length(name) == 1 && nzchar(name)){
    return(name)
  }
  return('Zarr store')
}

#The metadata for a new array, completed the way Zarr itself would complete it.
#Zarr fills in chunk_key_encoding when it writes array metadata, but only the
#local and memory stores hand the completed version back to the caller, and the
#array object is built from that return value. Over S3 the field is therefore
#missing on the object that has just been written, and any attempt to work out
#its own chunk names fails with an opaque error. Supplying it up front avoids
#the asymmetry without changing what ends up in the store.
.zarr_array_metadata = function(builder, store){
  meta = builder$metadata()
  sep = meta$chunk_key_encoding$configuration$separator
  if(is.null(sep) || !(sep %in% c('.', '/'))){
    chunk_sep = tryCatch(store$.__enclos_env__$private$.chunk_sep, error = function(e) NULL)
    if(!is.character(chunk_sep) || length(chunk_sep) != 1 || !nzchar(chunk_sep)){
      chunk_sep = '.'
    }
    meta$chunk_key_encoding = list(name = 'default',
                                   configuration = list(separator = chunk_sep))
  }
  return(meta)
}

#Delete everything in a store object. Used for overwrite_file, since there is no
#directory to unlink
.zarr_store_clear = function(filename){
  store = .zarr_store_of(filename)
  if(!isTRUE(tryCatch(store$supports_deletes, error = function(e) FALSE))){
    stop('The Zarr store (', .zarr_store_label(filename), ') does not support deletes, ',
         'so overwrite_file = TRUE cannot be used!', call. = FALSE)
  }
  store$clear()
  return(invisible(NULL))
}

#Normalise a store prefix to the key prefix zarr_s3store uses. The constructor
#does exactly this, so the root group has to be written under the same name or
#the store is created in one place and looked for in another.
.zarr_s3_prefix = function(prefix){
  if(nzchar(prefix)){
    return(sub('/*$', '/', prefix))
  }
  return('')
}

#The root group document of a Zarr v3 store. This is what makes a prefix a store
#at all; zarr_s3store$new() looks for it and refuses to return without one. The
#bytes are written out rather than serialised, because jsonlite turns the empty
#attribute list into [] where the v3 spec wants {}.
.zarr_root_group_bytes = function(){
  return(charToRaw('{"zarr_format":3,"node_type":"group","attributes":{}}'))
}

#The same test zarr_s3store uses privately to tell a missing key from a real
#error. Anything else (notably AccessDenied) is kept apart from absent, since a
#store may well exist behind credentials that cannot see it. The pattern looks for
#the service error codes, so it is deliberately narrow; anything it does not
#recognise is treated as unknown, which fails towards not overwriting.
.zarr_s3_not_found = function(message){
  return(grepl('NoSuchKey|NotFound|404|NoSuchBucket', message, ignore.case = TRUE))
}

.zarr_s3_probe = function(client, bucket, key){
  found = tryCatch({client$head_object(Bucket = bucket, Key = key); TRUE},
                   error = function(e) e)
  if(isTRUE(found)){
    return(list(exists = TRUE, message = NULL))
  }
  if(.zarr_s3_not_found(conditionMessage(found))){
    return(list(exists = FALSE, message = NULL))
  }
  return(list(exists = NA, message = conditionMessage(found)))
}

#The client config for a new store, built the same way zarr_s3store builds its
#own so that the client which writes the root group and the client which later
#opens the store agree. Kept separate so the parts that are easy to get wrong
#(path style addressing, the absent region, the NULL session token) can be
#checked without a network
.zarr_s3_config = function(region=NULL, endpoint=NULL, access_key=NULL, secret_key=NULL,
                           session_token=NULL){
  if(is.null(session_token)){
    #paws mishandles a NULL token at the time of writing, which is what an empty
    #string means here
    session_token = ''
  }
  cfg = list(credentials = list(creds = list(access_key_id = access_key,
                                            secret_access_key = secret_key,
                                            session_token = session_token)))
  if(!is.null(region)){
    cfg$region = region
  }
  if(!is.null(endpoint)){
    cfg$endpoint = endpoint
    #zarr_s3store assumes path style addressing whenever an endpoint is given, so
    #match it here or the probe and the write would go to different URLs
    cfg$s3_force_path_style = TRUE
  }
  return(cfg)
}

#Bootstrap a store over S3 by writing its root group. Kept separate from
#Rfits_s3_store_new() so that the logic which does not need a network (prefix
#normalising, the overwrite guard, the bytes themselves) can be tested directly.
.zarr_s3_root_create = function(client, bucket, prefix, force = FALSE){
  assertFlag(force)
  root = .zarr_s3_prefix(prefix)
  key = paste0(root, 'zarr.json')
  if(!isTRUE(force)){
    probe = .zarr_s3_probe(client, bucket, key)
    if(isTRUE(probe$exists)){
      stop('A Zarr store already exists at s3://', bucket, '/', root,
           '. Use force = TRUE to overwrite its root group.', call. = FALSE)
    }
    if(is.na(probe$exists)){
      stop('Cannot determine whether a Zarr store already exists at s3://', bucket, '/', root,
           ': ', probe$message, call. = FALSE)
    }
  }
  written = tryCatch(client$put_object(Bucket = bucket, Key = key,
                                       Body = .zarr_root_group_bytes()),
                     error = function(e) e)
  if(inherits(written, 'error')){
    stop('Cannot create the Zarr root group at s3://', bucket, '/', root, ': ',
         conditionMessage(written), call. = FALSE)
  }
  return(invisible(root))
}

#Create a brand new Zarr store over S3 (or any S3 compatible service, which
#includes Cloudflare R2). The zarr package can only open a store there, never
#make one: zarr_s3store$new() looks for the root group as its last act and
#errors if there is none, and create_zarr() is hard wired to local and memory
#stores. So the root group is written first with the same client that will be
#used to read it back, and only then is the normal constructor called.
Rfits_s3_store_new = function(bucket, prefix='', region=NULL, endpoint=NULL,
                              access_key=NULL, secret_key=NULL, session_token=NULL,
                              force=FALSE){
  .zarr_require()
  assertString(bucket, min.chars=1)
  assertString(prefix)
  assertString(region, null.ok=TRUE)
  assertString(endpoint, null.ok=TRUE)
  assertString(access_key, null.ok=TRUE)
  assertString(secret_key, null.ok=TRUE)
  assertString(session_token, null.ok=TRUE)
  assertFlag(force)

  if(is.null(access_key) || is.null(secret_key)){
    #Without keys zarr_s3store falls back to anonymous credentials, which can
    #read a public bucket but cannot write, so the store would be created in one
    #request and then be unopenable in the next. Better to say which argument is
    #missing than to let a 400 on a key name report it
    stop('access_key and secret_key are required. Without them zarr_s3store ',
         'silently falls back to anonymous credentials, so the new store could be ',
         'created but never opened.', call. = FALSE)
  }
  if(!requireNamespace('paws.storage', quietly=TRUE)){
    stop('The paws.storage package is needed to create a Zarr store over S3. ',
         'Please install it from CRAN.', call. = FALSE)
  }

  cfg = .zarr_s3_config(region = region, endpoint = endpoint, access_key = access_key,
                        secret_key = secret_key, session_token = session_token)
  client = paws.storage::s3(config = cfg)

  root = .zarr_s3_root_create(client, bucket, prefix, force=force)
  message('Created Zarr store at s3://', bucket, '/', root)
  #The store must build its own client, since zarr_s3store offers no way to be
  #handed one, which is why the credentials go twice. The same normalised values
  #are used for both so neither side can end up anonymous
  store = zarr::zarr_s3store$new(bucket = bucket, prefix = prefix, region = region,
                                 endpoint = endpoint,
                                 access_key = cfg$credentials$creds$access_key_id,
                                 secret_key = cfg$credentials$creds$secret_access_key,
                                 session_token = cfg$credentials$creds$session_token)
  return(invisible(store))
}

#Open an S3 store for writing, creating it first if the prefix has no root group
#yet. The probe decides rather than catching an error from zarr_s3store$new, since
#the constructor reports an absent prefix, a bad key and a wrong region in three
#different ways, and only the first of those may be created over. An existing
#prefix is opened rather than created, because Rfits_s3_store_new refuses a prefix
#that already holds a store; that is the right behaviour on its own but not here,
#where re-running a batch is expected to update the stores it made the first time.
.zarr_s3_store_for = function(bucket, prefix='', region=NULL, endpoint=NULL,
                              access_key=NULL, secret_key=NULL, session_token=NULL,
                              client=NULL){
  .zarr_require()
  if(is.null(session_token)){
    session_token = ''
  }
  if(is.null(client)){
    if(!requireNamespace('paws.storage', quietly=TRUE)){
      stop('The paws.storage package is needed to reach a Zarr store over S3. ',
           'Please install it from CRAN.', call. = FALSE)
    }
    client = paws.storage::s3(config = .zarr_s3_config(region = region, endpoint = endpoint,
                                                       access_key = access_key,
                                                       secret_key = secret_key,
                                                       session_token = session_token))
  }
  key = paste0(.zarr_s3_prefix(prefix), 'zarr.json')
  probe = .zarr_s3_probe(client, bucket, key)
  if(is.na(probe$exists)){
    stop('Cannot determine whether a Zarr store exists at s3://', bucket, '/',
         .zarr_s3_prefix(prefix), ': ', probe$message, call. = FALSE)
  }
  if(!isTRUE(probe$exists)){
    #Writes the root group and then opens the store with the same credentials, so
    #none of that is duplicated here. Returns the open store.
    return(Rfits_s3_store_new(bucket = bucket, prefix = prefix, region = region,
                              endpoint = endpoint, access_key = access_key,
                              secret_key = secret_key, session_token = session_token))
  }
  return(zarr::zarr_s3store$new(bucket = bucket, prefix = prefix, region = region,
                                endpoint = endpoint, access_key = access_key,
                                secret_key = secret_key, session_token = session_token,
                                read_only = FALSE))
}

#Does the store have a root group? Tri state: NA means it could not be
#established, which for a remote store usually means credentials or a prefix
#mistake. Deliberately kept apart from FALSE, since only a genuine absence may
#be bootstrapped over.
.zarr_store_has_root = function(store){
  root3 = tryCatch(store$exists('zarr.json'), error = function(e) NA)
  if(isTRUE(root3)){
    return(TRUE)
  }
  root2 = tryCatch(store$exists('.zgroup'), error = function(e) NA)
  if(isTRUE(root2)){
    return(TRUE)
  }
  if(anyNA(root3) && anyNA(root2)){
    return(NA)
  }
  return(FALSE)
}

#The root group is what a Zarr store is, so its absence is reported the same way
#for both back ends. Only a genuine absence may be bootstrapped by a writer,
#hence the separate NA branch rather than treating unknown as empty.
.zarr_store_root_error = function(store, opened){
  has_root = .zarr_store_has_root(store)
  if(is.na(has_root)){
    stop('Cannot determine whether the Zarr store (', .zarr_store_label(store), ') ',
         'exists, so it cannot be opened: ', conditionMessage(opened), call. = FALSE)
  }
  if(!has_root){
    stop('Zarr store has no root group, so there is nothing to read: ',
         .zarr_store_label(store), call. = FALSE)
  }
  stop('Cannot open the Zarr store (', .zarr_store_label(store), '): ',
       conditionMessage(opened), call. = FALSE)
}

.zarr_store_open_object = function(filename, write=FALSE){
  .zarr_require()
  store = .zarr_store_of(filename)
  read_only = tryCatch(store$read_only, error = function(e) FALSE)
  if(isTRUE(write) & isTRUE(read_only)){
    stop('The Zarr store (', .zarr_store_label(filename), ') is read only, so it cannot be ',
         'written to. Create it with read_only = FALSE.', call. = FALSE)
  }
  #Already open, e.g. a store handed back by a previous read
  if(inherits(filename, 'zarr')){
    return(filename)
  }
  #Opening a store with no root group produces a baffling error from Zarr. Try it
  #first so the common case costs nothing extra, and only go looking for the root
  #group once we know something is wrong.
  opened = tryCatch(zarr::zarr$new(store), error = function(e) e)
  if(!inherits(opened, 'error')){
    return(opened)
  }
  if(!isTRUE(write)){
    .zarr_store_root_error(store, opened)
  }
  #A new store, e.g. one that has just been created, is bootstrapped here rather
  #than by create_zarr(), which cannot be given a store object at all.
  if(isFALSE(.zarr_store_has_root(store))){
    store$create_group(name='')
    return(zarr::zarr$new(store))
  }
  .zarr_store_root_error(store, opened)
}

.zarr_store_open = function(filename, write=FALSE){
  if(.zarr_is_store(filename)){
    return(.zarr_store_open_object(filename, write=write))
  }
  if(!dir.exists(filename)){
    stop('Zarr store does not exist: ', filename, call. = FALSE)
  }
  zarr::open_zarr(filename, protocol='local', read_only=!write)
}

#Names of every array in the store, without the leading '/' (like hdf5r$names)
.zarr_extnames = function(store){
  paths = store$arrays
  if(is.null(paths) | length(paths) == 0){
    return(character(0))
  }
  return(sub('^/', '', paths))
}

.zarr_name_to_path = function(extname){
  return(paste0('/', sub('^/', '', extname)))
}

#Lists holding S3 classes (e.g. Rfits_keylist) cannot be serialised to JSON, and
#neither can exotic element types, so flatten everything down to bare values
.zarr_clean_list = function(x){
  x = unclass(x)
  x[] = lapply(x, function(val){
    if(is.null(val)){
      return(NA)
    }
    if(!is.null(dim(val))){
      val = as.vector(val)
    }
    if(is.list(val)){
      return(.zarr_clean_list(val))
    }
    if(!is.numeric(val) & !is.character(val) & !is.logical(val)){
      val = as.character(val)
    }
    return(val)
  })
  return(x)
}

#List attributes arriving from JSON have lost any NA values (they become NULL)
.zarr_json_to_list = function(x){
  if(is.null(x) | !is.list(x) | length(x) == 0){
    return(NULL)
  }
  is_null = vapply(x, is.null, logical(1))
  if(any(is_null)){
    x[is_null] = list(NA)
  }
  return(x)
}

#Parse a vector of FITS card images into the full set of header components
.zarr_header_to_meta = function(header, remove_HIERARCH=FALSE){
  loc_comment = grep('COMMENT', header)
  loc_history = grep('HISTORY', header)

  if(length(loc_comment) > 0){
    comment = gsub('COMMENT ', '', header[loc_comment])
  }else{
    comment = NULL
  }

  if(length(loc_history) > 0){
    history = gsub('HISTORY ', '', header[loc_history])
  }else{
    history = NULL
  }

  if(length(loc_comment) > 0 | length(loc_history) > 0){
    headertemp = header[-c(loc_comment, loc_history)]
  }else{
    headertemp = header
  }

  #hdr vector
  hdr = Rfits_header_to_hdr(headertemp, remove_HIERARCH=remove_HIERARCH)

  #keyword list
  keyvalues = Rfits_hdr_to_keyvalues(hdr)
  keynames = names(keyvalues)

  loc_HIERARCH = grep('HIERARCH', keynames)
  if(length(loc_HIERARCH) > 0){
    keynames_goodhead = keynames[-loc_HIERARCH]
    pattern_goodhead = paste(c(paste0(format(keynames_goodhead, width=8), '='), 'HIERARCH'), collapse = '|')
  }else{
    pattern_goodhead = paste(paste0(format(keynames, width=8), '='), collapse = '|')
  }
  loc_goodhead = grep(pattern_goodhead, headertemp)

  headertemp = headertemp[loc_goodhead]
  #comments list
  keycomments = lapply(strsplit(headertemp, '/ '), function(x) x[2])
  names(keycomments) = keynames

  return(list(header = header,
              hdr = hdr,
              keyvalues = keyvalues,
              keycomments = keycomments,
              keynames = keynames,
              comment = comment,
              history = history
  ))
}

#Pull the FITS metadata off a zarr node, from the card images if present, and
#otherwise rebuilt from the structured keyword attributes.
.zarr_get_meta = function(node, remove_HIERARCH=FALSE){
  header = node$attribute('header')
  if(is.character(header) & length(header) > 0){
    return(.zarr_header_to_meta(header, remove_HIERARCH=remove_HIERARCH))
  }

  keyvalues = .zarr_json_to_list(node$attribute('keyvalues'))
  if(is.null(keyvalues)){
    return(NULL)
  }
  keycomments = .zarr_json_to_list(node$attribute('keycomments'))
  if(is.null(keycomments) | length(keycomments) != length(keyvalues)){
    keycomments = as.list(rep('', length(keyvalues)))
    names(keycomments) = names(keyvalues)
  }
  comment = node$attribute('comment')
  history = node$attribute('history')
  header = Rfits_keyvalues_to_header(keyvalues, keycomments, comment, history)

  return(.zarr_header_to_meta(header, remove_HIERARCH=remove_HIERARCH))
}

Rfits_read_image_zarr = function(filename='temp.zarr', extname='data1', ext=NULL, header=TRUE,
                                 xlo=NULL, xhi=NULL, ylo=NULL, yhi=NULL, zlo=NULL, zhi=NULL,
                                 tlo=NULL, thi=NULL, remove_HIERARCH=FALSE, force_logical=FALSE,
                                 physical=TRUE, collapse=FALSE){
  .zarr_require()

  #A store object is neither a path nor a character, so it cannot be asserted or
  #expanded. It stays the thing we open, while the output records the store URI
  #in place of a file path.
  filename_source = filename
  if(.zarr_is_store(filename)){
    filename = .zarr_store_label(filename)
  }else{
    assertCharacter(filename, max.len=1)
    filename = path.expand(filename)
    filename_source = filename
  }
  assertCharacter(extname, max.len=1)
  assertFlag(header)
  assertIntegerish(xlo, null.ok=TRUE)
  assertIntegerish(xhi, null.ok=TRUE)
  assertIntegerish(ylo, null.ok=TRUE)
  assertIntegerish(yhi, null.ok=TRUE)
  assertIntegerish(zlo, null.ok=TRUE)
  assertIntegerish(zhi, null.ok=TRUE)
  assertIntegerish(tlo, null.ok=TRUE)
  assertIntegerish(thi, null.ok=TRUE)
  assertFlag(remove_HIERARCH)
  assertFlag(force_logical)
  assertFlag(physical)
  assertFlag(collapse)

  store = .zarr_store_open(filename_source)
  extnames = .zarr_extnames(store)

  output = NULL

  try({
    if(!is.null(ext)){
      assertIntegerish(ext, len=1)
      extname = extnames[ext]
      if(is.na(extname)){
        stop('Extension ', ext, ' does not exist in the Zarr store!')
      }
    }else{
      ext = which(extnames == extname)
      if(length(ext) == 0){
        #Allow nested lookup via the path form of the name
        extname = sub('^/', '', extname)
        ext = which(extnames == extname)
        if(length(ext) == 0){
          stop('Extension "', extname, '" does not exist in the Zarr store!')
        }
      }
      ext = ext[1]
      extname = extnames[ext]
    }

    node = store$get_node(.zarr_name_to_path(extname))
    if(is.null(node)){
      stop('Cannot find node for extension: ', extname)
    }

    dim = as.integer(node$shape)

    Ndim = length(dim)

    if(Ndim > 4){
      stop('Zarr arrays with more than 4 dimensions are not supported!')
    }

    naxis = as.integer(c(dim, rep(1L, 4 - Ndim))[1:4])

    subset = FALSE

    lo_req = list(xlo, ylo, zlo, tlo)
    hi_req = list(xhi, yhi, zhi, thi)

    safe = vector(mode='list', length=4)
    for(i in 1:4){
      lo = lo_req[[i]]
      hi = hi_req[[i]]
      if(is.null(lo)){lo = 1L}else{subset=TRUE}
      if(is.null(hi)){hi = naxis[i]}else{subset=TRUE}
      safe[[i]] = .safedim(1L, naxis[i], lo, hi)
    }

    if(subset){
      selection = vector(mode='list', length=Ndim)
      for(i in 1:Ndim){
        lo = as.integer(min(safe[[i]]$orig))
        hi = as.integer(max(safe[[i]]$orig))
        if(safe[[i]]$safe){
          assertIntegerish(lo, lower=1L, upper=naxis[i], len=1)
          assertIntegerish(hi, lower=1L, upper=naxis[i], len=1)
        }
        if(hi < lo){stop('The upper limit must be larger than the lower limit on dimension ', i)}
        selection[[i]] = c(lo, hi)
      }

      all_safe = all(vapply(safe, function(x) x$safe, logical(1)))
      temp_image = NULL
      if(all_safe){
        temp_image = node$read(selection)
      }

      len_tar = as.integer(vapply(safe, function(x) x$len_tar, numeric(1)))
      tar = lapply(safe, function(x) as.integer(x$tar))

      if(Ndim == 1){
        image = rep(NA, len_tar[1])
        if(all_safe){
          image[tar[[1]]] = temp_image
        }
      }
      if(Ndim == 2){
        image = array(NA, c(len_tar[1], len_tar[2]))
        if(all_safe){
          image[tar[[1]], tar[[2]]] = temp_image
        }
      }
      if(Ndim == 3){
        image = array(NA, c(len_tar[1], len_tar[2], len_tar[3]))
        if(all_safe){
          image[tar[[1]], tar[[2]], tar[[3]]] = temp_image
        }
      }
      if(Ndim == 4){
        image = array(NA, c(len_tar[1], len_tar[2], len_tar[3], len_tar[4]))
        if(all_safe){
          image[tar[[1]], tar[[2]], tar[[3]], tar[[4]]] = temp_image
        }
      }
    }else{
      image = node$read(NULL)
      if(Ndim > 1){
        dim(image) = dim
      }
    }

    #What Zarr hands back is the stored integers, and a quantised array is only
    #meaningful once the FITS scaling has been undone. This therefore happens before
    #the logical conversion, since scaling turns the image into doubles and there
    #would be nothing left to recover as flags afterwards. An array with no scale
    #keys is left exactly as it is, which is every store written before lossy
    #support. Scaling promotes an integer image to double, and NA is propagated
    #rather than scaled, so the fill values stay missing.
    #
    #Only an integer array can be a quantised one, so a float read (the common case)
    #does not pay for reading and parsing the header cards at all.
    #
    #force_logical asks for the stored integers as flags, which is a different thing
    #from the physical values, so the two cannot both be honoured. force_logical wins
    #and says so, rather than returning logicals under a name implying the scaling
    #had been undone.
    scale = if(is.integer(image)) .zarr_read_scale(node) else NULL
    if(!is.null(scale)){
      if(force_logical){
        message('This Zarr array carries BSCALE and BZERO, but force_logical = TRUE was ',
                'asked for, so the stored integers are returned as logical values ',
                'without scaling.')
      }else if(physical){
        image = scale$bzero + scale$bscale * image
      }
    }

    if(force_logical & is.integer(image)){
      #as.logical() on a matrix drops the dimensions, so put them back
      image_dim = dim(image)
      image = as.logical(image)
      if(!is.null(image_dim)){
        dim(image) = image_dim
      }
    }

    if(header){
      meta = .zarr_get_meta(node, remove_HIERARCH=remove_HIERARCH)

      if(is.null(meta)){
        #A store produced by a plain image-to-zarr converter carries the shared
        #root key names but no FITS metadata at all, which is worth separating
        #from a genuinely unnamed Rfits extension
        if(.zarr_is_foreign_store(store)){
          message('This Zarr store was not written by Rfits (it advertises the images to Zarr ',
                  'root attributes but no convention marker). FITS headers are not stored by ',
                  'such converters, so the array is returned without any metadata.')
        }else{
          warning('No FITS style metadata found for extension: ', extname)
        }
        output = image
      }else{
        header = meta$header
        hdr = meta$hdr
        keyvalues = meta$keyvalues
        keycomments = meta$keycomments
        keynames = meta$keynames
        comment = meta$comment
        history = meta$history

        if(subset){
          naxis_key = ifelse(isTRUE(keyvalues$ZIMAGE), 'ZNAXIS', 'NAXIS')
          #Align comments to values, so that new keys can be safely annotated
          keycomments_aligned = as.list(rep('', length(keyvalues)))
          names(keycomments_aligned) = names(keyvalues)
          matched = match(names(keycomments_aligned), names(keycomments), nomatch=0)
          keycomments_aligned[matched > 0] = keycomments[matched[matched > 0]]
          keycomments = keycomments_aligned

          for(i in 1:Ndim){
            keyn = paste0(naxis_key, i)
            keyc = paste0('CRPIX', i)
            keyvalues[[keyn]] = as.integer(safe[[i]]$len_tar)
            keycomments[[keyn]] = paste(keycomments[[keyn]], 'SUBMOD')
            if(!is.null(keyvalues[[keyc]])){
              keyvalues[[keyc]] = keyvalues[[keyc]] - as.integer(safe[[i]]$lo_tar) + 1L
              keycomments[[keyc]] = paste(keycomments[[keyc]], 'SUBMOD')
            }
          }
          #Rebuild every header form from the updated keyvalues, which keeps the
          #card formatting identical to the FITS and HDF5 back-ends
          hdr = Rfits_keyvalues_to_hdr(keyvalues)
          header = Rfits_keyvalues_to_header(keyvalues, keycomments, comment, history)
          keynames = names(keyvalues)
        }

        #Unlike the HDF5 back-end, also carry the fixed width and WCS reference
        #forms, so that the Rfits_image methods which expect them work unchanged
        raw = Rfits_header_to_raw(header)

        WCSfound = grep('CRVAL1', keynames, value=TRUE)
        if(length(WCSfound) == 0){
          WCSref = 'None'
        }else if(length(WCSfound) == 1){
          WCSref = 'NULL'
        }else{
          WCSref = {}
          for(i in 1:length(WCSfound)){
            WCScurrent = strsplit(WCSfound[i], 'CRVAL1', fixed=TRUE)[[1]]
            if(length(WCScurrent) == 1){
              WCSref = c(WCSref, 'Null')
            }else{
              WCSref = c(WCSref, WCScurrent[2])
            }
          }
        }

        output = list(imDat = image,
                      header = header,
                      hdr = hdr,
                      raw = raw,
                      keyvalues = keyvalues,
                      keycomments = keycomments,
                      keynames = keynames,
                      comment = comment,
                      history = history,
                      filename = filename,
                      ext = ext,
                      extname = extname,
                      WCSref = WCSref
        )

        if(Ndim == 1){class(output) = c('Rfits_vector', 'list')}
        if(Ndim == 2){class(output) = c('Rfits_image', 'list')}
        if(Ndim == 3){class(output) = c('Rfits_cube', 'list')}
        if(Ndim == 4){class(output) = c('Rfits_array', 'list')}
      }
    }else{
      output = image
    }
  })

  if(collapse & inherits(output, c('Rfits_cube', 'Rfits_array'))){
    if(length(dim(output)) == 3){
      if(dim(output)[3] == 1L){
        output = output[,,1, collapse=TRUE]
      }
    }
    if(length(dim(output)) == 4){
      if(dim(output)[3] == 1L & dim(output)[4] == 1L){
        output = output[,,1,1, collapse=TRUE]
      }else if(dim(output)[4] == 1L){
        output = output[,,,1, collapse=TRUE]
      }
    }
  }

  if(!is.null(output)){
    return(invisible(output))
  }
}

Rfits_read_vector_zarr = Rfits_read_image_zarr
Rfits_read_cube_zarr = Rfits_read_image_zarr
Rfits_read_array_zarr = Rfits_read_image_zarr

#A Zarr array carries no FITS node structure of its own, so the store is made
#self-describing in two places: a set of root group attributes summarising every
#array, and a small mirror of the technical parameters on each array itself. The
#names follow the wider images-to-Zarr convention where the meaning transfers, so
#that a foreign reader can at least find the arrays, but every value recorded is
#read back off the array that was written rather than echoed from the request.

#A fill or missing value cannot be stored as NA at the top level of an attribute,
#since jsonlite drops it and the key silently disappears. Missing text is
#therefore written as an empty string, and absent keys mean unavailable.
.zarr_root_keys = c('convention', 'schema_version', 'Rfits_version', 'created',
                    'total_images', 'images_array', 'images_shape', 'image_shape',
                    'chunk_shape', 'data_type', 'codec', 'compressor',
                    'compression_level', 'shuffle', 'fill_value', 'lossy', 'fits_header',
                    'key_count', 'supported_extensions', 'creation_info',
                    'append_history')

#The per array mirror of the technical parameters, and the root level summary that
#stands for the store as a whole. The two lists stay separate because the root adds
#the store level keys that no array carries. 'lossy' says whether the pixels are a
#scaled integer representation rather than the values themselves; it is recorded
#separately from data_type because the scale lives in the FITS cards, which a Zarr
#attribute cannot hold accurately.
.zarr_array_keys = c('data_type', 'chunk_shape', 'codec', 'compressor',
                     'compression_level', 'shuffle', 'fill_value', 'lossy')

#The blosc cname values accepted by zarr (0.5.0). Note that 'blosc' is not one of
#them, so the user facing default name maps to zarr's own default compressor.
#Throughout, 'codec' means the Zarr codec family (always 'blosc' for us) and
#'compressor' means the cname that codec carries, on arrays and at the root alike.
.zarr_cnames = c('blosclz', 'lz4', 'lz4hc', 'zstd', 'zlib')

.zarr_compressor_names = c('blosc', 'blosclz', 'lz4', 'lz4hc', 'zstd', 'zlib', 'gzip',
                           'bz2', 'lzma')

.zarr_shuffle_names = c('shuffle', 'noshuffle', 'bitshuffle')

#Logical shuffle values must never reach the codec: zarr only validates shuffle
#when it is character or integer, so TRUE/FALSE pass add_codec() unchanged and
#then fail deep inside write() with an opaque error. NULL means leave it to zarr,
#which picks a shuffle appropriate to the data type.
.zarr_shuffle = function(shuffle){
  if(is.null(shuffle)){
    return(NULL)
  }
  if(is.logical(shuffle)){
    if(length(shuffle) != 1 | is.na(shuffle)){
      stop('shuffle must be TRUE, FALSE, NULL, or one of: ',
           paste(.zarr_shuffle_names, collapse = ', '), call. = FALSE)
    }
    return(if(shuffle) 'shuffle' else 'noshuffle')
  }
  assertCharacter(shuffle, len=1)
  if(!(shuffle %in% .zarr_shuffle_names)){
    stop('shuffle must be TRUE, FALSE, NULL, or one of: ',
         paste(.zarr_shuffle_names, collapse = ', '), call. = FALSE)
  }
  return(shuffle)
}

#Resolve a requested compressor into a blosc configuration. Names that blosc
#cannot produce fall back with a warning rather than being recorded under a name
#that does not match the bytes on disk.
.zarr_compressor = function(compressor = 'blosc', clevel = 6L, shuffle = NULL){
  assertCharacter(compressor, len=1)
  assertIntegerish(clevel, len=1, lower=0, upper=9)

  compressor = tolower(compressor)[1]
  shuffle = .zarr_shuffle(shuffle)

  cname = switch(compressor,
                 #zarr's own blosc default, and what a plain blosc request means
                 blosc = 'zstd',
                 blosclz = 'blosclz',
                 lz4 = 'lz4',
                 lz4hc = 'lz4hc',
                 zstd = 'zstd',
                 #The standalone gzip codec needs the zlib package, which is not a
                 #dependency of either Rfits or zarr, so blosc carrying zlib is the
                 #only honest way to honour a gzip request.
                 zlib = 'zlib',
                 gzip = 'zlib',
                 NULL)

  if(is.null(cname)){
    if(compressor %in% c('bz2', 'lzma')){
      warning('Compressor "', compressor, '" cannot be produced by the Zarr blosc codec, ',
              'falling back to "zstd"!', call. = FALSE)
    }else{
      warning('Compressor "', compressor, '" is not recognised, falling back to "zstd". ',
              'Supported names are: ', paste(.zarr_compressor_names, collapse = ', '), '!',
              call. = FALSE)
    }
    cname = 'zstd'
  }

  config = list(cname = cname, clevel = as.integer(clevel))
  if(!is.null(shuffle)){
    config$shuffle = shuffle
  }
  return(config)
}

#The codec actually applied to an array, read back from its metadata. Returns
#NULL when the array carries no blosc codec, which is how a foreign array looks.
.zarr_codec_of = function(node){
  codecs = tryCatch(node$metadata$codecs, error = function(e) NULL)
  if(is.null(codecs) | length(codecs) == 0){
    return(NULL)
  }
  #Completed metadata holds plain lists; a freshly built array holds codec objects
  names_of = vapply(codecs, function(x){
    if(is.list(x)){
      return(as.character(x$name))
    }
    return(as.character(tryCatch(x$name, error = function(e) '')))
  }, character(1))
  loc = which(names_of == 'blosc')
  if(length(loc) == 0){
    return(NULL)
  }
  conf = codecs[[loc[1]]]
  if(is.list(conf)){
    conf = conf$configuration
  }else{
    conf = tryCatch(conf$configuration, error = function(e) NULL)
  }
  if(is.null(conf)){
    return(NULL)
  }
  return(list(name = 'blosc',
              cname = as.character(conf$cname),
              clevel = as.integer(conf$clevel),
              shuffle = as.character(conf$shuffle)))
}

#The chunk shape of an array, which zarr keeps nested inside the chunk grid
.zarr_chunk_shape_of = function(node){
  cs = tryCatch(node$metadata$chunk_grid$configuration$chunk_shape, error = function(e) NULL)
  if(is.null(cs)){
    return(NULL)
  }
  return(as.vector(as.integer(cs)))
}

.zarr_fill_text = function(fill){
  if(is.null(fill)){
    return('')
  }
  #NaN and NA cannot survive a JSON round trip as themselves, so record the
  #marker as the string a consumer would have to compare against
  if(is.logical(fill)){
    return(ifelse(isTRUE(fill), 'true', 'false'))
  }
  #is.na() is TRUE for NaN, so the NaN test has to come first
  if(length(fill) == 1 && is.nan(fill)){
    return('NaN')
  }
  if(length(fill) == 1 && is.na(fill)){
    return('NA')
  }
  return(as.character(fill))
}

#Trim the trailing singleton dimensions that FITS itself would not have written
.zarr_trim_shape = function(shape){
  shape = as.vector(as.integer(shape))
  if(length(shape) > 1){
    last = max(which(shape != 1L))
    shape = shape[1:last]
  }
  return(shape)
}

#Counts stay integers while they fit, because that is what they are, but a large
#archive overflows easily (4096 x 4096 x 1000 is 1.7e10) and as.integer() would
#turn that into NA. Accumulate in double and narrow only when it is safe.
.zarr_count = function(x){
  if(length(x) == 1 && !is.na(x) && abs(x) <= .Machine$integer.max){
    return(as.integer(round(x)))
  }
  return(as.numeric(x))
}

#Always a double, so that a running total cannot overflow on the way. Two counts
#that each fit in the integer range can still sum past it, so narrowing happens
#only at the boundary, in .zarr_count().
.zarr_element_count = function(shape){
  prod(as.numeric(as.vector(as.integer(shape))))
}

#Counts may exceed the integer range, so they cannot be printed with %i, which
#errors on a double that large
.zarr_count_text = function(x){
  if(is.na(x)){
    return('NA')
  }
  sprintf('%.0f', x)
}

#The technical parameters of one array, as they are actually stored. zarr keeps
#the dtype and fill value in the completed metadata rather than on readable
#bindings (node$data_type is an R6 object that cannot be coerced to character),
#so metadata is the only trustworthy source for them.
#
#lossy_asked is what the writer just did to the pixels, and is passed in rather
#than guessed from the header, because a header copied from a scaled FITS file
#describes the file the data came from and not this array. NULL means the array was
#not written by Rfits just now, so the only evidence left is the existing attribute,
#and failing that the FITS convention.
.zarr_array_tech = function(node, lossy_asked = NULL){
  shape = as.vector(as.integer(node$shape))
  codec = .zarr_codec_of(node)
  meta = tryCatch(node$metadata, error = function(e) NULL)
  data_type = if(is.null(meta)) '' else as.character(meta$data_type)
  if(is.null(lossy_asked)){
    #Agrees with .zarr_read_scale(), so what a store reports about itself is what
    #reading it will actually do
    stored = tryCatch(node$attribute('lossy'), error = function(e) NULL)
    if(is.logical(stored) && length(stored) == 1 && !is.na(stored)){
      lossy = stored
    }else{
      lossy = !is.null(.zarr_read_scale(node))
    }
  }else{
    lossy = lossy_asked
  }
  return(list(
    shape = shape,
    image_shape = .zarr_trim_shape(shape),
    chunk_shape = .zarr_chunk_shape_of(node),
    data_type = data_type,
    codec = if(is.null(codec)) '' else codec$name,
    compressor = if(is.null(codec)) '' else codec$cname,
    clevel = if(is.null(codec)) NA_integer_ else codec$clevel,
    shuffle = if(is.null(codec)) '' else codec$shuffle,
    fill_value = .zarr_fill_text(if(is.null(meta)) NULL else meta$fill_value),
    lossy = lossy,
    fits_header = is.character(tryCatch(node$attribute('header'), error = function(e) NULL)),
    key_count = length(tryCatch(node$attribute('keyvalues'), error = function(e) NULL))
  ))
}

#Write the per-array technical mirror. These sit alongside header/keyvalues, and
#use the names in .zarr_array_keys so that they cannot collide with the FITS
#metadata that .zarr_get_meta() reads.
.zarr_array_tech_set = function(node, tech){
  vals = list(data_type = tech$data_type,
              chunk_shape = tech$chunk_shape,
              codec = tech$codec,
              compressor = tech$compressor,
              compression_level = tech$clevel,
              shuffle = tech$shuffle,
              fill_value = tech$fill_value,
              lossy = tech$lossy)
  for(key in .zarr_array_keys){
    node$set_attribute(key, vals[[key]])
  }
  return(invisible(NULL))
}

#Every root attribute that is not append history, rebuilt from the arrays that
#currently exist. Rebuilt rather than merged, because set_attribute() keeps keys
#that were written for an array which no longer exists.
.zarr_root_values = function(store, created = NULL, append_history = NULL){
  extnames = .zarr_extnames(store)

  images_array = as.vector(extnames, 'character')
  shapes = list()
  image_shapes = list()
  chunk_shapes = list()
  data_types = list()
  codecs = list()
  compressors = list()
  levels = list()
  shuffles = list()
  fills = list()
  lossy = list()
  fits_flags = list()
  key_counts = list()

  #Double, so that summing integer-sized counts cannot overflow
  total = 0
  for(name in images_array){
    node = tryCatch(store$get_node(.zarr_name_to_path(name)), error = function(e) NULL)
    if(is.null(node)){
      next
    }
    tech = .zarr_array_tech(node)
    shapes[[name]] = tech$shape
    image_shapes[[name]] = tech$image_shape
    chunk_shapes[[name]] = tech$chunk_shape
    data_types[[name]] = tech$data_type
    codecs[[name]] = tech$codec
    #The cname is the informative part, and is what a foreign reader expects to
    #find under compressor. The codec family is recorded separately.
    compressors[[name]] = tech$compressor
    levels[[name]] = tech$clevel
    shuffles[[name]] = tech$shuffle
    fills[[name]] = tech$fill_value
    lossy[[name]] = tech$lossy
    fits_flags[[name]] = tech$fits_header
    key_counts[[name]] = tech$key_count
    total = total + .zarr_element_count(tech$shape)
  }

  attrs = list(
    convention = 'Rfits.zarr',
    schema_version = 1L,
    Rfits_version = tryCatch(as.character(utils::packageVersion('Rfits')),
                             error = function(e) ''),
    created = if(is.null(created)) format(Sys.time(), '%Y-%m-%dT%H:%M:%SZ', tz = 'UTC') else created,
    #Not a count of images: we are not batched, so this is the total number of
    #elements across all arrays. The per array truth is in images_shape.
    total_images = .zarr_count(total),
    images_array = if(length(images_array)) as.list(images_array) else NULL,
    images_shape = shapes,
    image_shape = image_shapes,
    chunk_shape = chunk_shapes,
    data_type = data_types,
    codec = codecs,
    compressor = compressors,
    compression_level = levels,
    shuffle = shuffles,
    fill_value = fills,
    lossy = lossy,
    fits_header = fits_flags,
    key_count = key_counts,
    supported_extensions = list('fits'),
    creation_info = list(rfits_backend = TRUE, batched_nchw = FALSE)
  )
  if(!is.null(append_history)){
    attrs$append_history = append_history
  }
  return(attrs)
}

#Refresh the store level description. The attribute set is rebuilt from the arrays
#that currently exist and written whole, because zarr only ever merges attributes
#and an entry for a deleted array would otherwise be described forever. Foreign
#attributes on the root are carried across untouched; only our own keys are
#replaced. Written through the store rather than root$save(), because on a memory
#store the root prefix is the empty string while its metadata lives under the key
#root, and save() therefore writes a second phantom entry that makes the store
#impossible to reopen.
.zarr_root_refresh = function(store, append_entry = NULL){
  meta = tryCatch(store$root$metadata, error = function(e) NULL)
  if(is.null(meta)){
    return(invisible(NULL))
  }
  existing = tryCatch(store$root$attributes, error = function(e) NULL)
  if(!is.list(existing)){
    existing = list()
  }

  created = existing$created
  if(!(is.character(created) && length(created) == 1 && nzchar(created))){
    created = NULL
  }

  history = existing$append_history
  if(is.list(history) & length(history) > 0){
    history = .zarr_json_to_list(history)
  }else{
    history = NULL
  }
  if(!is.null(append_entry)){
    history = c(history, list(append_entry))
  }

  attrs = .zarr_root_values(store, created = created, append_history = history)

  keep = existing[!(names(existing) %in% .zarr_root_keys)]
  new = list()
  for(key in names(attrs)){
    val = attrs[[key]]
    #NULL and empty entries cannot be represented in JSON at all, so they are left
    #out rather than written as something a reader would have to guess about
    if(is.null(val)){
      next
    }
    if(is.list(val) && length(val) == 0){
      next
    }
    new[[key]] = .zarr_clean_list(list(val))[[1]]
  }

  meta$attributes = c(keep, new)
  written = tryCatch({store$store$set_metadata('/', meta); TRUE}, error = function(e) FALSE)
  if(!written){
    #Fall back to the node API, which works on every store except a memory one
    tryCatch({
      for(key in names(new)){
        store$root$set_attribute(key, new[[key]])
      }
      store$root$save()
    }, error = function(e) NULL)
  }
  return(invisible(attrs))
}

#Read the store level description back. Absent is normal for stores written
#before this existed, and returns NULL rather than erroring.
.zarr_root_attrs = function(store){
  attrs = tryCatch(store$root$attributes, error = function(e) NULL)
  if(!is.list(attrs) | length(attrs) == 0){
    return(NULL)
  }
  return(lapply(attrs, .zarr_json_to_list_final))
}

.zarr_json_to_list_final = function(x){
  if(is.list(x)){
    return(.zarr_json_to_list(x))
  }
  return(x)
}

#A foreign store written by a plain image-to-zarr converter has the shared key
#names but no FITS metadata and no convention marker. Worth saying so, since
#otherwise the only message is the generic one about missing metadata.
.zarr_is_foreign_store = function(store){
  attrs = .zarr_root_attrs(store)
  if(is.null(attrs)){
    return(FALSE)
  }
  if(!is.null(attrs$convention)){
    return(FALSE)
  }
  return(!is.null(attrs$total_images) | !is.null(attrs$images_array))
}

#Work out the zarr data type (and coerce the data) for an R object. Each type
#also needs a fill value, since zarr read() converts anything equal to the fill
#back into NA. The defaults are dangerous (float64 fills at 9.97e+36, which
#.near() then matches against any large magnitude), so always set our own.
.zarr_types = c('int8', 'int16', 'int32', 'uint8', 'uint16', 'float32', 'float64', 'bool')

.zarr_data_type = function(data, data_type=NULL){
  if(is.null(data_type)){
    if(is.logical(data)){
      #Zarr bool uses FALSE as its fill value, so NA becomes FALSE and is lost on
      #read. FITS stores logicals as bytes anyway, so mirror that and stay NA safe.
      data_type = 'int8'
    }else if(is.integer(data)){
      data_type = 'int32'
    }else if(bit64::is.integer64(data)){
      message('Converting integer64 data to float64; the Zarr package cannot write 64 bit integers from R!')
      data_type = 'float64'
    }else if(is.numeric(data)){
      data_type = 'float64'
    }else{
      stop('Data type not recognised, must be logical, integer or numeric!')
    }
  }
  data_type = match.arg(as.character(data_type), .zarr_types)

  if(data_type == 'bool'){
    if(anyNA(data)){
      stop('Cannot store NA in a bool Zarr array, use int8 (the default for logical data) or float64!',
           call. = FALSE)
    }
    fill = FALSE
  }else if(grepl('float', data_type)){
    #NaN is the FITS convention for a missing float, and cannot collide with real data
    fill = NaN
  }else{
    #A missing value marker per type. Integers use exact equality when zarr
    #converts fill back to NA, so only pixels that genuinely equal the fill are
    #lost. R cannot represent the true INT_MIN, hence -2147483647L for int32.
    fill = switch(data_type,
                  int8 = -128L, int16 = -32768L, int32 = -2147483647L,
                  uint8 = 255L, uint16 = 65535L)
    if(any(data == fill, na.rm=TRUE)){
      message('Data contains ', fill, ', which is the fill value for ', data_type,
              '; those pixels will read back as NA!')
    }
  }

  storage = if(data_type == 'bool') 'logical' else if(grepl('float', data_type)) 'double' else 'integer'
  if(bit64::is.integer64(data)){
    #storage.mode() alone leaves the integer64 class on a double vector
    data = as.numeric(data)
  }
  storage.mode(data) = storage
  return(list(data_type=data_type, data=data, fill_value=fill))
}

#The integer types a lossy write may store into, and the span of each that is
#safe to quantise. Zarr reads back any pixel equal to the array's fill value as
#NA, so the fill is excluded from the usable span, and NA is then the only thing
#that can occupy it. That is what makes the missing pixels survive the round trip
#exactly, and why the lowest (signed) or highest (unsigned) value is out of
#bounds for real data. There is no int64 or uint32 here because the zarr package
#cannot write either from R.
.zarr_quant_types = c('uint8', 'int8', 'int16', 'uint16', 'int32')

#The order the automatic choice climbs, from fewest levels to most. Capped at 16
#bit: an ordinary image has more distinct pixels than any integer type has levels,
#so an uncapped rule would answer int32 every time, which costs exactly as much as
#the float32 array it replaces and makes the whole exercise pointless. Sixteen bit
#is also the FITS convention for a scaled image, and halves a float64 archive.
#Unsigned types come first at equal levels because they have a FITS BITPIX code
#(8 and 16 with a BZERO offset), which is the standard way a byte or short image
#declares its range. int32 stays available through data_type for a caller who
#genuinely needs the wider span.
.zarr_quant_auto_types = c('uint8', 'int16', 'uint16')

#The subset of those the automatic choice is allowed to reach. Sixteen bit is the
#FITS convention for a scaled image and halves a float64 archive; anything wider
#costs as much as the float it would replace, so it is only used when asked for
#through data_type, or when whole numbers genuinely need the span to stay exact.
.zarr_quant_auto_types = c('int8', 'uint8', 'int16', 'uint16')

.zarr_quant_fill = c(int8 = -128, uint8 = 255, int16 = -32768, uint16 = 65535,
                     int32 = -2147483647)

.zarr_quant_lo = c(int8 = -127, uint8 = 0, int16 = -32767, uint16 = 0,
                   int32 = -2147483646)

.zarr_quant_hi = c(int8 = 127, uint8 = 254, int16 = 32767, uint16 = 65534,
                   int32 = 2147483647)

#The FITS BITPIX code for a Zarr integer type, or NULL where FITS has none. FITS
#has no signed byte and no unsigned short, so for those two BITPIX is left as the
#caller had it rather than filled with a code that would misstate the sign. The
#array's own data_type attribute is always the exact truth.
.zarr_quant_bitpix = function(data_type){
  return(switch(data_type, uint8 = 8L, int16 = 16L, int32 = 32L, NULL))
}

#Is a stored Zarr type one of the integer types that a quantisation can live in?
.zarr_is_int_type = function(data_type){
  return(is.character(data_type) && length(data_type) == 1 && grepl('^u?int', data_type))
}

#Quantise numeric data into integers for a lossy Zarr write, following the FITS
#convention that a scaled image stores physical = BZERO + BSCALE * stored. The
#pair is what goes into the header and what the reader uses to undo the scaling,
#so a foreign tool can recover the data with nothing but the keywords.
#
#When a scale is supplied it is honoured exactly and only the type is chosen, which
#is what lets an image read from a scaled FITS file go back out with the
#quantisation it arrived with. When none is supplied the type and scale are fitted to
#the dynamic range of the data. Whole numbers need no scaling at all, so the smallest
#type that can hold them exactly is used and no keywords are written, which keeps a
#lossy write of a segmentation map lossless. Anything else is spread over every level
#a type offers, from the narrowest up, stopping at 16 bit: see the ladder below for
#why going wider saves nothing.
#
#The shift applied to BZERO matters: stored = (physical - BZERO)/BSCALE, so the
#smallest value lands on the lowest usable integer when BZERO = min - BSCALE * lo.
#Getting the sign wrong puts the whole array at the opposite end of the range and
#then fails the span check.
.zarr_quantise = function(data, bscale = NULL, bzero = NULL, data_type = NULL){
  if(!is.null(data_type)){
    if(is.character(data_type) && !(data_type %in% .zarr_quant_types)){
      if(.zarr_is_int_type(data_type)){
        stop('lossy = TRUE cannot store "', data_type, '"; R cannot represent it. Use one of: ',
             paste(.zarr_quant_types, collapse = ', '), '!', call. = FALSE)
      }
      stop('lossy = TRUE stores integers, so data_type must be one of: ',
           paste(.zarr_quant_types, collapse = ', '), '!', call. = FALSE)
    }
  }
  if(!is.null(bscale)){
    assertNumeric(bscale, len=1)
    if(!is.finite(bscale) | bscale <= 0){
      stop('bscale must be a finite positive number when lossy = TRUE!', call. = FALSE)
    }
  }
  if(!is.null(bzero)){
    assertNumeric(bzero, len=1)
    if(!is.finite(bzero)){
      stop('bzero must be a finite number when lossy = TRUE!', call. = FALSE)
    }
  }

  #Everything below works on a double copy that keeps the shape of the input, since
  #the writer needs the dims to match the array it is about to create. Setting the
  #storage mode preserves the dim attribute, where as.numeric() would drop it and a
  #matrix would then be written as a vector. For logical data it also means
  #FALSE/TRUE become 0/1 before anything else looks at them
  if(bit64::is.integer64(data)){
    #An integer64 holds its value in the bits of a double, so changing the storage
    #mode reinterprets those bits rather than converting them. as.numeric() is the
    #conversion, and it drops the dims, which are put back
    quant = as.numeric(data)
    dim(quant) = dim(data)
  }else{
    quant = data
    storage.mode(quant) = 'double'
  }

  keep = is.finite(quant)
  vals = quant[keep]
  if(length(vals) == 0){
    stop('There are no finite pixels to quantise, so lossy = TRUE has nothing to scale!',
         call. = FALSE)
  }
  Nbad = sum(!keep & !is.na(quant))
  if(Nbad > 0){
    #NaN and Inf have no integer to become. The fill value is the only way to say
    #'not a number' in an integer array, so they arrive back as NA.
    message(Nbad, ' non finite pixels will become NA, since only the fill value of an ',
            'integer array can hold them!')
  }

  min_val = min(vals)
  max_val = max(vals)
  #A scale with one half missing keeps the FITS default for the other half
  scale_given = !(is.null(bscale) & is.null(bzero))
  fixed_bscale = if(is.null(bscale)) 1 else bscale
  fixed_bzero = if(is.null(bzero)) 0 else bzero
  #Whole numbers are already integers, so the identity stores them exactly and no
  #scaling is needed at all
  integral = all(.is_whole_number(vals))
  #The number of levels the data actually uses. This is what decides the size of the
  #smallest type worth writing: a type with fewer levels than this cannot keep the
  #values apart, however the scale is chosen, so it would be a smaller file for data
  #that has quietly become unable to say which pixels differed.
  ndistinct = length(unique(vals))

  types = if(is.null(data_type)) .zarr_quant_types else data_type
  ints = NULL

  #The integer the pixels become under one candidate scale, or NULL when the type
  #cannot hold them. Rounded in double and checked before narrowing, so the halves
  #are decided before the type is chosen. The usable span excludes the fill value by
  #construction, so no finite pixel can be quantised onto it, and NA survives the
  #round trip exactly.
  scale_try = function(vals_use, type, use_bscale, use_bzero){
    lo = as.numeric(.zarr_quant_lo[[type]])
    hi = as.numeric(.zarr_quant_hi[[type]])
    part = round((vals_use - use_bzero)/use_bscale)
    if(any(part < lo | part > hi)){
      return(NULL)
    }
    return(part)
  }

  #Can this type hold the values as they are, with no scaling? Only used for whole
  #numbers, and the test is on the doubles rather than by narrowing to R's 32 bit
  #integer, because as.integer() turns anything past that range into NA_integer_ and
  #an NA here would report a representable range as an unrepresentable one
  span_fits = function(type){
    return(min_val >= as.numeric(.zarr_quant_lo[[type]]) &&
             max_val <= as.numeric(.zarr_quant_hi[[type]]))
  }

  if(scale_given){
    #A scale the caller gave is honoured exactly, so only the type is chosen, and the
    #first one whose span can hold the scaled integers is the smallest possible
    for(type in types){
      part = scale_try(quant[keep], type, fixed_bscale, fixed_bzero)
      if(!is.null(part)){
        ints = quant
        ints[keep] = part
        chosen_type = type
        chosen_bscale = fixed_bscale
        chosen_bzero = fixed_bzero
        break
      }
    }
    if(is.null(ints)){
      needed = round((quant[keep] - fixed_bzero)/fixed_bscale)
      stop('The data range cannot be stored in ', paste(types, collapse = ' / '),
           ' with BSCALE = ', format(fixed_bscale, digits = 15), ' and BZERO = ',
           format(fixed_bzero, digits = 15), '. The integers needed run from ',
           sprintf('%.0f', min(needed)), ' to ', sprintf('%.0f', max(needed)),
           '. Give a wider data_type, or a scale of your own!', call. = FALSE)
    }
  }else if(integral){
    #Whole numbers need no scaling at all, so the smallest type that can hold them
    #exactly is taken. Exactness outranks a smaller file: the lossy alternative for
    #data this well behaved would only throw away detail that costs nothing to keep
    for(type in types){
      if(!span_fits(type)){
        next
      }
      ints = quant
      chosen_type = type
      chosen_bscale = 1
      chosen_bzero = 0
      break
    }
    if(is.null(ints) && is.null(data_type)){
      #Range beyond every type R can put into Zarr, and nobody asked for a particular
      #type. Quantising whole numbers that no type holds would lose exact values for
      #nothing, since float64 is exact for whole numbers up to 2^53 and is what
      #lossy = FALSE already writes. An explicit data_type is not refused here,
      #because a caller who names a narrow type for wide integers clearly means to
      #squeeze the data into it
      stop('The data is all whole numbers but runs from ', sprintf('%.0f', min_val),
           ' to ', sprintf('%.0f', max_val), ', which no integer type Zarr can write ',
           'from R holds exactly. Store it with lossy = FALSE to keep float64, which ',
           'is exact for whole numbers this size.', call. = FALSE)
    }
  }

  if(is.null(ints)){
    #Not whole numbers (or whole numbers the caller has asked to squeeze), so the
    #scale is fitted to the dynamic range of the data. The question the ladder answers
    #is: what is the narrowest type with enough levels to keep every distinct value
    #apart? Spreading the range over all of them gives step = range/(levels - 1), so a
    #type with fewer levels than the data has distinct values cannot separate them no
    #matter how it is shifted, and is skipped.
    auto_types = intersect(types, .zarr_quant_auto_types)
    if(length(auto_types) == 0){
      #An explicit data_type outside the automatic set, which is the caller's choice
      #and is used as is
      auto_types = types
    }

    for(type in auto_types){
      lo = as.numeric(.zarr_quant_lo[[type]])
      hi = as.numeric(.zarr_quant_hi[[type]])
      levels = hi - lo + 1
      if(ndistinct == 1){
        #A constant array has no range to spread, so a calculated step would be zero
        #and the division nonsense. Putting every pixel on integer zero with the value
        #as the zero point reproduces it exactly, and zero is inside the usable span of
        #every type and is never a fill value
        cand = c(1, min_val)
      }else if(levels < ndistinct){
        #Fewer levels than the data has distinct values, so some of them must share an
        #integer however the range is scaled, and the next type up is smaller than the
        #dynamic range the data actually has
        next
      }else{
        use_bscale = (max_val - min_val)/(levels - 1)
        cand = c(use_bscale, min_val - use_bscale * lo)
      }
      part = scale_try(quant[keep], type, cand[1], cand[2])
      if(is.null(part)){
        next
      }
      ints = quant
      ints[keep] = part
      chosen_type = type
      chosen_bscale = cand[1]
      chosen_bzero = cand[2]
      break
    }
  }

  if(is.null(ints)){
    #Nothing in the ladder can hold every distinct value apart. For real image data
    #that is the ordinary case rather than a failure, since an image has far more
    #unique pixels than any 16 bit type has levels, which is exactly why the ladder
    #stops there instead of chasing a type that would cost as much as the float array
    #it replaces. So the widest type in play is taken and the write says nothing: the
    #caller asked for a lossy file, and what the loss is sits in max_error and in the
    #header. A whole number array reaching here means the caller named a type too
    #narrow to hold it exactly, which is a choice being honoured, not an accident, so
    #only that case is reported
    chosen_type = auto_types[length(auto_types)]
    lo = as.numeric(.zarr_quant_lo[[chosen_type]])
    hi = as.numeric(.zarr_quant_hi[[chosen_type]])
    chosen_bscale = (max_val - min_val)/(hi - lo)
    chosen_bzero = min_val - chosen_bscale * lo
    part = scale_try(quant[keep], chosen_type, chosen_bscale, chosen_bzero)
    if(is.null(part)){
      stop('The data range cannot be stored in ', paste(auto_types, collapse = ', '),
           '. The values run from ', format(min_val, digits = 15), ' to ',
           format(max_val, digits = 15), '. Give a wider data_type, or a scale of your ',
           'own!', call. = FALSE)
    }
    ints = quant
    ints[keep] = part
    merged = ndistinct - length(unique(part))
    if(merged > 0 && !is.null(data_type)){
      message('Storing in ', chosen_type, ' merges ', merged, ' of the ', ndistinct,
              ' distinct values in the data. A wider data_type would keep them apart.')
    }
  }

  #The positions that were not finite (NA, NaN or Inf) have no integer to become, so
  #they are set to NA_real_ before narrowing. Coercing Inf straight to integer works,
  #but only by raising an 'NAs introduced' warning, which would look like a fault in
  #the quantisation rather than the intended loss of those pixels. NA_real_ narrows
  #to NA_integer_ quietly, and that is what Zarr writes as the fill value.
  ints[!keep] = NA_real_
  storage.mode(ints) = 'integer'
  quantised = !(chosen_bscale == 1 & chosen_bzero == 0)
  #What the quantisation actually cost, measured on the pixels that were finite
  max_error = max(abs(as.numeric(vals) - (chosen_bzero + chosen_bscale * ints[keep])))

  return(list(data = ints, data_type = chosen_type,
              fill_value = as.integer(.zarr_quant_fill[[chosen_type]]),
              bscale = chosen_bscale, bzero = chosen_bzero, quantised = quantised,
              max_error = max_error))
}

#One HISTORY line describing how the array is quantised. Used both when an array
#is created and when it grows, so the two say the same thing about the same scale.
#HISTORY is the right place for it: the card round trip keeps the text intact, and
#a note about how the file was made is not a keyword a reader has to reason about.
.zarr_scale_history = function(data_type, quantised, max_error = NULL){
  if(!isTRUE(quantised)){
    return(paste('Rfits stored as', data_type, 'integers without scaling'))
  }
  line = paste('Rfits quantised to', data_type)
  if(!is.null(max_error)){
    line = paste0(line, ' (worst pixel error ', format(max_error, digits = 5), ')')
  }
  return(line)
}

#Fold a quantisation into the metadata stored with the array. BSCALE and BZERO are
#the FITS scaled integer keywords, so the header alone is enough to undo the
#scaling. BITPIX is only written where FITS has an exact code for the type in use,
#and a HISTORY line records what happened either way.
.zarr_quant_header = function(keyvalues, keycomments, comment, history, quant){
  made_header = is.null(keyvalues)
  if(made_header){
    #A lossy array whose scale is not written down anywhere is unreadable, so the
    #three keys that make it readable are created even when no metadata was asked
    #for at all
    keyvalues = list()
    keycomments = NULL
  }
  if(is.null(keycomments) | length(keycomments) != length(keyvalues)){
    aligned = as.list(rep('', length(keyvalues)))
    names(aligned) = names(keyvalues)
    if(!is.null(keycomments)){
      matched = match(names(aligned), names(keycomments), nomatch=0)
      aligned[matched > 0] = keycomments[matched[matched > 0]]
    }
    keycomments = aligned
  }

  bitpix = .zarr_quant_bitpix(quant$data_type)
  if(!is.null(bitpix)){
    if(is.null(keycomments$BITPIX)){
      keycomments$BITPIX = 'number of bits per data pixel'
    }
    keyvalues$BITPIX = bitpix
  }

  if(quant$quantised){
    keyvalues$BSCALE = quant$bscale
    keycomments$BSCALE = 'lossy quantisation step'
    keyvalues$BZERO = quant$bzero
    keycomments$BZERO = 'lossy quantisation zero point'
  }
  history = c(history, .zarr_scale_history(quant$data_type, quant$quantised,
                                           quant$max_error))

  if(made_header & length(keyvalues) == 0){
    #An array of whole numbers needs no scale of its own, and the caller asked for no
    #metadata, so there is nothing to write down. Rfits_keyvalues_to_header also cannot
    #take an empty list, since its loop runs 1:0. Returning NULL here keeps the array
    #exactly as a plain integer write would have left it.
    return(list(keyvalues = NULL, keycomments = NULL, history = history, header = NULL))
  }

  header = Rfits_keyvalues_to_header(keyvalues, keycomments, comment, history)
  return(list(keyvalues = keyvalues, keycomments = keycomments, history = history,
              header = header))
}

#Turn the raw BSCALE and BZERO of a header into a scale, as a list of bscale, bzero
#and the quantised flag. A value that is absent, malformed or not a positive step
#falls back to the FITS default, which is the identity, and an identity says nothing
#has been scaled. That matters in both directions: a float image whose header
#carries the default BSCALE = 1 and BZERO = 0 is not a quantised array, and treating
#it as one would either leave it alone when it should be scaled or refuse a lossy
#write that has no real scale to honour.
.zarr_scale_read = function(bscale_raw, bzero_raw){
  bscale = 1
  bzero = 0
  #Short circuit operators throughout. is.finite(NULL) is logical(0) rather than
  #FALSE, so a single & would make the whole test length zero and if() would fail
  #outright on an image whose header has no scale keys at all, which is the common
  #case being handled here
  if(is.numeric(bscale_raw) && length(bscale_raw) == 1 && is.finite(bscale_raw) &&
     bscale_raw > 0){
    bscale = as.numeric(bscale_raw)
  }
  if(is.numeric(bzero_raw) && length(bzero_raw) == 1 && is.finite(bzero_raw)){
    bzero = as.numeric(bzero_raw)
  }
  return(list(bscale = bscale, bzero = bzero, quantised = !(bscale == 1 & bzero == 0)))
}

#The quantisation scale carried by an array. The card images are the authoritative
#copy and the only one that survives at full precision: a BSCALE of 1.5e-4 stored
#through a Zarr attribute comes back from JSON as 0.0002, which would destroy the
#data, while a FITS card keeps fourteen significant figures. The structured keywords
#are a fallback for an array that has no cards at all.
.zarr_scale_of = function(node){
  found = NULL
  header = tryCatch(node$attribute('header'), error = function(e) NULL)
  if(is.character(header) & length(header) > 0){
    #When the cards are present but hold neither key the answer is the identity, and
    #the structured keywords are NOT consulted, because they are a mirror of the same
    #header and reading them would mean decoding the whole keyword list. That matters
    #because this is called once per array while a store describes itself, where the
    #cost of parsing every keyvalues attribute would be paid for nothing. The fallback
    #only applies to an array that has no card images at all.
    cards = header[grepl('^BSCALE *=', header) | grepl('^BZERO *=', header)]
    if(length(cards) == 0){
      return(.zarr_scale_read(NULL, NULL))
    }
    found = tryCatch(Rfits_header_to_keyvalues(cards), error = function(e) NULL)
  }else{
    found = .zarr_json_to_list(tryCatch(node$attribute('keyvalues'), error = function(e) NULL))
  }
  if(is.null(found)){
    return(.zarr_scale_read(NULL, NULL))
  }
  return(.zarr_scale_read(found$BSCALE, found$BZERO))
}

#The scale to apply when reading this array, or NULL when the pixels are to be
#returned as stored. Rfits records the answer in the array's own lossy attribute as
#it writes it, and that is what this reports, because the header keywords alone
#cannot tell the two cases apart.
#
#A FITS reader applies BSCALE and BZERO itself, so Rfits_read_image of a scaled image
#hands back physical values already, even when the pixels arrive as integers. Writing
#those to Zarr copies the header keywords along with them, which would leave an array
#holding physical values under keywords that describe something else, and scaling it
#again on read would apply the scale twice.
#
#An array with no lossy attribute was not written by a version of Rfits that knows
#about quantisation, and the two ways that can happen need opposite answers. If the
#array carries Rfits' own technical mirror it came from an older Rfits, which wrote
#whatever it was given and never scaled anything on read, so it must go on not being
#scaled or a store that already exists changes meaning under its reader. Otherwise the
#array was written by some other tool, and the FITS convention is the only reading
#available for it: integers on disk plus a scale in the header means the scale is
#still to be applied. A float array is never scaled on a guess either way, since a
#converter that copies headers onto physical floats is common and the keywords would
#be lying.
.zarr_read_scale = function(node){
  stored_type = tryCatch(as.character(node$metadata$data_type), error = function(e) '')
  if(!.zarr_is_int_type(stored_type)){
    return(NULL)
  }
  lossy = tryCatch(node$attribute('lossy'), error = function(e) NULL)
  if(is.logical(lossy) && length(lossy) == 1 && !is.na(lossy)){
    #Written by Rfits, so the record is the truth and the keywords are not consulted
    #a second time
    if(!lossy){
      return(NULL)
    }
  }else if(!is.null(tryCatch(node$attribute('data_type'), error = function(e) NULL))){
    #An older Rfits array, which was read as raw integers when it was written
    return(NULL)
  }
  scale = .zarr_scale_of(node)
  if(!scale$quantised){
    return(NULL)
  }
  return(scale)
}

#Grow an existing array along dimension 1 and write the new elements into the
#space. The data is the increment, not the complete final array: an array that is
#currently 4 x 6 grows to 7 x 6 when given 3 x 6. Dimension 1 is the slowest
#varying axis in FITS, R and Zarr alike, so the existing elements keep their
#indices. low is never touched in the resize, because shrinking is out of scope
#and zarr warns about chunk rounding for a non zero low.
.zarr_append = function(store, extname, data, shape, typed, data_type_requested,
                        codec_config, compressor_requested, update_root, filename_out,
                        scale_note = NULL){
  path = .zarr_name_to_path(extname)
  if(!(path %in% store$arrays)){
    stop('Cannot append to non-existent store extension: ', extname, call. = FALSE)
  }
  node = store$get_node(path)
  if(is.null(node)){
    stop('Cannot find node for extension: ', extname, call. = FALSE)
  }

  old_shape = as.vector(as.integer(node$shape))
  inc_shape = as.vector(as.integer(shape))
  Ndim = length(old_shape)

  if(length(inc_shape) != Ndim){
    stop('Image dimensions do not match: the existing array ', extname, ' has ', Ndim,
         ' dimensions and the appended data has ', length(inc_shape),
         '. Appending can only grow dimension 1, so write a new extension instead!',
         call. = FALSE)
  }
  if(Ndim > 1 & any(old_shape[-1] != inc_shape[-1])){
    stop('Image dimensions do not match: all dimensions except the first must be equal ',
         '(', paste(old_shape, collapse = ' x '), ' vs ',
         paste(old_shape[1], inc_shape[-1], collapse = ' x '), ')!', call. = FALSE)
  }
  if(inc_shape[1] < 1L){
    stop('Cannot append: the data has no elements along dimension 1!', call. = FALSE)
  }

  total_shape = inc_shape
  total_shape[1] = old_shape[1] + inc_shape[1]

  #The stored type is never widened or narrowed by an append
  existing_type = tryCatch(as.character(node$metadata$data_type), error = function(e) '')
  if(nzchar(existing_type) & typed$data_type != existing_type){
    if(!is.null(data_type_requested)){
      stop('Cannot append: data_type "', typed$data_type, '" was requested but the existing ',
           'array ', extname, ' is "', existing_type, '"!', call. = FALSE)
    }
    message('Appending to an existing ', existing_type, ' array; converting the new data ',
            'from ', typed$data_type, ' to match.')
    typed = .zarr_data_type(data, data_type = existing_type)
    data = typed$data
  }

  #An append cannot change how an existing array is compressed. Only say so when
  #the caller actually asked, since otherwise every append to a non default array
  #would complain about a setting nobody requested
  if(compressor_requested){
    applied = .zarr_codec_of(node)
    if(!is.null(applied)){
      want_shuffle = codec_config$shuffle
      if(is.null(want_shuffle)){
        shuffle_differs = FALSE
      }else{
        shuffle_differs = !(want_shuffle %in% c(applied$shuffle,
                                .zarr_default_shuffle_for(existing_type)))
      }
      if(applied$cname != codec_config$cname | applied$clevel != codec_config$clevel |
         shuffle_differs){
        message('The existing array ', extname, ' is compressed with blosc/', applied$cname,
                ' at level ', applied$clevel, '; the compressor arguments were not applied.')
      }
    }
  }

  N0 = old_shape[1]
  grew = inc_shape[1]
  start_index = N0 + 1L
  end_index = N0 + grew

  node$resize(low = rep(0L, Ndim), high = c(grew, rep(0L, Ndim - 1)))

  selection = vector(mode = 'list', length = Ndim)
  selection[[1]] = c(start_index, end_index)
  if(Ndim > 1){
    for(i in 2:Ndim){
      selection[[i]] = c(1L, total_shape[i])
    }
  }
  node$write(data, selection = selection)

  meta = .zarr_get_meta(node)
  append_entry = list(appended_count = as.integer(grew),
                      start_index = as.integer(start_index),
                      end_index = as.integer(end_index),
                      extname = extname)

  if(!is.null(meta)){
    .zarr_append_header(node, meta, existing_type = node$metadata$data_type,
                        new_shape = total_shape, grew = grew, start_index = start_index,
                        end_index = end_index, scale_note = scale_note)
  }else{
    #A bare array has no FITS metadata to keep in step. Nothing is invented for it,
    #since a half populated header would be worse than none at all.
    message('The existing array ', extname, ' has no FITS style metadata, so no header ',
            'keys were updated by the append.')
  }

  node$save()

  if(update_root){
    .zarr_root_refresh(store, append_entry = append_entry)
  }

  return(list(filename = filename_out,
              extname = extname,
              start_index = as.integer(start_index),
              end_index = as.integer(end_index),
              appended = as.integer(grew),
              dim = as.vector(as.integer(node$shape)),
              data_type = as.character(node$metadata$data_type),
              chunk_shape = .zarr_chunk_shape_of(node)))
}

#zarr picks a shuffle appropriate to the dtype when none is given, which is not a
#mismatch with an explicit request for it
.zarr_default_shuffle_for = function(data_type){
  if(!nzchar(data_type)){
    return(NULL)
  }
  if(data_type %in% c('bool', 'int8', 'uint8')){
    return('noshuffle')
  }
  if(data_type %in% c('int16', 'uint16', 'int32', 'uint32', 'int64', 'float32')){
    return('shuffle')
  }
  return('bitshuffle')
}

#Keep the FITS metadata honest after an append: the shape keys follow the array,
#and a HISTORY line records what happened. Keywords already in the header win,
#since the alternative is silently rewriting the WCS of a whole extension; keys
#supplied with the append are added only where they do not conflict.
#The scale_note is the line describing a quantisation applied to the increment,
#which is worth recording because the increment was not stored as it arrived. An
#append that needed no scaling adds nothing, so the header keeps the note that
#described it when it was created and does not gain a duplicate.
.zarr_append_header = function(node, meta, existing_type, new_shape, grew, start_index,
                               end_index, scale_note = NULL){
  keyvalues = meta$keyvalues
  keycomments = meta$keycomments
  comment = meta$comment
  history = meta$history

  naxis_key = ifelse(isTRUE(keyvalues$ZIMAGE), 'ZNAXIS', 'NAXIS')
  key1 = paste0(naxis_key, '1')
  keyvalues[[key1]] = as.integer(new_shape[1])
  if(is.null(keycomments[[key1]])){
    keycomments[[key1]] = 'APPENDED'
  }else if(!grepl('APPENDED', keycomments[[key1]])){
    keycomments[[key1]] = paste(keycomments[[key1]], 'APPENDED')
  }
  if(length(new_shape) > 1){
    keyvalues[[paste0(naxis_key, '2')]] = as.integer(new_shape[2])
  }

  new_keyvalues = .zarr_json_to_list(node$attribute('keyvalues'))
  if(!is.null(new_keyvalues)){
    added = setdiff(names(new_keyvalues), names(keyvalues))
    if(length(added) > 0){
      for(name in added){
        keyvalues[[name]] = new_keyvalues[[name]]
      }
      aligned = as.list(rep('', length(keyvalues)))
      names(aligned) = names(keyvalues)
      matched = match(names(aligned), names(keycomments), nomatch = 0)
      aligned[matched > 0] = keycomments[matched[matched > 0]]
      keycomments = aligned
    }
  }

  #Comments are realigned to the keywords before the cards are rebuilt. The shape
  #keys above are written into keyvalues whether or not the header had them, and an
  #array whose metadata never mentioned NAXIS (which is what a bare quantised write
  #creates, since it only needs the scale) would otherwise end up with one more value
  #than comment and fail inside Rfits_keyvalues_to_header
  if(is.null(keycomments) | length(keycomments) != length(keyvalues) |
     !identical(names(keycomments), names(keyvalues))){
    aligned = as.list(rep('', length(keyvalues)))
    names(aligned) = names(keyvalues)
    matched = match(names(aligned), names(keycomments), nomatch=0)
    aligned[matched > 0] = keycomments[matched[matched > 0]]
    keycomments = aligned
  }

  stamp = format(Sys.time(), '%Y-%m-%dT%H:%M:%SZ', tz = 'UTC')
  line = paste('Rfits appended', grew, 'elements to dimension 1 at',
               paste0('[', start_index, ',', end_index, ']'), 'on', stamp)
  history = c(history, line)
  if(!is.null(scale_note)){
    history = c(history, scale_note)
  }

  header = Rfits_keyvalues_to_header(keyvalues, keycomments, comment, history)
  node$set_attribute('header', as.character(header))
  node$set_attribute('keyvalues', .zarr_clean_list(keyvalues))
  node$set_attribute('keycomments', .zarr_clean_list(keycomments))
  node$set_attribute('history', as.character(history))
  if(!is.null(comment)){
    node$set_attribute('comment', as.character(comment))
  }
  return(invisible(header))
}

#Split the space into contiguous runs of whole chunks along one axis, so that no two
#bands share a chunk. That is what makes it safe for a worker to write a band without
#reading anything another worker has written: a chunk is only ever opened by one
#process, and zarr flushes a chunk in full. Returns one list per band of the 1 based
#inclusive bounds per dimension, which is what zarr node$write wants as a selection, or
#NULL when no axis has two chunks to split. The highest such axis is used, since R
#stores dim 1 fastest varying, so a band cut along a later axis is contiguous in memory.
.zarr_chunk_bands = function(shape, chunk_shape, nbands){
  Nd = length(shape)
  nchunks = ceiling(shape/chunk_shape)
  splittable = which(nchunks >= 2)
  if(length(splittable) == 0){
    return(NULL)
  }
  axis = max(splittable)
  nch = nchunks[axis]
  nbands = min(nbands, nch)
  #Whole chunks per band, front loaded, so the bands differ in size by at most one chunk
  per = integer(nbands)
  rest = nch
  for(k in seq_len(nbands)){
    left = nbands - k + 1L
    per[k] = ceiling(rest/left)
    rest = rest - per[k]
  }
  hi_chunk = cumsum(per)
  lo_chunk = hi_chunk - per + 1L
  full = lapply(seq_len(Nd), function(d) c(1L, as.integer(shape[d])))
  return(lapply(seq_len(nbands), function(k){
    sel = full
    sel[[axis]] = c(as.integer((lo_chunk[k] - 1L) * chunk_shape[axis] + 1L),
                    as.integer(min(hi_chunk[k] * chunk_shape[axis], shape[axis])))
    return(sel)
  }))
}

#Write one chunk aligned band, which is the whole of a worker's job. Everything is
#reached through :: or through the job, since a worker starts with nothing from the
#parent. The store is reopened from its path rather than carried across, because a
#store object keeps its data and its connection in the process that opened it.
.zarr_write_band = function(job){
  #Rfits is attached rather than assumed, since the function is reached by name on the
  #worker. zarr is not: every call into it here goes through zarr::, which loads the
  #namespace without attaching it, and attaching a package from Suggests is what R CMD
  #check objects to.
  suppressPackageStartupMessages(library(Rfits))
  store = .zarr_store_open(job$filename, write = TRUE)
  node = store$get_node(job$path)
  node$write(job$band, selection = job$selection)
  return(length(job$band))
}

#Fan the data out over workers as whole bands
.zarr_write_bands_parallel = function(filename, path, data, shape, chunk_shape, cores){
  bands = .zarr_chunk_bands(shape, chunk_shape, cores)
  if(is.null(bands)){
    return(invisible(FALSE))
  }
  cluster = parallel::makeCluster(min(cores, length(bands)), type = 'PSOCK')
  on.exit(parallel::stopCluster(cluster), add = TRUE)
  #The worker is named rather than passed as a function object, because parLapply ships
  #a closure together with its environment, and the environment of the caller holds the
  #array being written, so every worker would receive its own copy of all of it. A name
  #is resolved on the node, which is why it is defined there first. invisible() because
  #clusterEvalQ would print the value from every node.
  invisible(parallel::clusterEvalQ(cluster, {
    #::: and not a bare name. The expression runs on the worker, where Rfits is in the
    #namespace but its internals are not on the search path, so a bare .zarr_write_band
    #is not found and every band fails before a pixel is written.
    zarr_band_worker = Rfits:::.zarr_write_band
  }))
  jobs = lapply(bands, function(selection){
    #Sliced here rather than in the worker, so what each worker is sent is only its own
    #band. This is also why the split is taken along a trailing dimension, since R
    #stores the leading dimension fastest and such a band is one contiguous block.
    #drop = FALSE because a band that covers a whole axis of length one is otherwise
    #flattened to a vector, and a store cannot broadcast that into its selection.
    band = do.call(`[`, c(list(data), lapply(selection, function(b) b[1]:b[2]),
                          list(drop = FALSE)))
    return(list(filename = filename, path = path, selection = selection, band = band))
  })
  counts = parallel::parLapply(cluster, jobs, 'zarr_band_worker')
  written = sum(vapply(counts, function(n) if(is.numeric(n)) n else 0, numeric(1)))
  if(!identical(as.numeric(written), as.numeric(prod(shape)))){
    stop('The parallel Zarr write only covered ', written, ' of ', prod(shape),
         ' pixels!', call. = FALSE)
  }
  return(invisible(TRUE))
}

Rfits_write_image_zarr = function(data, filename='temp.zarr', extname='data1', create_ext=TRUE,
                                  overwrite_file=FALSE, data_type=NULL, chunk_shape=NULL,
                                  clevel=6L, compressor='blosc', shuffle=NULL,
                                  append=FALSE, update_root=TRUE, cores=NULL,
                                  lossy=FALSE, bscale=NULL, bzero=NULL,
                                  keyvalues, keycomments, keynames, comment, history){
  .zarr_require()

  filename_is_store = .zarr_is_store(filename)
  if(filename_is_store){
    store_label = .zarr_store_label(filename)
  }else{
    assertCharacter(filename, max.len=1)
    filename = path.expand(filename)
  }
  assertCharacter(extname, max.len=1)
  assertFlag(create_ext)
  assertFlag(overwrite_file)
  assertFlag(append)
  assertFlag(update_root)
  assertFlag(lossy)
  assertNumeric(bscale, len=1, null.ok=TRUE)
  assertNumeric(bzero, len=1, null.ok=TRUE)
  assertIntegerish(clevel, len=1, lower=0, upper=9)
  assertIntegerish(cores, len=1, lower=1, null.ok=TRUE)

  if(append & overwrite_file){
    stop('Cannot use both append and overwrite_file!', call. = FALSE)
  }
  #A lossy write replaces the array rather than extending it. An append could not
  #use a quantisation chosen from the increment alone: the scale has to be the one
  #the array was created with, and that is decided here so the branch below is not
  #reached with a scale that has already been fitted to the wrong data
  if(lossy & append){
    stop('Cannot use both lossy and append! An appended increment has to be stored ',
         'with the scale of the array that already exists, so re write the whole ',
         'array with lossy = TRUE instead.', call. = FALSE)
  }
  #Resolved before anything is created, so a bad name warns rather than silently
  #recording a compressor that does not match the bytes
  codec_config = .zarr_compressor(compressor, clevel=clevel, shuffle=shuffle)
  #Whether the caller asked for a compression setting at all, which the append
  #path uses to decide whether to report that a request went unfulfilled
  compressor_requested = !(missing(compressor) & missing(clevel) & missing(shuffle))

  if(!zarr::is_valid_node_name(sub('^/', '', extname))){
    stop('extname is not a valid Zarr array name: ', extname, call. = FALSE)
  }

  if(filename_is_store){
    #There is no directory to unlink or check the permissions of, so an emptied
    #store stands in for a removed one, and nothing else is verified locally
    if(overwrite_file & !create_ext){
      .zarr_store_clear(filename)
    }
    store = .zarr_store_open(filename, write=TRUE)
  }else{
    if(overwrite_file & !create_ext & dir.exists(filename)){
      assertAccess(filename, access='w')
      unlink(filename, recursive=TRUE)
    }else{
      #A Zarr store is a directory, so assertPathForOutput checks its parent
      assertPathForOutput(filename, overwrite=TRUE)
    }

    if(dir.exists(filename)){
      store = .zarr_store_open(filename, write=TRUE)
    }else{
      store = .zarr_store_new(filename)
    }
  }

  hdr_from_object = NULL
  if(inherits(data, what=c('Rfits_vector', 'Rfits_image', 'Rfits_cube', 'Rfits_array'))){
    #Like the HDF5 back-end, trust the header stored on the object as-is
    hdr_from_object = data$header
    if(missing(keyvalues)){keyvalues = data$keyvalues}
    if(missing(keycomments)){keycomments = data$keycomments}
    if(missing(comment)){comment = data$comment}
    if(missing(history)){history = data$history}
    data = data$imDat
  }

  if(missing(keyvalues)){keyvalues = NULL}
  if(missing(keycomments)){keycomments = NULL}
  if(missing(comment)){comment = NULL}
  if(missing(history)){history = NULL}
  if(missing(keynames)){keynames = NULL}

  #The card images are the authoritative copy of the metadata. When none came
  #with the object, derive them from whatever keywords have been given.
  if(!is.null(hdr_from_object)){
    header = hdr_from_object
  }else if(!is.null(keyvalues)){
    if(is.null(keycomments) | length(keycomments) != length(keyvalues)){
      aligned = as.list(rep('', length(keyvalues)))
      names(aligned) = names(keyvalues)
      if(!is.null(keycomments)){
        matched = match(names(aligned), names(keycomments), nomatch=0)
        aligned[matched > 0] = keycomments[matched[matched > 0]]
      }
      keycomments = aligned
    }
    header = Rfits_keyvalues_to_header(keyvalues, keycomments, comment, history)
  }else{
    header = NULL
    keycomments = NULL
  }

  #An integer64 vector is not is.vector(), so branch on the presence of dims
  if(is.null(dim(data))){
    assertVector(data)
    shape = length(data)
  }else{
    assertArray(data)
    shape = dim(data)
  }
  shape = as.integer(shape)

  if(length(shape) > 4){
    stop('Only up to 4 dimensional data can be stored as a FITS style image!')
  }

  filename_out = if(filename_is_store) store_label else filename

  quant = NULL
  if(lossy){
    #A scale is only calculated from the pixels when the caller has not supplied one
    #and the header does not already carry one. Re using the keywords of an image
    #that arrived scaled keeps it on the quantisation it was written with, so a FITS
    #to Zarr conversion cannot change which physical value each stored integer means
    use_bscale = bscale
    use_bzero = bzero
    if(is.null(use_bscale) & is.null(use_bzero)){
      existing_scale = .zarr_scale_read(keyvalues$BSCALE, keyvalues$BZERO)
      #quantised is FALSE for the FITS default pair, so a header that merely carries
      #BSCALE = 1 and BZERO = 0 cannot pin a lossy write to a scale that does nothing
      if(existing_scale$quantised){
        use_bscale = existing_scale$bscale
        use_bzero = existing_scale$bzero
      }else{
        #Only worth saying when the scale had to be invented from these pixels. The
        #keys that carry it are recorded in the array itself, which is where a later
        #reader (or an append) will find them
        message('Lossy quantisation scale calculated from these pixels; ',
                'no BSCALE or BZERO was supplied or already in the header.')
      }
    }
    quant = .zarr_quantise(data, bscale=use_bscale, bzero=use_bzero, data_type=data_type)
    data = quant$data
    #The keywords that carry the scale are rebuilt rather than left as they were,
    #since a scale fitted to these pixels did not exist until now
    meta_new = .zarr_quant_header(keyvalues, keycomments, comment, history, quant)
    keyvalues = meta_new$keyvalues
    keycomments = meta_new$keycomments
    history = meta_new$history
    header = meta_new$header
  }

  typed = if(is.null(quant)){
    .zarr_data_type(data, data_type=data_type)
  }else{
    list(data_type=quant$data_type, data=quant$data, fill_value=quant$fill_value)
  }
  data = typed$data

  path = .zarr_name_to_path(extname)
  exists_already = path %in% store$arrays

  #Appending grows the array that is already there, so it has to branch before
  #anything is deleted or created
  if(append){
    #An array written with lossy = TRUE holds integers, and physical values are only
    #recovered by scaling them. Coercing the increment to the stored type would write
    #physical values as if they were already integers, so the scale the array was made
    #with is applied to it here. That is also why lossy is refused above: the scale
    #belongs to the array, and an append has no business choosing a new one
    append_scale_note = NULL
    if(path %in% store$arrays){
      existing_node = tryCatch(store$get_node(path), error = function(e) NULL)
      if(!is.null(existing_node)){
        #The same decision the reader makes, so an increment can only ever be put on
        #the grid that a read of the array will take it back off. An array carrying
        #BSCALE in a copied header but not actually quantised returns NULL here, and
        #the increment is then stored as it arrived, which is also what a read gives
        existing_scale = .zarr_read_scale(existing_node)
        if(!is.null(existing_scale)){
          existing_type = tryCatch(as.character(existing_node$metadata$data_type),
                                   error = function(e) '')
          if(!(existing_type %in% .zarr_quant_types)){
            stop('Cannot append: the existing array ', extname, ' carries BSCALE and BZERO ',
                 'but is stored as "', existing_type, '", which Rfits cannot quantise into!',
                 call. = FALSE)
          }
          quant = .zarr_quantise(data, bscale=existing_scale$bscale,
                                 bzero=existing_scale$bzero, data_type=existing_type)
          data = quant$data
          typed = list(data_type=quant$data_type, data=quant$data,
                       fill_value=quant$fill_value)
          #The note says what happened to this increment, not what the array is, so
          #that the append leaves a record of the quantisation it applied
          append_scale_note = .zarr_scale_history(quant$data_type, quant$quantised,
                                                  quant$max_error)
        }
      }
    }
    #Appending has to resize the array before any of it can be written, and a resize is
    #a single metadata operation that the bands cannot be arranged around, so the
    #increment is written in one. Said out loud because a caller who sets cores batch
    #wide would otherwise have no idea which of their appends were parallel.
    if(!is.null(cores)){
      message('cores is ignored when append = TRUE; the increment is written in one.',
              call. = FALSE)
    }
    return(invisible(.zarr_append(store = store, extname = extname, data = data,
                                  shape = shape, typed = typed,
                                  data_type_requested = data_type,
                                  codec_config = codec_config,
                                  compressor_requested = compressor_requested,
                                  update_root = update_root,
                                  filename_out = filename_out,
                                  scale_note = append_scale_note)))
  }

  if(exists_already){
    if(!create_ext){
      stop('Extension ', extname, ' already exists, and create_ext = FALSE!')
    }
    store$delete_array(path)
  }

  builder = zarr::define_array(typed$data_type, shape)
  builder$fill_value = typed$fill_value
  if(is.null(chunk_shape)){
    chunk_shape = zarr::optimal_chunking(shape)
  }
  chunk_shape = as.integer(chunk_shape)
  #Checked separately so a wrong length does not trigger vector recycling warnings
  if(length(chunk_shape) != length(shape)){
    stop('chunk_shape must have one entry per dimension, i.e. of length ', length(shape), '!')
  }
  if(any(chunk_shape < 1L) | any(chunk_shape > shape)){
    stop('chunk_shape entries must be between 1 and the size of that dimension!')
  }
  builder$chunk_shape = chunk_shape
  builder$remove_codec('blosc')
  builder$add_codec('blosc', codec_config)

  node = store$add_array('/', sub('^/', '', extname), .zarr_array_metadata(builder, store$store))

  #The pixels go in either as one write or as a set of disjoint bands, and the metadata
  #afterwards either way, since the bands write into the array the node already describes
  in_bands = FALSE
  if(!is.null(cores)){
    #A store object cannot be handed to a worker. zarr keeps a store's contents in
    #memory for some store types, and a client connected by the parent for others, so
    #whatever a worker wrote would either be invisible here or go through a connection
    #that belongs to another process. Only a path names a store two processes can each
    #open for themselves, which is what makes the parallel write safe.
    if(filename_is_store){
      message('cores is ignored for a Zarr store object, since only a local store path ',
              'can be opened by the workers independently. The array is written in one.',
              call. = FALSE)
    }else{
      in_bands = .zarr_write_bands_parallel(filename = filename, path = path, data = data,
                                            shape = shape, chunk_shape = chunk_shape,
                                            cores = cores)
      if(!in_bands){
        message('cores is ignored: no dimension of ', paste(shape, collapse = ' x '),
                ' is large enough to hold two ', paste(chunk_shape, collapse = ' x '),
                ' chunks, so there is nothing to split. The array is written in one.',
                call. = FALSE)
      }
    }
  }

  if(!in_bands){
    node$write(data)
  }

  #FITS style metadata, stored as attributes on the array node
  if(!is.null(header)){
    node$set_attribute('header', as.character(header))
  }
  if(!is.null(keyvalues)){
    keyvalues = .zarr_clean_list(keyvalues)
    node$set_attribute('keyvalues', keyvalues)
    if(is.null(keycomments) | length(keycomments) != length(keyvalues)){
      keycomments = as.list(rep('', length(keyvalues)))
      names(keycomments) = names(keyvalues)
    }
    node$set_attribute('keycomments', .zarr_clean_list(keycomments))
  }
  if(!is.null(comment)){
    node$set_attribute('comment', as.character(comment))
  }
  if(!is.null(history)){
    node$set_attribute('history', as.character(history))
  }

  #The technical parameters are recorded from the array that now exists, not from
  #what was asked for, so a clamped or defaulted value cannot be misreported. The one
  #thing only the writer knows is whether these pixels were quantised, since a header
  #carried over from a scaled FITS file describes where the data came from rather than
  #what is in this array, so that is passed in explicitly
  tech = .zarr_array_tech(node, lossy_asked = !is.null(quant) && quant$quantised)
  .zarr_array_tech_set(node, tech)
  applied = .zarr_codec_of(node)
  if(!is.null(applied) & (applied$cname != codec_config$cname |
                          applied$clevel != codec_config$clevel)){
    message('Requested blosc/', codec_config$cname, ' at level ', codec_config$clevel,
            '; the array was created with blosc/', applied$cname, ' at level ',
            applied$clevel, '. The recorded values are the applied ones.')
  }

  node$save()

  if(update_root){
    .zarr_root_refresh(store)
  }

  return(invisible(list(filename = filename_out,
                        extname = extname,
                        dim = shape, data_type = typed$data_type,
                        chunk_shape = chunk_shape,
                        compressor = if(is.null(applied)) codec_config$cname else applied$cname,
                        compression_level = if(is.null(applied)) codec_config$clevel else applied$clevel,
                        lossy = !is.null(quant),
                        quantised = if(is.null(quant)) FALSE else quant$quantised,
                        bscale = if(is.null(quant)) NULL else quant$bscale,
                        bzero = if(is.null(quant)) NULL else quant$bzero,
                        max_error = if(is.null(quant)) NULL else quant$max_error)))
}

Rfits_write_vector_zarr = Rfits_write_image_zarr
Rfits_write_cube_zarr = Rfits_write_image_zarr
Rfits_write_array_zarr = Rfits_write_image_zarr

#Thin wrappers, provided because a name that says what is happening is clearer at
#the call site than remembering the argument that switches the writer to it. Built
#as a call rather than passing append = TRUE directly, so that a caller who also
#supplies append gets the sensible value rather than the R error for an argument
#matched by multiple actual arguments. Anything left out stays missing, which the
#writer relies on to tell an absent keyvalues from an explicitly NULL one.
Rfits_append_image_zarr = function(data, filename='temp.zarr', extname='data1', ...){
  args = list(...)
  args$append = TRUE
  return(do.call(Rfits_write_image_zarr,
                 c(list(data = data, filename = filename, extname = extname), args)))
}

Rfits_append_vector_zarr = Rfits_append_image_zarr
Rfits_append_cube_zarr = Rfits_append_image_zarr
Rfits_append_array_zarr = Rfits_append_image_zarr

#The store name for a FITS file: the base name with the extension removed and
#.zarr added, so image_example.fits becomes image_example.zarr. Both .fit and
#.fits are handled, compressed or not, since those are the names the readers
#accept. Only the basename is kept, which is what makes a collision between two
#files of the same name visible to the caller rather than silent.
.zarr_store_stub = function(filename){
  base = basename(filename)
  return(paste0(sub('\\.(fits|fit)(\\.gz)?$', '', base, ignore.case = TRUE), '.zarr'))
}

#The FITS names the readers take, matched the way Rfits_key_scan matches them
.zarr_fits_pattern = '\\.(fits|fit)(\\.gz)?$'

#Convert one FITS file to one store and report what happened as a single row. Kept
#out of Rfits_dir_to_zarr so that the serial and parallel paths run exactly the same
#code, and so that nothing here writes into a shared data frame from inside a worker,
#where the row would be lost the moment it came back.
#
#Both the progress message and the warning about a skipped file are raised in here
#rather than by the caller, so that the serial path reports a file the moment it is
#handled, exactly as it did before this was pulled out of the loop. In a worker the
#message streams out as it is raised, but the warning is collected and only raised
#once the batch is over, which is later but not lost.
.zarr_convert_one = function(i, fullnames, filelist, stubs, target, prefix,
                             bucket, region, endpoint, access_key, secret_key,
                             session_token, ext, extname, args, verbose = TRUE){
  remote = !is.null(bucket)

  file_in = fullnames[i]
  name_out = stubs[i]

  #Decided before the read, so a file that fails still records where it was aimed
  if(remote){
    store_prefix = paste0(.zarr_s3_prefix(prefix), name_out)
    dest = paste0('s3://', bucket, '/', store_prefix)
    shown = paste0(bucket, '/', store_prefix)
  }else{
    dest = file.path(target, name_out)
    shown = dest
  }

  if(verbose){
    message('[', i, '/', length(fullnames), '] ', filelist[i], ' -> ', shown)
  }

  #Recorded rather than returned directly, because an assignment inside a handler is
  #local to that handler and would be lost the moment the loop moved on
  written = NULL
  nkey = NA_integer_
  fail = NULL

  tryCatch({
    data = Rfits_read_image(file_in, ext = ext, header = TRUE)
    if(!inherits(data, c('Rfits_vector', 'Rfits_image', 'Rfits_cube', 'Rfits_array'))){
      stop('Extension ', ext, ' of ', basename(file_in), ' is not an image!')
    }
    nkey = length(data$keyvalues)

    if(remote){
      #One store per file, so the store object is built per iteration
      store = .zarr_s3_store_for(bucket = bucket, prefix = store_prefix, region = region,
                                 endpoint = endpoint, access_key = access_key,
                                 secret_key = secret_key, session_token = session_token)
    }else{
      store = dest
    }

    written = do.call(Rfits_write_image_zarr,
                      c(list(data = data, filename = store, extname = extname), args))
  }, error = function(e) {
    fail <<- conditionMessage(e)
  })

  if(is.null(fail)){
    #The writer reports the name it used, which for a store object is the URI
    #including the trailing slash. The pre-computed dest is kept for a failure,
    #where it records what the run was aimed at
    return(list(store = written$filename,
                dim = paste(written$dim, collapse = 'x'),
                data_type = written$data_type,
                nkey = nkey,
                status = 'ok',
                error = NA_character_))
  }

  if(verbose){
    warning('Skipping ', basename(file_in), ': ', fail, call. = FALSE)
  }
  return(list(store = dest,
              dim = NA_character_,
              data_type = NA_character_,
              nkey = NA_integer_,
              status = 'error',
              error = fail))
}

Rfits_dir_to_zarr = function(dir = NULL, filelist = NULL, pattern = NULL, recursive = TRUE,
                             target = NULL, bucket = NULL, prefix = '',
                             region = NULL, endpoint = NULL,
                             access_key = NULL, secret_key = NULL, session_token = NULL,
                             ext = 1L, extname = 'data1', cores = NULL, verbose = TRUE, ...){
  .zarr_require()

  assertString(dir, null.ok = TRUE)
  assertCharacter(filelist, null.ok = TRUE)
  assertCharacter(pattern, null.ok = TRUE)
  assertFlag(recursive)
  assertFlag(verbose)
  assertString(target, null.ok = TRUE)
  assertString(prefix)
  assertString(region, null.ok = TRUE)
  assertString(endpoint, null.ok = TRUE)
  assertString(access_key, null.ok = TRUE)
  assertString(secret_key, null.ok = TRUE)
  assertString(session_token, null.ok = TRUE)
  assertIntegerish(ext, len = 1)
  assertString(extname)
  assertIntegerish(cores, len = 1, lower = 1, null.ok = TRUE)

  #Reading the credentials out of the environment is worth doing here, since the
  #alternative is a key in a script and this function exists to be scripted. bucket
  #is deliberately NOT taken from the environment: passing it explicitly is what
  #asks for a remote run, so that ambient credentials cannot silently redirect a
  #local batch. Anything passed wins over the environment. The names are the ones
  #the package tests already use.
  env_default = function(value, name){
    if(!is.null(value)){
      return(value)
    }
    value = Sys.getenv(name, '')
    if(nzchar(value)){
      return(value)
    }
    return(NULL)
  }
  region = env_default(region, 'RFITS_S3_REGION')
  endpoint = env_default(endpoint, 'RFITS_S3_ENDPOINT')
  access_key = env_default(access_key, 'RFITS_S3_ACCESS_KEY')
  secret_key = env_default(secret_key, 'RFITS_S3_SECRET_KEY')
  session_token = env_default(session_token, 'RFITS_S3_SESSION_TOKEN')

  #Where the output goes. A bucket means remote, and the stores are named by prefix
  #plus stub; a target is a directory the stores are created in. Giving both is a
  #mistake rather than a choice to be resolved, since it is not obvious which one
  #the caller meant.
  remote = !is.null(bucket)
  if(remote && !is.null(target)){
    stop('Give either target (a local directory) or bucket (a remote store), not both!',
         call. = FALSE)
  }
  if(!remote && is.null(target)){
    stop('One of target or bucket is required!', call. = FALSE)
  }

  #Gather the input files before anything is created, so a bad specification costs
  #nothing over the network
  if(is.null(filelist)){
    if(is.null(dir)){
      stop('One of dir or filelist is required!', call. = FALSE)
    }
    dir = path.expand(dir)
    if(!dir.exists(dir)){
      stop('Directory does not exist: ', dir, call. = FALSE)
    }
    #Kept relative to dir so the collision report says which sub directory each
    #file came from
    filelist = list.files(dir, full.names = FALSE, recursive = recursive)
    filelist = grep(.zarr_fits_pattern, filelist, value = TRUE)
    if(length(pattern) > 0){
      for(p in pattern){
        filelist = grep(p, filelist, value = TRUE)
      }
    }
    filelist = sort(unique(filelist))
    if(length(filelist) == 0){
      stop('No FITS files found in ', dir, if(recursive) '' else ' (non recursive)',
           if(length(pattern) > 0) ' with the given pattern' else '', '!', call. = FALSE)
    }
    fullnames = file.path(dir, filelist)
  }else{
    fullnames = path.expand(filelist)
    fullnames = grep(.zarr_fits_pattern, fullnames, value = TRUE)
    if(length(pattern) > 0){
      for(p in pattern){
        fullnames = grep(p, fullnames, value = TRUE)
      }
    }
    filelist = fullnames
    if(length(fullnames) == 0){
      stop('No FITS files in filelist!', call. = FALSE)
    }
  }

  #A store name has to identify one file. Two images of the same name in different
  #sub directories would otherwise silently write over each other, and the second
  #would look like a success, so this is stopped here rather than half way through
  stubs = vapply(fullnames, .zarr_store_stub, character(1))
  dup = duplicated(stubs) | duplicated(stubs, fromLast = TRUE)
  if(any(dup)){
    stop('These FITS files would share a Zarr store name: ',
         paste(paste0(filelist[dup], ' -> ', stubs[dup]), collapse = ', '),
         '. Rename them, or run one directory at a time.', call. = FALSE)
  }

  #Bad credentials or an unwritable directory are worth failing on before any
  #transfer has been paid for
  args = list(...)
  if(remote){
    if(is.null(access_key) | is.null(secret_key)){
      stop('access_key and secret_key are required for a remote batch, whether given ',
           'directly or by RFITS_S3_ACCESS_KEY and RFITS_S3_SECRET_KEY.', call. = FALSE)
    }
  }else{
    target = path.expand(target)
    if(!dir.exists(target)){
      created = dir.create(target, recursive = TRUE, showWarnings = FALSE)
      if(!created | !dir.exists(target)){
        stop('Cannot create the output directory: ', target, call. = FALSE)
      }
    }
    assertAccess(target, access = 'w')
  }

  Nfile = length(fullnames)
  output = data.frame(fits = fullnames, store = rep(NA_character_, Nfile),
                      extname = rep(extname, Nfile), dim = rep(NA_character_, Nfile),
                      data_type = rep(NA_character_, Nfile), nkey = rep(NA_integer_, Nfile),
                      status = rep(NA_character_, Nfile), error = rep(NA_character_, Nfile),
                      stringsAsFactors = FALSE)

  #The per-file index is supplied by the caller, and deliberately not given a value
  #here: an entry in this list would collide with the one added by do.call
  batch = c(list(fullnames = fullnames, filelist = filelist, stubs = stubs,
                 target = target, prefix = prefix, bucket = bucket, region = region,
                 endpoint = endpoint, access_key = access_key, secret_key = secret_key,
                 session_token = session_token, ext = ext, extname = extname,
                 args = args, verbose = verbose))

  if(is.null(cores)){
    rows = lapply(seq_len(Nfile), function(i){
      return(do.call(.zarr_convert_one, c(list(i = i), batch)))
    })
  }else{
    #A cluster owned by this call rather than registerDoParallel plus %dopar%, because
    #that pair changes a backend shared by the whole session. Registering here would
    #take over a cluster the caller had made, and the one made here would stay
    #registered after this call returned, so their next %dopar% would fail with a
    #message about dead workers that points nowhere near here. PSOCK on every platform,
    #since forking with cfitsio and OpenMP state in the parent is not safe.
    #More workers than files only pays for idle processes, hence the cap.
    #
    #outfile = '' is what makes the per-file progress visible: without it a message
    #raised in a worker is swallowed, and a batch that takes an hour would report
    #nothing at all until it finished. The cost is one 'starting worker' line each.
    cluster = parallel::makeCluster(min(cores, Nfile), type = 'PSOCK', outfile = '')
    #The cluster is stopped on the way out even if a worker dies, since idle workers
    #would each hold the memory of a whole image until the session ended
    on.exit(parallel::stopCluster(cluster), add = TRUE)
    #Workers start empty, so the package has to be loaded there before the helper can be
    #found. zarr is not attached: the reader and the writer reach it through zarr::, and
    #attaching a package from Suggests is what R CMD check objects to
    parallel::clusterEvalQ(cluster, {
      suppressPackageStartupMessages(library(Rfits))
      return(NULL)
    })
    #Each file is read, compressed and written by one worker, so the parallelism is
    #embarrassing: no worker touches a store another worker has opened
    rows = parallel::parLapply(cluster, seq_len(Nfile), function(i){
      #Wrapped again because the helper catches what it expects to catch, and anything
      #else should still cost one row rather than the whole batch
      tryCatch(do.call(.zarr_convert_one, c(list(i = i), batch)),
               error = function(e){
                 return(list(store = NA_character_, dim = NA_character_,
                             data_type = NA_character_, nkey = NA_integer_,
                             status = 'error',
                             error = paste0('worker failed: ', conditionMessage(e))))
               })
    })
  }

  n_ok = 0L
  n_fail = 0L
  for(i in seq_len(Nfile)){
    row = rows[[i]]
    output$store[i] = row$store
    output$dim[i] = row$dim
    output$data_type[i] = row$data_type
    output$nkey[i] = row$nkey
    output$status[i] = row$status
    output$error[i] = row$error
    if(identical(row$status, 'ok')){
      n_ok = n_ok + 1L
    }else{
      n_fail = n_fail + 1L
    }
  }

  if(verbose){
    message('Wrote ', n_ok, ' of ', Nfile, ' FITS files to Zarr',
            if(n_fail > 0) paste0(', ', n_fail, ' failed') else '')
  }

  return(invisible(output))
}

#Summarise a Zarr store without reading any pixel data. Nothing here is derived
#from the data itself, since for a remote store that would transfer the whole
#array; the numbers come from the metadata and the key sizes on disk.
Rfits_inspect_zarr = function(filename, print = TRUE, ...){
  .zarr_require()
  assertFlag(print)

  is_store = .zarr_is_store(filename)
  if(is_store){
    label = .zarr_store_label(filename)
  }else{
    assertCharacter(filename, max.len=1)
    label = path.expand(filename)
  }
  store = .zarr_store_open(if(is_store) filename else label)

  extnames = .zarr_extnames(store)
  root = .zarr_root_attrs(store)
  self_describing = !is.null(root) && !is.null(root$convention)
  #The format version sits on whatever metadata the store does have, so fall back
  #to the root when the store holds no arrays yet
  zarr_format = tryCatch(store$root$metadata$zarr_format, error = function(e) NULL)

  arrays = list()
  #Doubles, so that summing counts cannot overflow on the way to the total
  total_elements = 0
  total_chunks = 0
  for(name in extnames){
    node = tryCatch(store$get_node(.zarr_name_to_path(name)), error = function(e) NULL)
    if(is.null(node)){
      next
    }
    tech = .zarr_array_tech(node)

    nchunks = tryCatch(length(store$store$list_chunks(node$prefix)), error = function(e) NA_integer_)
    #Chunk keys are the data; zarr.json holds the metadata
    keys = tryCatch(store$store$list_dir(node$prefix), error = function(e) character(0))
    chunk_keys = setdiff(keys, c('zarr.json', '.zarray', '.zattrs'))
    sizes = tryCatch(vapply(chunk_keys, function(k){
      gz = tryCatch(store$store$getsize(paste0(node$prefix, k)), error = function(e) NA_real_)
      if(is.null(gz) | length(gz) != 1){return(NA_real_)}
      return(as.numeric(gz))
    }, numeric(1)), error = function(e) NA_real_)
    chunk_bytes = if(all(is.na(sizes))) NA_real_ else sum(sizes, na.rm = TRUE)

    meta = tryCatch(.zarr_get_meta(node), error = function(e) NULL)
    keynames = if(is.null(meta)) NULL else meta$keynames

    arrays[[name]] = list(
      name = name,
      shape = tech$shape,
      image_shape = tech$image_shape,
      data_type = tech$data_type,
      chunk_shape = tech$chunk_shape,
      codec = tech$codec,
      compressor = tech$compressor,
      compression_level = tech$clevel,
      shuffle = tech$shuffle,
      fill_value = tech$fill_value,
      lossy = tech$lossy,
      n_chunks = nchunks,
      chunk_bytes = chunk_bytes,
      avg_chunk_bytes = if(is.na(chunk_bytes) | is.na(nchunks) | nchunks == 0){
        NA_real_
      }else{
        chunk_bytes / nchunks
      },
      elements = .zarr_count(.zarr_element_count(tech$shape)),
      fits_header = tech$fits_header,
      key_count = tech$key_count,
      first_key = if(is.null(keynames) | length(keynames) == 0) NA_character_ else keynames[1],
      last_key = if(is.null(keynames) | length(keynames) == 0) NA_character_ else
        keynames[length(keynames)],
      has_wcs = !is.null(keynames) & any(grepl('^CRVAL', keynames))
    )
    total_elements = total_elements + .zarr_element_count(tech$shape)
    if(!is.na(nchunks)){
      total_chunks = total_chunks + nchunks
    }
  }

  #Size on disk is only meaningful for a store that is a directory we can see
  store_bytes = NA_real_
  if(!is_store){
    if(dir.exists(label)){
      files = list.files(label, recursive = TRUE, full.names = TRUE, all.files = TRUE,
                         include.dirs = TRUE)
      info = file.info(files)
      store_bytes = sum(info$size[!info$isdir], na.rm = TRUE)
    }
  }

  out = list(filename = label,
             is_store_object = is_store,
             zarr_format = if(is.null(zarr_format)) NA_integer_ else as.integer(zarr_format),
             zarr_version = tryCatch(as.character(utils::packageVersion('zarr')),
                                     error = function(e) NA_character_),
             Rfits_version = tryCatch(as.character(utils::packageVersion('Rfits')),
                                      error = function(e) NA_character_),
             self_describing = self_describing,
             convention = if(is.null(root)) NA_character_ else root$convention,
             created = if(is.null(root)) NA_character_ else root$created,
             n_arrays = length(arrays),
             array_names = names(arrays),
             total_elements = .zarr_count(total_elements),
             total_chunks = .zarr_count(total_chunks),
             store_bytes = store_bytes,
             append_history = if(is.null(root)) NULL else root$append_history,
             arrays = arrays)

  if(print){
    rule = strrep('=', 80)
    cat(rule, '\n')
    cat('SUMMARY STATISTICS\n')
    cat(rule, '\n')
    cat(sprintf('store path:            %s\n', out$filename))
    if(is_store){
      cat('store type:            object (remote or in-memory; no local path)\n')
    }
    cat(sprintf('zarr format:           %s (zarr package %s)\n',
                out$zarr_format, out$zarr_version))
    cat(sprintf('Rfits version:         %s\n', out$Rfits_version))
    cat(sprintf('store self-description: %s\n',
                if(self_describing) paste0('present (convention "', out$convention,
                                           '", created ', out$created, ')')
                else 'absent (written by an older Rfits, or not by Rfits at all)'))
    if(!self_describing & .zarr_is_foreign_store(store)){
      cat('  note: the root advertises the images to Zarr keys without a convention\n')
      cat('  marker, so this store was probably not written by Rfits and holds no\n')
      cat('  FITS headers.\n')
    }
    cat(sprintf('arrays:                %s\n', .zarr_count_text(out$n_arrays)))
    cat(sprintf('total elements:        %s\n', .zarr_count_text(out$total_elements)))
    cat(sprintf('total chunks:          %s\n', .zarr_count_text(out$total_chunks)))
    if(is.na(out$store_bytes)){
      cat('store size (bytes):    NA (not a local directory)\n')
    }else{
      cat(sprintf('store size (bytes):    %.0f\n', out$store_bytes))
    }
    if(!is.null(out$append_history)){
      cat(sprintf('append events:         %i\n', length(out$append_history)))
    }

    if(length(arrays) > 0){
      cat(rule, '\n')
      cat('ARRAYS\n')
      cat(rule, '\n')
      for(name in names(arrays)){
        a = arrays[[name]]
        cat(sprintf('\n%s\n', name))
        cat(sprintf('  shape:               %s\n', paste(a$shape, collapse = ' x ')))
        cat(sprintf('  data type:           %s\n', a$data_type))
        cat(sprintf('  chunks:              %s (%s total)\n',
                    paste(a$chunk_shape, collapse = ' x '),
                    if(is.na(a$n_chunks)) 'unknown' else a$n_chunks))
        if(nzchar(a$codec)){
          cat(sprintf('  compression:         %s/%s level %s shuffle %s\n',
                      a$codec, a$compressor, a$compression_level, a$shuffle))
        }else{
          cat('  compression:         none recorded\n')
        }
        cat(sprintf('  fill value:          %s\n', a$fill_value))
        cat(sprintf('  lossy scaling:       %s\n', if(isTRUE(a$lossy)){
          'yes (BSCALE and BZERO in the header)'
        }else{
          'no'
        }))
        if(!is.na(a$chunk_bytes)){
          cat(sprintf('  chunk bytes:         %.0f (avg %.0f)\n', a$chunk_bytes,
                      a$avg_chunk_bytes))
        }
        cat(sprintf('  FITS header:         %s\n', if(a$fits_header) 'yes' else 'no'))
        cat(sprintf('  keywords:            %i\n', a$key_count))
        if(a$key_count > 0){
          cat(sprintf('  first/last keyword:  %s / %s\n', a$first_key, a$last_key))
          cat(sprintf('  WCS keys present:    %s\n', if(a$has_wcs) 'yes' else 'no'))
        }
      }
    }
    cat(rule, '\n')
  }

  return(invisible(out))
}

Rfits_point_zarr = function(filename='temp.zarr', extname='data1', ext=NULL, header=TRUE){
  .zarr_require()

  if(.zarr_is_store(filename)){
    store = .zarr_store_open(filename)
    filename = .zarr_store_label(filename)
  }else{
    assertCharacter(filename, max.len=1)
    filename = path.expand(filename)
    store = .zarr_store_open(filename)
  }
  assertCharacter(extname, max.len=1)
  assertFlag(header)

  extnames = .zarr_extnames(store)

  if(is.null(ext)){
    ext = which(extnames == sub('^/', '', extname))
    if(length(ext) == 0){
      stop('Extension "', extname, '" does not exist in the Zarr store!')
    }
    ext = ext[1]
  }else{
    assertIntegerish(ext, len=1)
    if(ext < 1 | ext > length(extnames)){
      stop('Extension ', ext, ' does not exist in the Zarr store!')
    }
    ext = as.integer(ext)
  }

  extname = extnames[ext]
  node = store$get_node(.zarr_name_to_path(extname))
  dim = as.integer(node$shape)
  Ndim = length(dim)

  type = c('vector', 'image', 'cube', 'array')[Ndim]
  if(length(type) == 0 | is.na(type)){
    stop('Only 1-4 dimensional data can be pointed at as a FITS style image!')
  }

  meta = .zarr_get_meta(node)

  output = list(filename=filename, extname=extname, ext=ext, header=header,
                keyvalues=meta$keyvalues, dim=dim, type=type, store=store)
  class(output) = 'Rfits_pointer_zarr'
  return(invisible(output))
}

#What to hand to .zarr_store_open to re-read a pointer. A pointer made from a
#store object cannot be rebuilt from its filename, which is only a label.
#
#A lazy pointer (one made by .zarr_lazy_pointer) carries no store, only the spec
#needed to build one, held in the environment at x$store. It is opened here, on
#first use, and the handle memoised in that same environment. Pointers are plain
#lists, so x$store = <handle> inside a method would not persist to the caller's
#copy; writing into a shared environment does, so a store is opened at most once
#however many times the pointer is sliced. This is what lets a search that only
#lists its matches cost no store opens at all.
.zarr_pointer_source = function(x){
  store = x$store
  if(inherits(store, 'Rfits_lazy_store')){
    handle = get0('store', envir = store, inherits = FALSE)
    if(is.null(handle)){
      handle = .zarr_lazy_open(store)
      assign('store', handle, envir = store)
    }
    return(handle)
  }
  if(!is.null(store)){
    return(store)
  }
  return(path.expand(x$filename))
}

#Open the store a lazy pointer was built for, and return it already opened as a zarr
#object rather than as a bare store. Kept apart from the resolver above so that a
#pointer with nothing to open says so, rather than failing inside the reader.
#
#The opened object is what must be memoised, because it holds the chunk cache and is
#the form .zarr_store_open() passes straight through. Handing back a store (or a
#directory path) instead would leave the reader to open it on every slice, which over
#S3 costs a hierarchy walk per slice and throws away the chunks just fetched, so
#overlapping cutouts never got cheaper. This is also what an eager Rfits_point_zarr
#pointer holds, so the two paths now keep the same thing.
.zarr_lazy_open = function(env){
  spec = get0('openspec', envir = env, inherits = FALSE)
  if(is.null(spec)){
    stop('This Rfits_pointer_zarr has no store to open!', call. = FALSE)
  }
  if(isTRUE(spec$remote)){
    return(.zarr_store_open(.zarr_s3_store_for(bucket = spec$bucket, prefix = spec$prefix,
                                               region = spec$region,
                                               endpoint = spec$endpoint,
                                               access_key = spec$access_key,
                                               secret_key = spec$secret_key,
                                               session_token = spec$session_token)))
  }
  return(.zarr_store_open(spec$dir))
}

#Build a pointer from metadata already in hand, without opening the store. The
#fields mirror what Rfits_point_zarr puts in a live pointer, so the two are
#indistinguishable until the lazy one is sliced.
#
#ext is left NULL and resolved by the reader on open, because knowing the index
#means listing the store, which is the read being avoided; extname is enough to
#slice by, and resolves to the same extension in any store that has it. The
#keyvalues must be the full header rather than the two axis trimmed form the search
#uses, or a cube silently loses its third axis.
.zarr_lazy_pointer = function(filename, extname, keyvalues, dim, type, header = TRUE,
                              openspec){
  output = list(filename = filename, extname = extname, ext = NULL, header = header,
                keyvalues = keyvalues, dim = as.integer(dim), type = type,
                store = new.env(parent = emptyenv()))
  #The class tells a lazy handle slot from a real store object, since both are
  #environments
  class(output$store) = 'Rfits_lazy_store'
  assign('openspec', openspec, envir = output$store)
  class(output) = 'Rfits_pointer_zarr'
  return(output)
}

#Drop trailing dimensions. An Rfits object goes through its own [ method so the
#header keywords are rebuilt for the smaller array, while the plain array that
#header=FALSE returns has no method and is just re-dimensioned
.zarr_collapse = function(data, ndim, header){
  if(header){
    if(ndim == 2L & length(dim(data)) == 4L){
      return(data[,,1,1, collapse=TRUE])
    }
    if(ndim == 2L){
      return(data[,,1, collapse=TRUE])
    }
    return(data[,,,1, collapse=TRUE])
  }
  dim(data) = dim(data)[seq_len(ndim)]
  return(data)
}

#Subsetting mirrors [.Rfits_image (and so [.Rfits_pointer), so that box cutouts,
#RA/Dec positions and the i=c(x,y) shorthand all behave the same whichever
#back-end the pointer was made from. What gets read is still only the requested
#region, which is the point of a pointer.
`[.Rfits_pointer_zarr` = function(x, i=NULL, j=NULL, k=NULL, m=NULL, box=201, type='pix',
                                  header=x$header, physical=TRUE, collapse=TRUE){
  assertChoice(type, c('pix', 'coord'))
  assertFlag(header)
  assertFlag(physical)
  assertFlag(collapse)

  dim_x = x$dim
  Ndim = length(dim_x)

  #`a:end` means "from a to the end of that dimension". This has to be resolved
  #before anything else touches i, j, k or m, because they are promises here and
  #the bare name `end` is stats::end, so merely asking is.null(i) would try to
  #evaluate `50:end` and fail. Expanding it to the two bounds also avoids the
  #vector as long as the image that spelling out the whole range would allocate.
  #The flag records that i came from a range expression, since the length-2 rule
  #below would otherwise read c(a, dim) as a centre to put a box around.
  i_range = FALSE
  if(!missing(i)){
    i_res = .resolve_a_to_end(substitute(i), dim_x[1], parent.frame())
    if(!is.null(i_res)){
      i = i_res
      i_range = TRUE
    }
  }
  if(!missing(j) && Ndim >= 2L){
    j_res = .resolve_a_to_end(substitute(j), dim_x[2], parent.frame())
    if(!is.null(j_res)){j = j_res}
  }
  if(!missing(k) && Ndim >= 3L){
    k_res = .resolve_a_to_end(substitute(k), dim_x[3], parent.frame())
    if(!is.null(k_res)){k = k_res}
  }
  if(!missing(m) && Ndim >= 4L){
    m_res = .resolve_a_to_end(substitute(m), dim_x[4], parent.frame())
    if(!is.null(m_res)){m = m_res}
  }

  #Random access by row matrix is a FITS pointer feature (cfitsio reads single
  #pixels), and the Zarr reader has no equivalent, so reject it rather than let it
  #fall through and return the wrong shape
  if(is.matrix(i) | is.matrix(j) | is.matrix(k) | is.matrix(m)){
    stop('Matrix indexing (e.g. p[cbind(x, y)]) is not supported for Zarr pointers!',
         call. = FALSE)
  }

  #A box with no location given is cut out around the centre of the image. Box
  #cutouts are a two dimensional feature, so higher dimensions are left alone
  if(!missing(box) & Ndim == 2L & is.null(i) & is.null(j)){
    i = ceiling(dim_x[1]/2)
    j = ceiling(dim_x[2]/2)
  }

  #Two values given for i alone are a location, not a range, so that
  #p[c(50,150)] centres a box there rather than cutting out 50:150. Adjacent
  #values really are a range, and so is anything that came from `a:end`. This is
  #only for arrays that can hold a box at all: a 1D pointer keeps range meaning
  #for c(a,b), as [.Rfits_vector does
  if(Ndim >= 2L & !is.null(i) & is.null(j) & !i_range){
    if(length(i) == 2L){
      if(i[2] - i[1] != 1){
        j = ceiling(i[2])
        i = ceiling(i[1])
      }
    }
  }

  if(type == 'coord'){
    if(requireNamespace("Rwcs", quietly=TRUE)){
      if(is.null(x$keyvalues)){
        stop('No FITS style metadata is stored for this Zarr array, so type = "coord" ',
             'cannot be used!', call. = FALSE)
      }
      assertNumeric(i, len=1)
      assertNumeric(j, len=1)
      #The pointer keeps no raw header, so rebuild the fixed width form from the
      #keywords. Rwcs uses it for the full distortion terms when it is present
      ij = Rwcs::Rwcs_s2p(i, j, keyvalues=x$keyvalues, pixcen='R',
                          header=Rfits_keyvalues_to_raw(x$keyvalues))[1,]
      i = ceiling(ij[1])
      j = ceiling(ij[2])
    }else{
      message('The Rwcs package is needed to use type=coord.')
    }
  }

  #A box is only applied to a single location, and only in two dimensions
  if(Ndim == 2L & !is.null(i) & !is.null(j)){
    if(length(i) == 1 & length(j) == 1){
      if(length(box) == 1){box = c(box, box)}
      i = ceiling(i + c(-(box[1]-1L)/2, (box[1]-1L)/2))
      j = ceiling(j + c(-(box[2]-1L)/2, (box[2]-1L)/2))
    }
  }

  #The FITS pointer reports too many dimensions as a NULL NAXIS keyword, but a
  #Zarr array may carry no FITS metadata at all, so go by the stored shape
  if(Ndim < 2L & !is.null(j)){stop('The Zarr array is 1 dimensional: specifying too many dimensions!')}
  if(Ndim < 3L & !is.null(k)){stop('The Zarr array has no third dimension: specifying too many dimensions!')}
  if(Ndim < 4L & !is.null(m)){stop('The Zarr array has no fourth dimension: specifying too many dimensions!')}

  if(!is.null(i)){
    xlo = ceiling(min(i))
    xhi = ceiling(max(i))
  }else{
    xlo = NULL
    xhi = NULL
  }
  if(!is.null(j)){
    ylo = ceiling(min(j))
    yhi = ceiling(max(j))
  }else{
    ylo = NULL
    yhi = NULL
  }
  if(!is.null(k)){
    zlo = ceiling(min(k))
    zhi = ceiling(max(k))
  }else{
    zlo = NULL
    zhi = NULL
  }
  if(!is.null(m)){
    tlo = ceiling(min(m))
    thi = ceiling(max(m))
  }else{
    tlo = NULL
    thi = NULL
  }

  #Collapsing is done here rather than by the reader, since only a dimension the
  #caller actually sliced may be dropped
  data = Rfits_read_image_zarr(.zarr_pointer_source(x), extname=x$extname, ext=x$ext,
                               xlo=xlo, xhi=xhi, ylo=ylo, yhi=yhi, zlo=zlo, zhi=zhi,
                               tlo=tlo, thi=thi, header=header, physical=physical,
                               collapse=FALSE)

  if(collapse){
    if(length(dim(data)) == 3L){
      if(dim(data)[3L] == 1L & !is.null(k)){
        data = .zarr_collapse(data, 2L, header=header)
      }
    }else if(length(dim(data)) == 4L){
      if(dim(data)[3L] == 1L & dim(data)[4L] == 1L & !is.null(k) & !is.null(m)){
        data = .zarr_collapse(data, 2L, header=header)
      }else if(dim(data)[4L] == 1L & !is.null(m)){
        data = .zarr_collapse(data, 3L, header=header)
      }
    }
  }

  return(data)
}

length.Rfits_pointer_zarr = function(x){
  return(prod(x$dim))
}

dim.Rfits_pointer_zarr = function(x){
  .zarr_require()
  store = .zarr_store_open(.zarr_pointer_source(x))
  node = store$get_node(.zarr_name_to_path(x$extname))
  return(as.integer(node$shape))
}

print.Rfits_pointer_zarr = function(x, ...){
  cat('File path:', x$filename, '\n')
  #A lazy pointer has no extension index until it is first sliced, and printing a
  #bare NULL there reads like a missing value rather than a deferred one
  cat('Ext num:', if(is.null(x$ext)) 'not read yet' else x$ext, '\n')
  cat('Ext name:', x$extname, '\n')
  cat('Class: Rfits_pointer_zarr\n')
  cat('Type:', x$type, '\n')
  cat('Dim:', x$dim, '\n')
  cat('Key N:', length(x$keyvalues), '\n')
}
