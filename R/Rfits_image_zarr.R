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
                                 collapse=FALSE){
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
                    'compression_level', 'shuffle', 'fill_value', 'fits_header',
                    'key_count', 'supported_extensions', 'creation_info',
                    'append_history')

.zarr_array_keys = c('data_type', 'chunk_shape', 'codec', 'compressor',
                     'compression_level', 'shuffle', 'fill_value')

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
.zarr_array_tech = function(node){
  shape = as.vector(as.integer(node$shape))
  codec = .zarr_codec_of(node)
  meta = tryCatch(node$metadata, error = function(e) NULL)
  data_type = if(is.null(meta)) '' else as.character(meta$data_type)
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
              fill_value = tech$fill_value)
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

#Grow an existing array along dimension 1 and write the new elements into the
#space. The data is the increment, not the complete final array: an array that is
#currently 4 x 6 grows to 7 x 6 when given 3 x 6. Dimension 1 is the slowest
#varying axis in FITS, R and Zarr alike, so the existing elements keep their
#indices. low is never touched in the resize, because shrinking is out of scope
#and zarr warns about chunk rounding for a non zero low.
.zarr_append = function(store, extname, data, shape, typed, data_type_requested,
                        codec_config, compressor_requested, update_root, filename_out){
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
                        end_index = end_index)
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
.zarr_append_header = function(node, meta, existing_type, new_shape, grew, start_index,
                               end_index){
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

  stamp = format(Sys.time(), '%Y-%m-%dT%H:%M:%SZ', tz = 'UTC')
  line = paste('Rfits appended', grew, 'elements to dimension 1 at',
               paste0('[', start_index, ',', end_index, ']'), 'on', stamp)
  history = c(history, line)

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

Rfits_write_image_zarr = function(data, filename='temp.zarr', extname='data1', create_ext=TRUE,
                                  overwrite_file=FALSE, data_type=NULL, chunk_shape=NULL,
                                  clevel=6L, compressor='blosc', shuffle=NULL,
                                  append=FALSE, update_root=TRUE,
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
  assertIntegerish(clevel, len=1, lower=0, upper=9)

  if(append & overwrite_file){
    stop('Cannot use both append and overwrite_file!', call. = FALSE)
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

  typed = .zarr_data_type(data, data_type=data_type)
  data = typed$data

  path = .zarr_name_to_path(extname)
  exists_already = path %in% store$arrays

  #Appending grows the array that is already there, so it has to branch before
  #anything is deleted or created
  if(append){
    return(invisible(.zarr_append(store = store, extname = extname, data = data,
                                  shape = shape, typed = typed,
                                  data_type_requested = data_type,
                                  codec_config = codec_config,
                                  compressor_requested = compressor_requested,
                                  update_root = update_root,
                                  filename_out = filename_out)))
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

  node$write(data)

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
  #what was asked for, so a clamped or defaulted value cannot be misreported
  tech = .zarr_array_tech(node)
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
                        compression_level = if(is.null(applied)) codec_config$clevel else applied$clevel)))
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
.zarr_pointer_source = function(x){
  if(!is.null(x$store)){
    return(x$store)
  }
  return(path.expand(x$filename))
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
                                  header=x$header, collapse=TRUE){
  assertChoice(type, c('pix', 'coord'))
  assertFlag(header)
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
                               tlo=tlo, thi=thi, header=header, collapse=FALSE)

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
  cat('Ext num:', x$ext, '\n')
  cat('Ext name:', x$extname, '\n')
  cat('Class: Rfits_pointer_zarr\n')
  cat('Type:', x$type, '\n')
  cat('Dim:', x$dim, '\n')
  cat('Key N:', length(x$keyvalues), '\n')
}
