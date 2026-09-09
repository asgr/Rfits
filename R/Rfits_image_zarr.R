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
        warning('No FITS style metadata found for extension: ', extname)
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

Rfits_write_image_zarr = function(data, filename='temp.zarr', extname='data1', create_ext=TRUE,
                                  overwrite_file=FALSE, data_type=NULL, chunk_shape=NULL,
                                  clevel=6L, keyvalues, keycomments, keynames, comment, history){
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
  assertIntegerish(clevel, len=1, lower=0, upper=9)

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

  typed = .zarr_data_type(data, data_type=data_type)
  data = typed$data

  path = .zarr_name_to_path(extname)
  if(path %in% store$arrays){
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
  builder$add_codec('blosc', list(clevel = as.integer(clevel)))

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

  node$save()

  return(invisible(list(filename = if(filename_is_store) store_label else filename,
                        extname = extname,
                        dim = shape, data_type = typed$data_type,
                        chunk_shape = chunk_shape)))
}

Rfits_write_vector_zarr = Rfits_write_image_zarr
Rfits_write_cube_zarr = Rfits_write_image_zarr
Rfits_write_array_zarr = Rfits_write_image_zarr

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
    express = as.character(substitute(i))
    if(length(express) == 3L && express[1] == ':' && grepl('end', express[3]) && Ndim >= 1L){
      i = c(as.numeric(express[2]), dim_x[1])
      i_range = TRUE
    }
  }
  if(!missing(j) && Ndim >= 2L){
    express = as.character(substitute(j))
    if(length(express) == 3L && express[1] == ':' && grepl('end', express[3])){
      j = c(as.numeric(express[2]), dim_x[2])
    }
  }
  if(!missing(k) && Ndim >= 3L){
    express = as.character(substitute(k))
    if(length(express) == 3L && express[1] == ':' && grepl('end', express[3])){
      k = c(as.numeric(express[2]), dim_x[3])
    }
  }
  if(!missing(m) && Ndim >= 4L){
    express = as.character(substitute(m))
    if(length(express) == 3L && express[1] == ':' && grepl('end', express[3])){
      m = c(as.numeric(express[2]), dim_x[4])
    }
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
