#Searching a directory (or an S3 prefix) of Zarr stores for the ones that overlap a
#requested position, and cutting a box out of each of them.
#
#The point of this is to find the right stores without reading any pixels. A store
#made by Rfits_dir_to_zarr keeps its WCS in a single small zarr.json beside the array
#data, so deciding whether a store can hold the request costs one tiny metadata read.
#Only the stores that survive that check are opened, and only the requested box is
#then sliced, by the usual [.Rfits_pointer_zarr method. That matters most over S3,
#where the alternative is to open every store under the prefix.
#
#The search itself is two stage. A cone style test in plain trigonometry rejects the
#tiles that are nowhere near the position, using only the reference point, the pixel
#scale and the array shape from the header. The boundary test (Rwcs_overlap, which
#projects the box edge into the tile frame and, if that finds nothing, the tile edge
#into the box frame) is then run only on the survivors, because it costs a pair of
#wcslib calls per tile.

#Credentials and endpoints come from the environment the same way the batch writer
#reads them, so a scripted run does not need them in the script. bucket is never
#taken from the environment: passing it explicitly is what asks for a remote run.
.zarr_env_default = function(value, name){
  if(!is.null(value)){
    return(value)
  }
  value = Sys.getenv(name, '')
  if(nzchar(value)){
    return(value)
  }
  return(NULL)
}

.zarr_s3_env_defaults = function(region = NULL, endpoint = NULL, access_key = NULL,
                                 secret_key = NULL, session_token = NULL){
  return(list(region = .zarr_env_default(region, 'RFITS_S3_REGION'),
              endpoint = .zarr_env_default(endpoint, 'RFITS_S3_ENDPOINT'),
              access_key = .zarr_env_default(access_key, 'RFITS_S3_ACCESS_KEY'),
              secret_key = .zarr_env_default(secret_key, 'RFITS_S3_SECRET_KEY'),
              session_token = .zarr_env_default(session_token, 'RFITS_S3_SESSION_TOKEN')))
}

#The metadata key of an array within a store. Zarr v3 (what Rfits writes) keeps it at
#<extname>/zarr.json, with the store root at zarr.json.
.zarr_meta_key = function(extname){
  return(paste0(sub('^/', '', extname), '/zarr.json'))
}

#Read the shape and FITS keywords out of the bytes of a zarr.json, without opening the
#store as a Zarr object at all. The card images are the authoritative copy of the
#metadata and the keyword list only a convenience duplicate, so header is preferred
#and keyvalues used only when there are no cards. Returns NULL if there is nothing
#usable, which the caller treats as a store it cannot search.
.zarr_meta_from_json = function(bytes){
  if(!requireNamespace('jsonlite', quietly = TRUE)){
    return(NULL)
  }
  meta = tryCatch(jsonlite::fromJSON(rawToChar(bytes), simplifyVector = FALSE),
                  error = function(e) NULL)
  if(is.null(meta) || !is.list(meta)){
    return(NULL)
  }
  shape = meta$shape
  if(is.null(shape)){
    shape = NA_integer_
  }else{
    shape = as.integer(unlist(shape))
  }
  keyvalues = NULL
  att = meta$attributes
  if(!is.null(att)){
    if(!is.null(att$header)){
      keyvalues = tryCatch(Rfits_header_to_keyvalues(as.character(unlist(att$header))),
                           error = function(e) NULL)
    }
    if(is.null(keyvalues)){
      keyvalues = .zarr_json_to_list(att$keyvalues)
    }
  }
  return(list(shape = shape, keyvalues = keyvalues))
}

#As .zarr_meta_from_json, for a store on disk. NULL means the metadata file is not
#there or could not be read, which sends the caller to the slower but more tolerant
#path of opening the store properly.
.zarr_meta_from_dir = function(store, extname){
  path = file.path(store, .zarr_meta_key(extname))
  info = file.info(path)
  if(is.na(info$size) || isTRUE(info$isdir) || info$size <= 0){
    return(NULL)
  }
  bytes = tryCatch(readBin(path, what = 'raw', n = as.integer(info$size)),
                   error = function(e) NULL)
  if(is.null(bytes) || length(bytes) == 0){
    return(NULL)
  }
  return(.zarr_meta_from_json(bytes))
}

#As .zarr_meta_from_json, for a store over S3. This is one small GET. A missing key is
#NULL, since a prefix may hold things that are not stores; any other failure comes back
#as an error string instead, because a store we are not allowed to see must not be
#reported as a store that does not exist.
.zarr_meta_from_s3 = function(client, bucket, store_prefix, extname){
  key = paste0(.zarr_s3_prefix(store_prefix), .zarr_meta_key(extname))
  got = tryCatch(client$get_object(Bucket = bucket, Key = key), error = function(e) e)
  if(inherits(got, 'error')){
    if(.zarr_s3_not_found(conditionMessage(got))){
      return(NULL)
    }
    out = list()
    attr(out, 'error') = conditionMessage(got)
    return(out)
  }
  body = got$Body
  if(is.character(body)){
    body = charToRaw(body)
  }
  if(is.raw(body) && length(body) > 0){
    meta = .zarr_meta_from_json(body)
    if(!is.null(meta)){
      return(meta)
    }
    out = list()
    attr(out, 'error') = 'Zarr array metadata could not be parsed'
    return(out)
  }
  return(NULL)
}

#The immediate sub prefixes of an S3 prefix, in one request per page. A directory of a
#thousand stores is a thousand common prefixes, so this is one request, not a listing
#of every chunk key.
.zarr_s3_list_prefixes = function(client, bucket, prefix){
  out = character(0)
  token = NULL
  repeat{
    res = if(is.null(token)){
      client$list_objects_v2(Bucket = bucket, Prefix = prefix, Delimiter = '/',
                             MaxKeys = 1000)
    }else{
      client$list_objects_v2(Bucket = bucket, Prefix = prefix, Delimiter = '/',
                             MaxKeys = 1000, ContinuationToken = token)
    }
    found = res$CommonPrefixes
    if(length(found) > 0){
      out = c(out, vapply(found, function(p){
        if(is.null(p$Prefix)){
          NA_character_
        }else{
          as.character(p$Prefix)
        }
      }, character(1)))
    }
    if(!isTRUE(res$IsTruncated) || is.null(res$NextContinuationToken)){
      break
    }
    token = res$NextContinuationToken
  }
  out = out[!is.na(out)]
  return(unique(out))
}

#Is the prefix itself a store? The root group document is the only thing that makes a
#prefix a store, and it is one small key, so this is worth asking before listing. The
#tri state probe is deliberately not used as a veto: a prefix that cannot be probed is
#simply not the store we are looking for, and the listing will report what is there.
.zarr_s3_prefix_has_store = function(client, bucket, prefix){
  for(root in c('zarr.json', '.zgroup')){
    #v3 then v2, the latter read but never written by Rfits
    if(isTRUE(.zarr_s3_probe(client, bucket, paste0(prefix, root))$exists)){
      return(TRUE)
    }
  }
  return(FALSE)
}

#The store prefixes under a directory prefix. Breadth first over directories: one
#request per directory whatever it holds, which is the remote equivalent of
#list.files(recursive = TRUE). Only names ending in .zarr are taken as stores, and a
#store is never descended into. Stopping at the first directory level would be one
#request cheaper, but it would find a different set of stores from the local search on
#the same mirrored layout, and a search whose answer depends on which back end the data
#is sitting on is not worth having. max_dirs bounds the walk, since a bucket prefix
#pointed at the head of an unrelated tree would otherwise be paid for per directory.
.zarr_s3_find_stores = function(client, bucket, prefix = '', recursive = TRUE,
                                pattern = NULL, max_dirs = 1000){
  base = .zarr_s3_prefix(prefix)

  #The prefix may itself be the store rather than a directory of them, which is one
  #small request to find out
  if(.zarr_s3_prefix_has_store(client, bucket, base)){
    stores = base
    if(length(pattern) > 0){
      for(p in pattern){
        stores = grep(p, stores, value = TRUE)
      }
    }
    return(sub('/$', '', stores))
  }

  stores = character(0)
  frontier = base
  n_dirs = 0L
  depth = 0L
  while(length(frontier) > 0){
    depth = depth + 1L
    if(!isTRUE(recursive) && depth > 1L){
      break
    }
    if(n_dirs + length(frontier) > max_dirs){
      warning('Stopped searching s3://', bucket, '/', base, ' after ', n_dirs,
              ' directories; use a narrower prefix or raise max_dirs.', call. = FALSE)
      break
    }
    descend = character(0)
    for(dir_prefix in frontier){
      n_dirs = n_dirs + 1L
      subs = .zarr_s3_list_prefixes(client, bucket, dir_prefix)
      is_store = grepl('\\.zarr/$', subs)
      stores = c(stores, subs[is_store])
      descend = c(descend, subs[!is_store])
    }
    frontier = unique(descend)
  }

  stores = sort(unique(sub('/$', '', stores)))
  if(length(pattern) > 0){
    for(p in pattern){
      stores = grep(p, stores, value = TRUE)
    }
  }
  return(stores)
}

#Per axis pixel scales in arcsec from the CD matrix, the same arithmetic as
#Rwcs_pixscale (type = 'old') but in plain R, so the coarse filter needs neither Rwcs
#nor wcslib. NULL means there is no usable CD matrix, which the caller reports as a
#store it cannot search rather than as one that does not overlap.
.zarr_cd_pixscale = function(keyvalues){
  need = c('CD1_1', 'CD1_2', 'CD2_1', 'CD2_2')
  vals = keyvalues[need]
  bad = any(vapply(vals, function(v){
    is.null(v) || !is.numeric(v) || length(v) != 1 || !is.finite(v)
  }, logical(1)))
  if(bad){
    return(NULL)
  }
  sx = 3600 * sqrt(vals$CD1_1^2 + vals$CD1_2^2)
  sy = 3600 * sqrt(vals$CD2_1^2 + vals$CD2_2^2)
  if(sx <= 0 || sy <= 0){
    return(NULL)
  }
  return(list(x = sx, y = sy))
}

#The shape a WCS aware reader should use. A tile compressed image describes its real
#shape in ZNAXIS rather than NAXIS, which is what Rwcs_overlap and .wcs2_axes assume.
#The array shape from the store is the fallback when the header is silent.
.zarr_wcs_shape = function(keyvalues, shape = NULL){
  n1 = keyvalues$NAXIS1
  n2 = keyvalues$NAXIS2
  if(isTRUE(keyvalues$ZIMAGE)){
    n1 = keyvalues$ZNAXIS1
    n2 = keyvalues$ZNAXIS2
  }
  ok = function(v) !is.null(v) && is.numeric(v) && length(v) == 1 && is.finite(v) && v >= 1
  if(!ok(n1) || !ok(n2)){
    if(is.null(shape) || length(shape) < 2 || anyNA(shape[1:2])){
      return(NULL)
    }
    n1 = shape[1]
    n2 = shape[2]
  }
  return(c(as.numeric(n1), as.numeric(n2)))
}

#Great circle separation in arcsec, vectorised over any of its arguments. This only
#has to be good enough to rule out a tile degrees away. Haversine is used rather than
#the spherical law of cosines because the latter loses all its significant figures at
#the small separations that matter most here.
.zarr_ang_sep_arcsec = function(ra1, dec1, ra2, dec2){
  rad = pi/180
  lon = (ra1 - ra2) * rad
  lat1 = dec1 * rad
  lat2 = dec2 * rad
  h = sin((lat2 - lat1)/2)
  k = sin(lon/2)
  h = h * h + cos(lat1) * cos(lat2) * k * k
  h = pmin(1, pmax(0, h))
  return(2 * asin(sqrt(h)) / rad * 3600)
}

#Reduce the metadata of one candidate extension to what is needed to search it, and
#say why it cannot be searched when it cannot. status is one of 'ok', 'noheader',
#'nowcs', 'nodim', 'nopixscale'.
.zarr_cutout_tile_info = function(keyvalues, shape = NULL){
  if(is.null(keyvalues) || length(keyvalues) == 0){
    return(list(status = 'noheader'))
  }
  #Only two celestial axes may be handed to wcslib, or ncoord and nelem disagree with
  #the parsed structure and corrupt memory inside Cwcs_head_p2s (see .wcs2_axes)
  kv = .wcs2_axes(keyvalues)$keyvalues
  ra = kv$CRVAL1
  dec = kv$CRVAL2
  good = function(v) !is.null(v) && is.numeric(v) && length(v) == 1 && is.finite(v)
  if(!good(ra) || !good(dec)){
    return(list(status = 'nowcs'))
  }
  naxis = .zarr_wcs_shape(kv, shape)
  if(is.null(naxis)){
    return(list(status = 'nodim'))
  }
  scale = .zarr_cd_pixscale(kv)
  if(is.null(scale)){
    return(list(status = 'nopixscale'))
  }
  crpix = c(if(good(kv$CRPIX1)) kv$CRPIX1 else 1, if(good(kv$CRPIX2)) kv$CRPIX2 else 1)
  #The dim a pointer should carry. The array shape from the store is authoritative,
  #but a header-only cache miss has no shape and the WCS shape stands in for it.
  dim = if(is.null(shape) || anyNA(shape)) naxis else shape
  #full_keyvalues is the header as the store holds it, kept beside the trimmed copy
  #because a pointer handed back to the caller must carry every keyword, including the
  #third and fourth axes the search itself never looks at. Only the trimmed kv may be
  #given to Rwcs, so the search uses keyvalues and the pointer uses full_keyvalues.
  type = c('vector', 'image', 'cube', 'array')[length(dim)]
  if(is.na(type)){
    type = 'array'
  }
  return(list(status = 'ok', keyvalues = kv, full_keyvalues = keyvalues,
              naxis = naxis, scale = scale, ra = ra, dec = dec, crpix = crpix,
              store_shape = shape, dim = dim, type = type))
}

#The furthest a point may be from the reference position of a tile and still possibly
#fall inside it, in arcsec: the tile half diagonal, plus the box half diagonal, plus
#the distance from the reference point to the tile centre (CRPIX is not necessarily the
#centre). Nothing beyond this can overlap however the tile is rotated or projected, and
#the margin covers projection curvature and a slightly optimistic pixel scale.
.zarr_cutout_max_sep = function(naxis, crpix, scale, box_pix, safety = 0.1){
  half_tile = 0.5 * sqrt((naxis[1] * scale$x)^2 + (naxis[2] * scale$y)^2)
  half_box = 0.5 * sqrt((box_pix[1] * scale$x)^2 + (box_pix[2] * scale$y)^2)
  ref_off = 0.5 * sqrt(((naxis[1] - 2 * (crpix[1] - 1))^2 * scale$x^2) +
                         ((naxis[2] - 2 * (crpix[2] - 1))^2 * scale$y^2))
  return((1 + safety) * (half_tile + half_box) + ref_off)
}

#The WCS of the requested box, in the tile's own projection. Built from the tile
#keywords so the box is centred on the requested position at the tile's own pixel
#scale, which is what lets one request be phrased in arcsec across tiles that do not
#share a scale.
.zarr_cutout_box_keyvalues = function(keyvalues, RA, Dec, box_pix){
  kv = keyvalues
  kv$NAXIS = 2L
  kv$NAXIS1 = as.integer(box_pix[1])
  kv$NAXIS2 = as.integer(box_pix[2])
  if(isTRUE(kv$ZIMAGE) && !is.null(kv$ZNAXIS1)){
    kv$ZNAXIS = 2L
    kv$ZNAXIS1 = as.integer(box_pix[1])
    kv$ZNAXIS2 = as.integer(box_pix[2])
  }
  kv$CRPIX1 = (box_pix[1] + 1) / 2
  kv$CRPIX2 = (box_pix[2] + 1) / 2
  kv$CRVAL1 = RA
  kv$CRVAL2 = Dec
  return(kv)
}

#The box used for the overlap test, which is the requested box grown by buffer on each
#side. buffer is applied by widening the box rather than by passing Rwcs_overlap's own
#buffer argument: that argument reaches Rwcs_in_image, but the function first rejects
#anything whose centres are further apart than a max_sep built from the unbuffered
#frames, and so silently discards the buffer for exactly the distant tiles it exists to
#catch. A wider box grows that bound and the boundary test together, and costs nothing
#else. Kept as its own helper because the coarse bound has to be computed from the same
#widened box, or the two stages can disagree about what is a candidate.
.zarr_cutout_test_box = function(box_pix, buffer = 0, scale = NULL){
  if(!(buffer > 0)){
    return(box_pix)
  }
  if(is.null(scale)){
    stop('A pixel scale is needed to widen the box by buffer.', call. = FALSE)
  }
  pad = c(buffer/scale$x, buffer/scale$y)
  if(any(!is.finite(pad)) || any(pad < 0)){
    stop('buffer could not be converted to pixels!', call. = FALSE)
  }
  #Monotone by construction. Rounding a sub-pixel pad to zero must never leave the
  #test box smaller than the box being asked for, since the coarse bound is derived
  #from it.
  return(pmax(box_pix, ceiling(box_pix + 2 * pad)))
}

#The precise overlap test for one box against one tile: Rwcs_overlap, with the box as
#the test frame and the tile as the reference. That argument order is the cheaper one,
#since the boundary of keyvalues_test is projected first and the box is by far the
#smaller of the two.
#
#test_box is already the buffered box, widened by the caller rather than here, because
#the coarse bound has to be derived from the very same numbers. Widening it in both
#places would let the two drift apart, and the cheap stage then rejects tiles the
#expensive stage would have kept. Only the test uses the widened box; the cutout that
#comes back is still the one that was asked for.
.zarr_cutout_overlap = function(keyvalues, RA, Dec, test_box){
  if(!requireNamespace('Rwcs', quietly = TRUE)){
    stop('The Rwcs package is needed to check overlap by RA/Dec. Please install it ',
         'from CRAN.', call. = FALSE)
  }
  box_kv = .zarr_cutout_box_keyvalues(keyvalues, RA, Dec, test_box)
  #Rwcs_overlap keeps its reference WCS in options() and reports on wcslib failures.
  #Neither should be visible once per surviving tile.
  res = suppressMessages(tryCatch(Rwcs::Rwcs_overlap(keyvalues_test = box_kv,
                                                    keyvalues_ref = keyvalues),
                                  error = function(e) e))
  if(inherits(res, 'error')){
    stop(conditionMessage(res), call. = FALSE)
  }
  return(isTRUE(res))
}

#The positions to search for, given as RA/Dec vectors or as a two column matrix. Both
#are degrees.
.zarr_cutout_positions = function(RA, Dec, loc){
  if(!is.null(loc)){
    if(!is.matrix(loc) || ncol(loc) != 2){
      stop('loc must be a matrix with two columns (RA, Dec)!', call. = FALSE)
    }
    pos = loc
  }else{
    assertNumeric(RA, min.len=1)
    assertNumeric(Dec, min.len=1)
    if(length(RA) != length(Dec)){
      if(length(RA) == 1){
        RA = rep(RA, length(Dec))
      }else if(length(Dec) == 1){
        Dec = rep(Dec, length(RA))
      }else{
        stop('RA and Dec must have the same length, or one of them must be length 1!',
             call. = FALSE)
      }
    }
    pos = cbind(RA, Dec)
  }
  pos = as.matrix(pos)
  storage.mode(pos) = 'double'
  #Row names survive assembles such as rbind(one_point, another) and would reappear as
  #names on the extracted positions, so that the same request made as a matrix differs
  #from the same request made as two vectors for no reason a caller could use
  rownames(pos) = NULL
  if(any(!is.finite(pos))){
    stop('The requested RA/Dec positions must all be finite!', call. = FALSE)
  }
  colnames(pos) = c('RA', 'Dec')
  return(pos)
}

#The box in pixels of one tile. In arcsec it is converted per tile, so a search across
#tiles of different pixel scale asks for the same angular box everywhere.
.zarr_cutout_box_pix = function(box, box.unit, scale){
  if(length(box) == 1){box = c(box, box)}
  #Positivity is a property of the request whatever unit it is in. How big the box has
  #to be is not: in pixels it must cover at least one, in arcsec it must cover at least
  #one of this tile, which is the check below rather than this one.
  if(any(!is.finite(box)) || any(box <= 0)){
    stop('box must be one or two positive numbers!', call. = FALSE)
  }
  if(box.unit == 'arcsec'){
    box_pix = c(box[1]/scale$x, box[2]/scale$y)
    #Silently rounding a sub pixel box up to one would hide a request that does not fit
    if(any(box_pix < 1)){
      stop('the requested box (', box[1], ' x ', box[2], ' arcsec) is smaller than one ',
           'pixel of this tile (', format(round(scale$x, 4), nsmall = 4), ' x ',
           format(round(scale$y, 4), nsmall = 4), ' arcsec)', call. = FALSE)
    }
    box_pix = ceiling(box_pix)
  }else{
    if(any(box < 1)){
      stop('box must be one or two numbers, each of at least 1 pixel!', call. = FALSE)
    }
    box_pix = ceiling(box)
  }
  return(as.integer(box_pix))
}

#Resolve an RA/Dec position to the pixel it falls on, in the frame of the given
#header. The same conversion the type = 'coord' cutout path makes, kept separate so a
#cube or array can build a pixel range from it.
.zarr_cutout_centre_pixel = function(keyvalues, RA, Dec){
  if(!requireNamespace('Rwcs', quietly = TRUE)){
    stop('The Rwcs package is needed to cut out by RA/Dec. Please install it from CRAN.',
         call. = FALSE)
  }
  #Only RA and Dec are being projected, so a header claiming more axes has to be
  #trimmed first (see .wcs2_axes). The fixed width form is rebuilt from the keywords,
  #since a search only ever holds keywords.
  wcs2 = .wcs2_axes(keyvalues, Rfits_keyvalues_to_raw(keyvalues))
  xy = suppressMessages(Rwcs::Rwcs_s2p(RA, Dec, keyvalues = wcs2$keyvalues,
                                       header = wcs2$header, pixcen = 'R'))
  return(c(as.numeric(xy[1, 1]), as.numeric(xy[1, 2])))
}

Rfits_cutout_zarr_dir = function(dir = NULL, filelist = NULL, pattern = NULL,
                                 recursive = TRUE,
                                 bucket = NULL, prefix = '',
                                 region = NULL, endpoint = NULL,
                                 access_key = NULL, secret_key = NULL,
                                 session_token = NULL,
                                 RA = NULL, Dec = NULL, loc = NULL,
                                 box = 101, box.unit = c('pix', 'arcsec'),
                                 extname = 'data1',
                                 buffer = 0, safety = 0.1,
                                 cache = NULL, refresh = FALSE,
                                 header = TRUE, extract = TRUE,
                                 max_dirs = 1000,
                                 verbose = TRUE, ...){
  .zarr_require()

  assertString(dir, null.ok = TRUE)
  assertCharacter(filelist, null.ok = TRUE)
  assertCharacter(pattern, null.ok = TRUE)
  assertFlag(recursive)
  assertString(bucket, null.ok = TRUE)
  assertString(prefix)
  assertString(region, null.ok = TRUE)
  assertString(endpoint, null.ok = TRUE)
  assertString(access_key, null.ok = TRUE)
  assertString(secret_key, null.ok = TRUE)
  assertString(session_token, null.ok = TRUE)
  #match.arg does the validation, and reports the allowed values itself
  box.unit = match.arg(box.unit)
  assertNumeric(box, min.len=1, max.len=2)
  assertCharacter(extname, min.len=1)
  assertNumeric(buffer, len=1, lower=0)
  assertNumeric(safety, len=1, lower=0)
  assertString(cache, null.ok=TRUE)
  assertFlag(refresh)
  assertFlag(header)
  assertFlag(extract)
  assertFlag(verbose)
  assertCount(max_dirs, positive = TRUE)

  if(is.null(bucket) && is.null(dir) && is.null(filelist)){
    stop('One of dir, filelist, or bucket is required!', call. = FALSE)
  }
  if(!is.null(bucket) && (!is.null(dir) || !is.null(filelist))){
    stop('Give bucket (a remote prefix) or dir/filelist (local stores), not both!',
         call. = FALSE)
  }

  pos = .zarr_cutout_positions(RA = RA, Dec = Dec, loc = loc)
  Npos = nrow(pos)

  creds = .zarr_s3_env_defaults(region = region, endpoint = endpoint,
                                access_key = access_key, secret_key = secret_key,
                                session_token = session_token)
  remote = !is.null(bucket)
  client = NULL

  if(remote){
    if(!requireNamespace('paws.storage', quietly = TRUE)){
      stop('The paws.storage package is needed to search a Zarr directory over S3. ',
           'Please install it from CRAN.', call. = FALSE)
    }
    if(is.null(creds$access_key) || is.null(creds$secret_key)){
      stop('access_key and secret_key are required to search a Zarr directory over S3, ',
           'whether given directly or by RFITS_S3_ACCESS_KEY and RFITS_S3_SECRET_KEY.',
           call. = FALSE)
    }
    #One client for the whole scan. Reading pixels still needs a store object per
    #store, since the prefix is fixed when zarr_s3store is built, but those are only
    #made for stores that actually matched.
    client = paws.storage::s3(config = .zarr_s3_config(region = creds$region,
                                                       endpoint = creds$endpoint,
                                                       access_key = creds$access_key,
                                                       secret_key = creds$secret_key,
                                                       session_token = creds$session_token))
  }

  #The index, if one is held for this store set. It is opened before anything is read
  #because it answers two questions: which stores exist under a remote prefix, and what
  #their headers say. A remote one is downloaded whole to a temporary file, since every
  #query against it is an arrow scan and a scan needs a seekable file.
  idx_path = NULL
  idx_rows = NULL
  idx_at = NULL
  idx_src = NULL
  if(!is.null(cache)){
    src = .zarr_index_open(cache, bucket = bucket, prefix = prefix, client = client)
    if(!is.null(src$path)){
      ver = .zarr_index_version_of(src$path)
      if(is.na(ver) || ver > .zarr_index_version){
        .zarr_index_close(src)
        stop('The Zarr index at ', src$shown, ' is not a readable Rfits index (version ',
             if(is.na(ver)) 'missing' else ver, '). Rebuild it with ',
             'Rfits_zarr_index(index = ..., refresh = TRUE).', call. = FALSE)
      }
      #The file is kept open even when its rows are being ignored, because the merge at the
      #end still needs the rows of stores this run is not searching. refresh means "do not
      #trust these entries", not "these rows never existed".
      idx_src = src
      idx_path = src$path
      if(!isTRUE(refresh)){
        idx_rows = .zarr_index_light_rows(idx_path)
        idx_at = stats::setNames(as.list(seq_len(nrow(idx_rows))), idx_rows$cache_key)
      }
    }else{
      .zarr_index_close(src)
    }
  }
  #The downloaded copy of a remote index is this call's own temporary file, and nothing
  #after the scan needs it, so it goes whatever the search returns
  on.exit(.zarr_index_close(idx_src))

  #The stores to search. This comes after the index has been opened because the index
  #holds the listing of a remote prefix, and reading a cached walk out of it is the only
  #way a search avoids a request per directory. Both this and the standalone builder go
  #through .zarr_index_find_stores so they cannot disagree about which store is which, or
  #about what makes a cached listing the one asked for.
  stores = .zarr_index_find_stores(dir = dir, filelist = filelist, pattern = pattern,
                                   recursive = recursive, bucket = bucket,
                                   prefix = prefix, max_dirs = max_dirs,
                                   index_path = idx_path, refresh = refresh,
                                   client = client, verbose = verbose)
  filelist = stores$store
  Nstore = length(filelist)
  labels = stores$label
  client = stores$client

  #The search works on the light rows, which carry everything the cone filter reads but
  #not the keywords. A store's keywords are only needed once it has survived that filter,
  #and they are the bulk of the index, so they are fetched for the candidates rather than
  #for every store in the directory (see .zarr_index_heavy_col).
  tiles = vector(mode = 'list', length = Nstore)
  status = rep(NA_character_, Nstore)
  used_ext = rep(NA_character_, Nstore)
  store_errors = rep(NA_character_, Nstore)
  #Where each tile's keywords are: in the index, to be fetched, or already in hand
  from_index = rep(FALSE, Nstore)
  idx_use = vector(mode = 'list', length = Nstore)
  new_rows = vector(mode = 'list', length = Nstore)

  for(i in seq_len(Nstore)){
    store = filelist[i]
    got = NULL
    fail = NULL
    for(name in extname){
      key = .zarr_index_store_key(labels[i], name)
      at = if(is.null(idx_at)) NULL else idx_at[[key]]
      if(!is.null(at)){
        row = idx_rows[at, ]
        if(!is.na(row$error) && nzchar(row$error)){
          store_errors[i] = row$error
        }
        #A row written without the full keywords and the array type cannot build a correct
        #lazy pointer (a cube would lose its third axis), so it counts as a miss and is
        #rewritten by the read below
        ok_row = .zarr_index_row_usable(row)
        if(ok_row && !remote){
          #Local rows are checked against the size and mtime of the metadata document,
          #which costs a stat and so cannot go stale unnoticed. Over S3 the only check
          #available is the request the index exists to avoid, so remote rows are trusted
          #and refresh = TRUE is the documented way to ignore them.
          ok_row = identical(row$stamp, .zarr_index_stamp(store, name))
        }
        if(ok_row){
          got = .zarr_index_tile(row)
          from_index[i] = TRUE
          idx_use[i] = at
          used_ext[i] = name
          break
        }
      }

      read = .zarr_store_meta(store, label = labels[i], extname = name,
                              remote = remote, client = client, bucket = bucket)
      if(is.null(read$extname)){
        if(!is.null(read$info)){
          #A store that could not be read at all is a failure rather than a filtering
          #choice, and the reason is reported whatever verbose says
          fail = read$info$error
        }
        next
      }
      got = read$info
      used_ext[i] = name
      stamp = if(remote){
        NA_character_
      }else{
        .zarr_index_stamp(store, name)
      }
      new_rows[[i]] = .zarr_index_store_row(labels[i], store, name, got, stamp = stamp)
      break
    }

    if(!is.null(fail)){
      status[i] = 'error'
      store_errors[i] = fail
      next
    }
    if(is.null(got)){
      status[i] = 'noext'
      store_errors[i] = paste0('no extension named ', paste(extname, collapse = ' or '))
      next
    }
    status[i] = got$status
    tiles[[i]] = got
  }

  added = new_rows[!vapply(new_rows, is.null, logical(1))]
  #The version travels with every write, including the first, since an index that cannot
  #say which version it is cannot be read back safely
  fresh = .zarr_index_rows(c(added, list(.zarr_index_meta_row('index_version',
                                                               .zarr_index_version))))
  if(!is.null(stores$walked) && !is.null(cache)){
    fresh = .zarr_index_merge(fresh, .zarr_index_rows(
      list(.zarr_index_walk_row(stores$walk_key, stores$walked))))
  }
  if(!is.null(cache) && nrow(fresh) > 0){
    #The whole file is read first because a merge that carried rows over without their
    #keywords would quietly strip the stores this run did not visit. That is the one place
    #where loading an index rather than querying it is right, and it is a few hundred kB.
    #A failed write is a warning rather than an error: the search already has its answers,
    #and losing them because a cache could not be updated would be the worse outcome.
    saved = tryCatch({
      if(!is.null(idx_path)){
        fresh = .zarr_index_merge(.zarr_index_load_all(idx_path), fresh)
      }
      .zarr_index_put(fresh, cache, bucket = bucket, prefix = prefix, client = client)
      TRUE
    }, error = function(e) e)
    if(!isTRUE(saved) && verbose){
      warning('Could not write the Zarr index: ', conditionMessage(saved), call. = FALSE)
    }
  }

  #A store that could not be read, or that holds none of the extensions asked for, is a
  #failure rather than a filtering choice. It is counted here and reported once after the
  #loop: a directory of a thousand tiles with one typo in extname would otherwise emit a
  #thousand identical warnings, which hides the message rather than making it.
  skipped = which(status %in% c('error', 'noext'))
  if(length(skipped) > 0){
    reasons = store_errors[skipped]
    for(reason in unique(reasons)){
      at = skipped[reasons == reason]
      shown = basename(filelist[at])
      warning(length(at), ' store(s) skipped: ', reason,
              if(length(at) <= 3) paste0(' [', paste(shown, collapse = ', '), ']')
              else paste0(' [', paste(head(shown, 3), collapse = ', '), ', ...]'),
              call. = FALSE)
    }
  }

  usable = vapply(tiles, function(t) !is.null(t) && identical(t$status, 'ok'),
                  logical(1))
  #A cube or 4D array is searched as well, but a box applies to its first two axes and
  #every plane comes back, which is what the pointer methods do. An array with no
  #second axis cannot hold a two dimensional box at all.
  for(i in which(usable)){
    shape = tiles[[i]]$store_shape
    ndim = if(is.null(shape) || anyNA(shape)) length(tiles[[i]]$naxis) else length(shape)
    if(ndim < 2){
      usable[i] = FALSE
      status[i] = 'not2d'
    }
  }

  n_usable = sum(usable)
  if(verbose){
    message('Found ', Nstore, ' Zarr store(s), ', n_usable, ' with usable WCS metadata')
  }
  if(n_usable == 0){
    stop('None of the Zarr stores found carry usable WCS metadata, so overlap cannot ',
         'be determined. Check extname, and the status column of the scan table.',
         call. = FALSE)
  }

  #The box in pixels of each tile, the widened box the search uses, and the radius
  #within which that tile can possibly be hit. All depend only on the tile, so they are
  #worked out once and reused for every requested position. The radius is computed from
  #the widened box and not given buffer a second time, so the coarse bound and the
  #precise test describe the same region; if they disagreed, the cheap stage could
  #reject a tile the expensive stage would have accepted.
  box_pix = vector(mode = 'list', length = Nstore)
  test_box = vector(mode = 'list', length = Nstore)
  max_sep = rep(NA_real_, Nstore)
  box_fail = NULL
  for(i in which(usable)){
    tile = tiles[[i]]
    this_box = tryCatch(.zarr_cutout_box_pix(box, box.unit, tile$scale),
                        error = function(e) e)
    if(inherits(this_box, 'error')){
      usable[i] = FALSE
      status[i] = 'boxtoosmall'
      store_errors[i] = conditionMessage(this_box)
      if(is.null(box_fail)){
        box_fail = conditionMessage(this_box)
      }
      next
    }
    box_pix[[i]] = this_box
    test_box[[i]] = .zarr_cutout_test_box(this_box, buffer = buffer, scale = tile$scale)
    max_sep[i] = .zarr_cutout_max_sep(tile$naxis, tile$crpix, tile$scale, test_box[[i]],
                                      safety = safety)
  }

  #If no store could be searched with this box, that is a mistake in the request rather
  #than an absence of data, and the real reason is reported rather than a guess about
  #arcsec versus pixels
  if(!any(usable)){
    if(!is.null(box_fail)){
      stop('No Zarr store can be searched with this box: ', box_fail, call. = FALSE)
    }
    stop('No Zarr store can be searched with this request.', call. = FALSE)
  }

  #One vectorised cone test per position, over every tile at once. This is the whole
  #cost of rejecting the tiles that are nowhere near the request.
  idx_usable = which(usable)
  candidates = vector(mode = 'list', length = Npos)
  if(length(idx_usable) > 0){
    tile_ra = vapply(idx_usable, function(i) tiles[[i]]$ra, numeric(1))
    tile_dec = vapply(idx_usable, function(i) tiles[[i]]$dec, numeric(1))
    #max_sep is already built from the buffered test box, so buffer is not added again
    tile_limit = max_sep[idx_usable]
    for(k in seq_len(Npos)){
      sep = .zarr_ang_sep_arcsec(pos[k, 1], pos[k, 2], tile_ra, tile_dec)
      candidates[[k]] = idx_usable[sep <= tile_limit]
    }
  }

  output = list()
  out_names = character(0)
  out_idx = integer(0)
  out_stores = character(0)
  matches = data.frame(position = integer(0), store = character(0),
                       extname = character(0), RA = numeric(0), Dec = numeric(0),
                       box_x = integer(0), box_y = integer(0), dim = character(0),
                       stringsAsFactors = FALSE)

  #Stores opened so far, so one matched by several positions is not reopened each time
  open_ptrs = vector(mode = 'list', length = Nstore)

  #The keywords of the candidates, now that the cheap stage has said which ones matter.
  #This is the second half of the staged read: the light rows gave the coarse filter
  #everything it needed without decoding a single header, and only the handful that
  #survive to the boundary test have their full header pulled out of the index, by key.
  #The order of the survivors is irrelevant, so they are fetched once for all positions
  #rather than per position, which keeps it to one scan of the file.
  need_kv = which(from_index & !vapply(tiles, function(t) !is.null(t$full_keyvalues),
                                       logical(1)))
  cand = unique(unlist(candidates, use.names = FALSE))
  need_kv = intersect(need_kv, cand)
  if(length(need_kv) > 0){
    keys = vapply(need_kv, function(i) .zarr_index_store_key(labels[i], used_ext[i]),
                  character(1))
    pay = .zarr_index_payload_rows(idx_path, keys)
    blobs = stats::setNames(as.list(pay$keyvalues), pay$cache_key)
    for(i in need_kv){
      key = .zarr_index_store_key(labels[i], used_ext[i])
      tile = .zarr_index_tile(idx_rows[idx_use[[i]], ], keyvalues = blobs[[key]])
      if(identical(tile$status, 'error')){
        #An index row whose blob will not unpick is treated as a miss rather than as a
        #store that does not overlap, because that is what it is
        status[i] = 'error'
        store_errors[i] = tile$error
        from_index[i] = FALSE
        next
      }
      tiles[[i]] = tile
    }
  }

  for(k in seq_len(Npos)){
    for(i in candidates[[k]]){
      tile = tiles[[i]]
      hit = tryCatch(.zarr_cutout_overlap(tile$keyvalues, pos[k, 1], pos[k, 2],
                                          test_box[[i]]),
                     error = function(e) e)
      if(inherits(hit, 'error')){
        status[i] = 'error'
        store_errors[i] = conditionMessage(hit)
        warning('Overlap test failed for ', basename(filelist[i]), ': ',
                conditionMessage(hit), call. = FALSE)
        next
      }
      if(!isTRUE(hit)){
        next
      }

      if(is.null(open_ptrs[[i]])){
        #A lazy pointer, built from the metadata the search has already paid for
        #rather than by opening the store. Over S3 an open costs a probe plus a
        #hierarchy walk (a few tenths of a second per store), and extract = FALSE
        #never touches pixels at all, so nothing else has to pay for it. Opening is
        #deferred to [.Rfits_pointer_zarr via x$openspec, which means the pointer a
        #caller slices behaves exactly as one made by Rfits_point_zarr.
        source = if(remote){
          list(remote = TRUE, bucket = bucket, prefix = filelist[i],
               region = creds$region, endpoint = creds$endpoint,
               access_key = creds$access_key, secret_key = creds$secret_key,
               session_token = creds$session_token)
        }else{
          list(remote = FALSE, dir = filelist[i])
        }
        tile = tiles[[i]]
        #The untrimmed keywords when the search held them; a pointer made straight
        #from an opened store (which is how a non cached store arrives) is already
        #complete, and its $dim is the shape.
        full = tile$full_keyvalues
        if(is.null(full)){
          full = tile$keyvalues
        }
        open_ptrs[[i]] = .zarr_lazy_pointer(filename = labels[i],
                                            extname = used_ext[i],
                                            keyvalues = full, dim = tile$dim,
                                            type = tile$type, header = header,
                                            openspec = source)
      }
      ptr = open_ptrs[[i]]
      this_box = box_pix[[i]]

      item = NULL
      fail = NULL
      tryCatch({
        if(!isTRUE(extract)){
          #Report the match, and the pixels needed to check it, but do not fetch them.
          #The cached pointer was made with header = TRUE because the slice path needs
          #the keywords to resolve a position, so the flag is carried over to this one
          #copy rather than being changed on the cached original.
          item = ptr
          item$header = header
        }else if(ptr$type == 'image'){
          #The documented coordinate cutout: the pointer resolves RA/Dec to a pixel and
          #takes the box around it, padding rather than shrinking at the edge
          item = ptr[pos[k, 1], pos[k, 2], box = this_box, type = 'coord',
                     header = header, ...]
        }else{
          #A box is a two dimensional feature of the pointer methods, so a cube or
          #array has its bounds resolved here and given as a range instead. That keeps
          #the higher dimensions whole rather than slicing one plane. The bounds are
          #left unclamped, exactly as the image branch leaves them, so a box running
          #off the edge keeps the requested size and is padded with NA.
          cen = ceiling(.zarr_cutout_centre_pixel(ptr$keyvalues, pos[k, 1], pos[k, 2]))
          lo = ceiling(cen + (1 - this_box)/2)
          hi = ceiling(cen + (this_box - 1)/2)
          if(ptr$type == 'cube'){
            item = ptr[lo[1]:hi[1], lo[2]:hi[2], header = header, ...]
          }else{
            item = ptr[lo[1]:hi[1], lo[2]:hi[2], , header = header, ...]
          }
        }
      }, error = function(e) {
        fail <<- conditionMessage(e)
      })

      if(!is.null(fail)){
        status[i] = 'error'
        store_errors[i] = fail
        warning('Cutout failed for ', basename(filelist[i]), ': ', fail, call. = FALSE)
        next
      }

      name = sub('\\.zarr$', '', basename(filelist[i]))
      if(Npos > 1){
        name = paste0(name, '_', k)
      }
      output[[length(output) + 1]] = item
      out_names = c(out_names, name)
      out_idx = c(out_idx, i)
      out_stores = c(out_stores, labels[i])
      #The shape is taken from the stored dim of a pointer rather than its dim method,
      #which would reopen the store and so cost a request for a number we already have
      item_dim = if(isTRUE(extract)) dim(item) else item$dim
      matches = rbind(matches, data.frame(position = k, store = labels[i],
                                          extname = used_ext[i],
                                          RA = unname(pos[k, 1]),
                                          Dec = unname(pos[k, 2]),
                                          box_x = this_box[1], box_y = this_box[2],
                                          dim = paste(item_dim, collapse = 'x'),
                                          stringsAsFactors = FALSE))
    }
  }

  #Names are the store stub, since that is what a caller asked for. Two stores of the
  #same name in different sub directories would collide, and only there is the path
  #dragged in to tell them apart; the extension is not used, because stores that share
  #a name almost always share an extension too, so it would resolve nothing.
  if(length(out_names) > 0){
    dup = duplicated(out_names) | duplicated(out_names, fromLast = TRUE)
    if(any(dup)){
      qualify = vapply(filelist[out_idx[dup]], function(s){
        gsub('[^A-Za-z0-9]+', '_', s)
      }, character(1))
      out_names[dup] = paste0(qualify, '_', out_names[dup])
    }
    out_names = make.unique(out_names, sep = '_')
  }
  names(output) = out_names
  class(output) = 'Rfits_list'
  attributes(output)$filename = out_stores

  #The components are a mix of scalars and short vectors, so the element wanted has to
  #be asked for here rather than indexed at the call site: indexing outside collapses
  #to a scalar first, and naxis[2] of a one axis tile is then NA whichever way the tile
  #looks. A store that was never searchable has no tile to index at all, which is the
  #first test rather than an accident of $ returning NULL.
  num_from_tile = function(t, which, num = 1){
    if(is.null(t) || is.null(t[[which]])){
      return(NA_real_)
    }
    val = as.numeric(t[[which]])
    if(length(val) < num){
      return(NA_real_)
    }
    return(val[num])
  }

  scan = data.frame(store = labels, extname = used_ext, status = status,
                    naxis1 = vapply(tiles, function(t) num_from_tile(t, 'naxis', 1),
                                    numeric(1)),
                    naxis2 = vapply(tiles, function(t) num_from_tile(t, 'naxis', 2),
                                    numeric(1)),
                    pixscale_x = vapply(tiles,
                                        function(t) num_from_tile(t$scale, 'x'),
                                        numeric(1)),
                    pixscale_y = vapply(tiles,
                                        function(t) num_from_tile(t$scale, 'y'),
                                        numeric(1)),
                    centre_RA = vapply(tiles, function(t) num_from_tile(t, 'ra'),
                                       numeric(1)),
                    centre_Dec = vapply(tiles, function(t) num_from_tile(t, 'dec'),
                                        numeric(1)),
                    max_sep_arcsec = max_sep,
                    error = store_errors,
                    stringsAsFactors = FALSE)

  attributes(output)$scan = scan
  attributes(output)$matches = matches
  attributes(output)$cutout = list(RA = pos[, 1], Dec = pos[, 2], box = box,
                                   box.unit = box.unit, buffer = buffer,
                                   extract = extract, extname = extname)

  if(verbose){
    message('Matched ', length(output), ' cutout(s) from ',
            length(unique(matches$store)), ' store(s)')
  }
  #An empty result is said out loud whatever verbose says. It is the one outcome a
  #caller cannot infer from the returned object, since an empty Rfits_list looks the
  #same whether nothing overlapped or nothing was searched, and the scan table only
  #answers that once someone has thought to look at it.
  if(length(output) == 0){
    message('No Zarr store overlaps the requested position(s).')
  }

  return(invisible(output))
}
