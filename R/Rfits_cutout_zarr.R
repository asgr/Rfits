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

#Can a cached tile record build a lazy pointer? Only 'ok' stores are ever pointed at,
#and those must carry the untrimmed keywords and the array type; an entry written
#before the cache held either is treated as a miss and re-read, rather than handed
#back as a pointer whose header is quietly missing an axis.
.zarr_cache_entry_usable = function(info){
  if(is.null(info)){
    return(FALSE)
  }
  if(!identical(info$status, 'ok')){
    return(TRUE)
  }
  return(!is.null(info$full_keyvalues) && !is.null(info$type) && !is.null(info$dim))
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

  #The cache is loaded before the store list is built, because the listing of a
  #remote prefix is itself one of the things worth caching: finding the stores costs
  #a request per directory, and a bucket laid out as one directory of tiles pays
  #that on every search. Metadata entries are keyed by store and extension; this one
  #entry is keyed by the whole spec of the walk, so a narrower prefix or a different
  #pattern cannot be served from a listing that did not make them.
  cache_path = NULL
  cached = list()
  list_key = NULL
  walked = NULL
  if(!is.null(cache)){
    cache_path = path.expand(cache)
    if(!isTRUE(refresh) && file.exists(cache_path)){
      loaded = tryCatch(readRDS(cache_path), error = function(e) NULL)
      if(is.list(loaded)){
        cached = loaded
      }
    }
    if(remote){
      #max_dirs is part of the spec because a walk that stopped early holds a partial
      #list, and reusing that as though it were complete would be worse than the cost
      #of walking again
      list_key = paste0('LIST|', bucket, '|', prefix, '|', isTRUE(recursive), '|',
                        paste0(pattern, collapse = ','), '|', max_dirs)
    }
  }
  cached_list = if(is.null(list_key)) NULL else cached[[list_key]]

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
    if(is.null(cached_list)){
      #A walk that gave up at max_dirs holds a partial list, and caching that would
      #hide stores from every later search without saying so. Catching the warning
      #here rather than in the helper keeps the batch paths untouched.
      truncated = FALSE
      filelist = withCallingHandlers(
        .zarr_s3_find_stores(client, bucket, prefix, recursive = recursive,
                             pattern = pattern, max_dirs = max_dirs),
        warning = function(w){
          #No muffleRestart here, so the warning still reaches the caller: it is
          #their only notice that the search saw part of the directory
          if(grepl('directories; use a narrower prefix', conditionMessage(w))){
            truncated <<- TRUE
          }
        })
      #Remember that this run paid for the walk, so the list is worth caching
      if(!truncated){
        walked = filelist
      }
    }else{
      #Served straight from the cache. A listing whose directories were walked
      #cannot be checked against the bucket without walking it again, which is the
      #request refresh exists to skip, so this is trusted exactly as a cached header
      #is and is reported as such when verbose.
      filelist = as.character(cached_list)
      if(verbose){
        message('Using cached store list for s3://', bucket, '/',
                .zarr_s3_prefix(prefix), ' (', length(filelist),
                ' stores); refresh = TRUE to re-list')
      }
    }
    if(length(filelist) == 0){
      stop('No Zarr stores found under s3://', bucket, '/', .zarr_s3_prefix(prefix),
           if(length(pattern) > 0) ' with the given pattern' else '', '!', call. = FALSE)
    }
  }else{
    if(is.null(filelist)){
      dir = path.expand(dir)
      if(!dir.exists(dir)){
        stop('Directory does not exist: ', dir, call. = FALSE)
      }
      #A Zarr store is a directory, so the search is for directories, not files.
      #list.files reports what is below dir and never dir itself, so a directory named
      #*.zarr is offered as its own store here, matching the remote path which probes
      #the prefix before listing it.
      filelist = character(0)
      if(grepl('\\.zarr$', dir)){
        filelist = dir
      }
      found = list.files(dir, full.names = TRUE, include.dirs = TRUE,
                         recursive = recursive)
      filelist = c(filelist, grep('\\.zarr$', found, value = TRUE))
      filelist = filelist[dir.exists(filelist)]
    }else{
      filelist = path.expand(filelist)
      filelist = filelist[dir.exists(filelist)]
    }
    if(length(pattern) > 0){
      for(p in pattern){
        filelist = grep(p, filelist, value = TRUE)
      }
    }
    filelist = sort(unique(filelist))
    if(length(filelist) == 0){
      stop('No Zarr stores found', if(!is.null(dir)) paste0(' in ', dir) else '',
           if(length(pattern) > 0) ' with the given pattern' else '', '!', call. = FALSE)
    }
  }

  Nstore = length(filelist)
  labels = if(remote) paste0('s3://', bucket, '/', filelist) else filelist

  #A cached copy of the search metadata, keyed by store and extension (the listing is
  #keyed separately, above). Local entries are validated against the size and mtime
  #of the metadata file, which costs a stat and so cannot go stale unnoticed. Over
  #S3 the only check available would be the request the cache exists to avoid, so a
  #cached remote entry is trusted and refresh = TRUE is the documented way to ignore it.
  fresh = list()
  if(!is.null(cache_path)){
    #Untouched entries carry over. A run filtered by pattern searches a subset of the
    #stores, and rebuilding the cache from only that subset would quietly discard the
    #work every other run had put into it.
    fresh = cached
    if(!is.null(list_key)){
      #Either the listing this run just walked and may keep, or the one it was served
      #and must not drop. Only the exact spec that was searched is recorded, so a run
      #narrowed by pattern cannot make a broad listing out of a partial one.
      fresh[[list_key]] = if(is.null(cached_list)) walked else cached_list
    }
  }

  tiles = vector(mode = 'list', length = Nstore)
  status = rep(NA_character_, Nstore)
  used_ext = rep(NA_character_, Nstore)
  store_errors = rep(NA_character_, Nstore)

  for(i in seq_len(Nstore)){
    store = filelist[i]
    got = NULL
    fail = NULL
    for(name in extname){
      entry = NULL
      stamp = NA_character_
      if(!is.null(cache_path)){
        hit = cached[[paste0(labels[i], '|', name)]]
        #An entry written before the format carried the full keywords and the array
        #type cannot build a correct lazy pointer (a cube would lose its third axis),
        #so it counts as a miss and is rewritten by the read below
        if(!is.null(hit) && !.zarr_cache_entry_usable(hit$info)){
          hit = NULL
        }
        if(!is.null(hit)){
          if(remote){
            entry = hit$info
          }else{
            info_stat = file.info(file.path(store, .zarr_meta_key(name)))
            if(isTRUE(info_stat$exists) && !is.na(info_stat$size)){
              stamp = paste0(info_stat$size, '-', as.numeric(info_stat$mtime))
              if(identical(hit$stamp, stamp)){
                entry = hit$info
              }
            }
          }
        }
      }

      if(!is.null(entry)){
        got = entry
      }else if(remote){
        meta = .zarr_meta_from_s3(client, bucket, store, name)
        if(is.null(meta)){
          next
        }
        if(!is.null(attr(meta, 'error'))){
          fail = attr(meta, 'error')
          break
        }
        got = .zarr_cutout_tile_info(meta$keyvalues, meta$shape)
      }else{
        meta = .zarr_meta_from_dir(store, name)
        if(is.null(meta)){
          #Either a v2 store or one whose array is not named as asked. Only now is it
          #worth opening the store properly, which also gives a better message than
          #'not found' when the store is genuinely unreadable.
          opened = tryCatch(Rfits_point_zarr(store, extname = name, header = TRUE),
                            error = function(e) e)
          if(inherits(opened, 'error')){
            msg = conditionMessage(opened)
            if(grepl('does not exist in the Zarr store', msg)){
              next
            }
            fail = msg
            break
          }
          got = .zarr_cutout_tile_info(opened$keyvalues, opened$dim)
        }else{
          got = .zarr_cutout_tile_info(meta$keyvalues, meta$shape)
        }
      }

      if(!is.null(cache_path)){
        fresh[[paste0(labels[i], '|', name)]] = list(stamp = stamp, info = got)
      }
      used_ext[i] = name
      break
    }

    #A store that could not be read at all, or that holds none of the extensions asked
    #for, is a failure rather than a filtering choice, so it is reported whatever
    #verbose says. It is counted here and reported once after the loop: a directory of
    #a thousand tiles with one typo in extname would otherwise emit a thousand
    #identical warnings, which hides the message rather than making it.
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

  if(!is.null(cache_path)){
    saved = tryCatch({saveRDS(fresh, cache_path); TRUE}, error = function(e) FALSE)
    if(!isTRUE(saved) && verbose){
      warning('Could not write the metadata cache: ', cache_path, call. = FALSE)
    }
  }

  #One warning per distinct reason rather than per store. The reason is usually a
  #property of the request (a mistyped extname, a directory that is not a store at
  #all) and so repeats for every store in the directory, and a thousand identical
  #warnings is noise that buries the message rather than a way of making it. The
  #per store detail is left in the scan table for whoever needs it.
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
