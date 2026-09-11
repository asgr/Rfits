#utility functions

.safedim = function(lo_orig=1L, hi_orig=1L, lo_tar=1L, hi_tar=1L){
  len_orig = hi_orig - lo_orig + 1L
  len_tar = hi_tar - lo_tar + 1L
  
  out_lo_orig = max(lo_tar, 1L)
  out_hi_orig = min(hi_tar, len_orig)
  diff = (1L - lo_tar)
  out_lo_tar = out_lo_orig + diff
  out_hi_tar = out_hi_orig + diff
  safe = (out_hi_tar >= out_lo_tar) & (out_hi_orig >= out_lo_orig)
  return(list(orig = out_lo_orig:out_hi_orig, tar = out_lo_tar:out_hi_tar, len_orig=len_orig,
              len_tar=len_tar, safe=safe, lo_orig=lo_orig, hi_orig=hi_orig, lo_tar=lo_tar,
              hi_tar=hi_tar, diff=diff))
}

.minmax = function(x) c(min(x), max(x))

#Projecting pixels needs a WCS with exactly two coordinate axes, but wcslib
#derives the number of axes from the header, taking WCSAXES where present and
#NAXIS otherwise. Handing it the header of a cube or a 4D array, while only ever
#asking about RA and Dec, leaves ncoord and nelem inconsistent with the parsed
#wcsprm. Rwcs reports that on stderr and returns zeros rather than failing, and
#repeated calls corrupt memory inside Cwcs_head_p2s, so this is not simply a
#matter of getting a bad answer.
#
#Trim the keywords to the two celestial axes: drop every keyword that names an
#axis beyond the second, and state the axis count as two. The claim is taken from
#whichever of NAXIS, ZNAXIS and WCSAXES says more than two, since a compressed
#cube has NAXIS = 0 on the primary header and describes its real shape in ZNAXIS.
#A header that claims no more than two axes comes back untouched, raw form not
#even rebuilt, so the ordinary image case cannot change at all. Only a copy is
#modified, never the keywords stored on the object.
.wcs2_axes = function(keyvalues, header=NULL){
  counts = c('NAXIS', 'ZNAXIS', 'WCSAXES')
  claims = any(vapply(counts, function(k){
    !is.null(keyvalues[[k]]) && is.numeric(keyvalues[[k]]) && keyvalues[[k]] > 2
  }, logical(1)))
  if(!claims){
    return(list(keyvalues = keyvalues, header = header))
  }
  
  nms = names(keyvalues)
  drop = grepl('^(Z)?NAXIS[3-9]$', nms) |
    grepl('^(Z)?(CRPIX|CRVAL|CTYPE|CUNIT|CDELT|CROTA|LONPOLE|LATPOLE)[3-9]$', nms) |
    grepl('^(Z)?(CD|PC)[0-9]+_[3-9]$', nms) |
    grepl('^(Z)?(CD|PC)[3-9]_[0-9]+$', nms)
  keyvalues = keyvalues[!drop]
  
  #WCSAXES wins in wcslib, so set it even where the header had none. NAXIS and
  #ZNAXIS are only pulled down where they overstate the axes; a primary header
  #legitimately sitting at 0 is left alone. ZNAXIS matters as much as NAXIS,
  #because wcslib maps it on for tile compressed images, so leaving it at 3
  #would re-create the very inconsistency being fixed here.
  keyvalues$WCSAXES = 2L
  for(k in c('NAXIS', 'ZNAXIS')){
    if(!is.null(keyvalues[[k]]) && keyvalues[[k]] > 2){
      keyvalues[[k]] = 2L
    }
  }
  if(!is.null(header)){header = Rfits_keyvalues_to_raw(keyvalues)}
  
  return(list(keyvalues = keyvalues, header = header))
}

.spans_up_to = function(x, upper) all(.minmax(x) == c(1, upper))

#Does x hold a whole number, so that a keyword may be stored as an integer
#rather than a double? Vectorised, and always TRUE or FALSE, never NA.
#
#Equality with the number's own rounding is used in preference to the obvious
#x %% 1 == 0. R works out %% in long double, and for a negative whose size
#falls under half the spacing of long double just below 1 (2^-65, about
#2.7e-20) the correction 1 - |x| rounds to the divisor itself, which R then
#reports as a remainder of 0. The test therefore calls -1e-20 an exact multiple
#of 1, and as.integer() replaces it with 0. Positive numbers are spared,
#because their remainder is the number itself and needs no correction, so the
#flaw only bites on negative values.
#
#The is.finite() guard covers the other two ways the old test misbehaved.
#round(Inf) is Inf, so equality alone would call infinity whole and hand it to
#as.integer(), and NA_real_ %% 1 is NA, which made if() in Rfits_write_key fail
#outright with 'missing value where TRUE/FALSE needed'.
.is_whole_number = function(x){
  return(is.finite(x) & x == round(x))
}

#Is this subset expression an `a:end` range? In Rfits `end` means "the end of
#that dimension", not stats::end, so the expression has to be recognised from
#its unevaluated form and must never be forced -- merely asking is.null(i) or
#is.matrix(i) would evaluate `50:end` and fail.
.is_a_to_end = function(express){
  length(express) == 3L && identical(express[[1]], as.name(':')) &&
    grepl('end', deparse(express[[3]]))
}

#Resolve `a:end` to its two bounds, or return NULL if express is not such a
#range. Only the bounds are built, never the whole index vector, which for a
#pointer would be as long as the very dimension being read. The start is
#evaluated in the caller's frame so that p[nn:end] and p[ceiling(n/2):end] work,
#rather than being deparsed to text; the end is the dimension length, already
#known from the header.
.resolve_a_to_end = function(express, dim, envir){
  if(!.is_a_to_end(express)){return(NULL)}
  return(c(as.numeric(eval(express[[2]], envir)), dim))
}
