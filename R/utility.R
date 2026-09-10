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

#Projecting pixels needs a WCS with exactly two coordinate axes, but wcslib takes
#the number of axes from NAXIS. Handing it the header of a cube or a 4D array,
#while only ever asking about RA and Dec, leaves ncoord and nelem inconsistent
#with the parsed wcsprm. Rwcs reports that on stderr and returns zeros rather
#than failing, and repeated calls corrupt memory inside Cwcs_head_p2s, so this is
#not simply a matter of getting a bad answer.
#
#Trim the keywords to the two celestial axes: drop every keyword that names an
#axis beyond the second, and make NAXIS / WCSAXES agree with what is left. A
#header that already describes two axes is returned untouched, so the ordinary
#image case cannot change at all. The raw form is only rebuilt when something
#actually had to be dropped.
.wcs2_axes = function(keyvalues, header=NULL){
  naxis = if(!is.null(keyvalues$WCSAXES)) keyvalues$WCSAXES else keyvalues$NAXIS
  if(is.null(naxis) | naxis <= 2){
    return(list(keyvalues = keyvalues, header = header))
  }
  
  nms = names(keyvalues)
  drop = grepl('^(Z)?NAXIS[3-9]$', nms) |
    grepl('^(Z)?(CRPIX|CRVAL|CTYPE|CUNIT|CDELT|CROTA|LONPOLE|LATPOLE)[3-9]$', nms) |
    grepl('^(Z)?(CD|PC)[0-9]+_[3-9]$', nms) |
    grepl('^(Z)?(CD|PC)[3-9]_[0-9]+$', nms) |
    grepl('^(Z)?WCSAXES[3-9]$', nms)
  if(all(!drop)){
    #More axes are claimed than any keyword describes, so NAXIS alone is the
    #thing that is wrong
    drop = grepl('^(WCSAXES|NAXIS)$', nms)
  }
  
  keyvalues = keyvalues[!drop]
  keyvalues$NAXIS = 2L
  if(!is.null(keyvalues$WCSAXES)){keyvalues$WCSAXES = 2L}
  if(!is.null(header)){header = Rfits_keyvalues_to_raw(keyvalues)}
  
  return(list(keyvalues = keyvalues, header = header))
}

.spans_up_to = function(x, upper) all(.minmax(x) == c(1, upper))

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
