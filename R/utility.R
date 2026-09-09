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
