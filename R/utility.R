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
