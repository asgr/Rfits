Rfits_point = function(filename='temp.fits', ext=1, header=TRUE, zap=NULL, zaptype='full',
                       allow_write=FALSE, sparse=1L, scale_sparse=FALSE){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  assertAccess(filename, access='r')
  filename = Rfits_gunzip(filename)
  if(is.character(ext)){ext = Rfits_extname_to_ext(filename, ext)}
  assertIntegerish(ext, len=1)
  assertFlag(header)
  
  temp = Rfits_read_header(filename=filename, ext=ext, zap=zap, zaptype=zaptype)
  keyvalues = temp$keyvalues
  raw = temp$raw
  
  if(isTRUE(keyvalues$ZIMAGE)){
    naxis1 = keyvalues$ZNAXIS1
    naxis2 = keyvalues$ZNAXIS2
    naxis3 = keyvalues$ZNAXIS3
    naxis4 = keyvalues$ZNAXIS4
    datatype = keyvalues$ZBITPIX
  }else{
    naxis1 = keyvalues$NAXIS1
    naxis2 = keyvalues$NAXIS2
    naxis3 = keyvalues$NAXIS3
    naxis4 = keyvalues$NAXIS4
    datatype = keyvalues$BITPIX
  }
  
  dim = c(naxis1); type='vector'
  if(!is.null(naxis2)){dim = c(dim, naxis2); type='image'}
  if(!is.null(naxis3)){dim = c(dim, naxis3); type='cube'}
  if(!is.null(naxis4)){dim = c(dim, naxis4); type='array'}
  
  output = list(filename=filename, ext=ext, keyvalues=keyvalues, raw=raw, header=header,
                zap=zap, zaptype=zaptype, allow_write=allow_write, sparse=sparse,
                scale_sparse=scale_sparse, dim=dim, type=type)
  class(output) = 'Rfits_pointer'
  return(invisible(output))
}
