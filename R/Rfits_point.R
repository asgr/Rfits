Rfits_point = function(filename='temp.fits', ext=1, header=FALSE, zap=NULL){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  assertAccess(filename, access='r')
  filename = Rfits_gunzip(filename)
  assertIntegerish(ext, len=1)
  assertFlag(header)
  
  temp = Rfits_read_header(filename=filename, ext=ext, zap=zap)
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
  
  output = list(filename=filename, ext=ext, keyvalues=keyvalues, raw=raw, header=header, dim=dim, type=type)
  class(output) = 'Rfits_pointer'
  return(invisible(output))
}

plot.Rfits_pointer=function(x, useraw=FALSE, ...){
  if(!inherits(x, 'Rfits_pointer')){
    stop('Object class is not of type Rfits_image!')
  }
  if(is.null(x$keyvalues$CRVAL1)){
    magimage(x[,,header=FALSE])
  }else{
    if(requireNamespace("Rwcs", quietly=TRUE)){
      if(useraw){header = x$raw}else{header = NULL}
      Rwcs::Rwcs_image(x[,,header=FALSE], keyvalues=x$keyvalues, header=header, ...)
    }else{
      message('The Rwcs package is needed to plot a Rfits_image object.')
    }
  }
}
