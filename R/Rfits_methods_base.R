#plotting

plot.Rfits_image = function(x, useraw=TRUE, interactive=FALSE, ...){
  if(!inherits(x, 'Rfits_image')){
    stop('Object class is not of type Rfits_image!')
  }
  if(is.null(x$keyvalues$CRVAL1)){
    magimage(x$imDat, ...)
  }else{
    if(requireNamespace("Rwcs", quietly=TRUE)){
      if(useraw){header = x$raw}else{header = NULL}
      Rwcs::Rwcs_image(x, interactive=interactive, ...)
    }else{
      message('The Rwcs package is needed to plot a Rfits_image object.')
    }
  }
}

plot.Rfits_cube = function(x, slice=1, useraw=TRUE, ...){
  if(!inherits(x, 'Rfits_cube')){
    stop('Object class is not of type Rfits_cube!')
  }
  if(requireNamespace("Rwcs", quietly=TRUE)){
    if(useraw){header = x$raw}else{header = NULL}
    Rwcs::Rwcs_image(x$imDat[,,slice], keyvalues=x$keyvalues, header=header, ...)
  }else{
    message('The Rwcs package is needed to plot a Rfits_cube object.')
  }
}

plot.Rfits_array = function(x, slice=c(1,1), useraw=TRUE, ...){
  if(!inherits(x, 'Rfits_array')){
    stop('Object class is not of type Rfits_array!')
  }
  if(requireNamespace("Rwcs", quietly=TRUE)){
    if(useraw){header = x$raw}else{header = NULL}
    Rwcs::Rwcs_image(x$imDat[,,slice[1],slice[2]], keyvalues=x$keyvalues, header=header, ...)
  }else{
    message('The Rwcs package is needed to plot a Rfits_cube object.')
  }
}

plot.Rfits_vector = function(x, ...){
  if(!inherits(x, 'Rfits_vector')){
    stop('Object class is not of type Rfits_vector!')
  }
  xref = 1:length(x$imDat)
  if(!is.null(x$keyvalues$CRPIX1)){
    xref = xref - x$keyvalues$CRPIX1
  }
  if(!is.null(x$keyvalues$CDELT1)){
    xref = xref * x$keyvalues$CDELT1
  }
  if(!is.null(x$keyvalues$CRVAL1)){
    xref = xref + x$keyvalues$CRVAL1
  }
  if(requireNamespace("magicaxis", quietly=TRUE)){
    magicaxis::magplot(xref, x$imDat, type='l', ...)
  }else{
    plot(xref, x$imDat, type='l', ...)
  }
}

plot.Rfits_pointer=function(x, useraw=TRUE, sparse='auto', interactive=FALSE, ...){
  if(!inherits(x, 'Rfits_pointer')){
    stop('Object class is not of type Rfits_image!')
  }
  
  if(sparse == 'auto'){
    sparse = ceiling(max(dim(x)/1e3))
  }
  
  if(is.null(x$keyvalues$CRVAL1)){
    magimage(x[,,sparse=sparse,header=FALSE], sparse=1L, ...)
  }else{
    if(interactive){
      assign(".current_image", x, envir = .GlobalEnv)
    }
    Rwcs::Rwcs_image(x[,,sparse=sparse], useraw=useraw, interactive=FALSE, ...)
  }
}


lines.Rfits_vector = function(x, ...){
  if(!inherits(x, 'Rfits_vector')){
    stop('Object class is not of type Rfits_vector!')
  }
  xref = 1:length(x$imDat)
  if(!is.null(x$keyvalues$CRPIX1)){
    xref = xref - x$keyvalues$CRPIX1
  }
  if(!is.null(x$keyvalues$CDELT1)){
    xref = xref * x$keyvalues$CDELT1
  }
  if(!is.null(x$keyvalues$CRVAL1)){
    xref = xref + x$keyvalues$CRVAL1
  }
  lines(xref, x$imDat, ...)
}

#print

print.Rfits_vector=function(x , ...){
  
  if(isTRUE(x$keyvalues$ZIMAGE)){
    naxis1 = x$keyvalues$ZNAXIS1
    datatype = x$keyvalues$ZBITPIX
  }else{
    naxis1 = x$keyvalues$NAXIS1
    datatype = x$keyvalues$BITPIX
  }
  
  cat('Class: Rfits_vector\n')
  cat('File path:',x$filename,'\n')
  cat('Ext num:',x$ext,'\n')
  cat('Ext name:',x$keyvalues[['EXTNAME']],'\n')
  cat('RAM size:',round(object.size(x)/(2^20),4),'MB\n')
  cat('BITPIX:',datatype,'\n')
  cat('NAXIS1:',naxis1,'\n')
  cat('Key N:',length(x$keyvalues),'\n')
}

print.Rfits_image=function(x , ...){
  
  if(isTRUE(x$keyvalues$ZIMAGE)){
    naxis1 = x$keyvalues$ZNAXIS1
    naxis2 = x$keyvalues$ZNAXIS2
    datatype = x$keyvalues$ZBITPIX
  }else{
    naxis1 = x$keyvalues$NAXIS1
    naxis2 = x$keyvalues$NAXIS2
    datatype = x$keyvalues$BITPIX
  }
  
  cat('Class: Rfits_image\n')
  cat('File path:',x$filename,'\n')
  cat('Ext num:',x$ext,'\n')
  cat('Ext name:',x$keyvalues[['EXTNAME']],'\n')
  cat('RAM size:',round(object.size(x)/(2^20),4),'MB\n')
  cat('BITPIX:',datatype,'\n')
  cat('NAXIS1:',naxis1,'\n')
  cat('NAXIS2:',naxis2,'\n')
  cat('Key N:',length(x$keyvalues),'\n')
}

print.Rfits_cube=function(x , ...){
  
  if(isTRUE(x$keyvalues$ZIMAGE)){
    naxis1 = x$keyvalues$ZNAXIS1
    naxis2 = x$keyvalues$ZNAXIS2
    naxis3 = x$keyvalues$ZNAXIS3
    datatype = x$keyvalues$ZBITPIX
  }else{
    naxis1 = x$keyvalues$NAXIS1
    naxis2 = x$keyvalues$NAXIS2
    naxis3 = x$keyvalues$NAXIS3
    datatype = x$keyvalues$BITPIX
  }
  
  cat('Class: Rfits_cube\n')
  cat('File path:',x$filename,'\n')
  cat('Ext num:',x$ext,'\n')
  cat('Ext name:',x$keyvalues[['EXTNAME']],'\n')
  cat('RAM size:',round(object.size(x)/(2^20),4),'MB\n')
  cat('BITPIX:',datatype,'\n')
  cat('NAXIS1:',naxis1,'\n')
  cat('NAXIS2:',naxis2,'\n')
  cat('NAXIS3:',naxis3,'\n')
  cat('Key N:',length(x$keyvalues),'\n')
}

print.Rfits_array=function(x , ...){
  
  if(isTRUE(x$keyvalues$ZIMAGE)){
    naxis1 = x$keyvalues$ZNAXIS1
    naxis2 = x$keyvalues$ZNAXIS2
    naxis3 = x$keyvalues$ZNAXIS3
    naxis4 = x$keyvalues$ZNAXIS4
    datatype = x$keyvalues$ZBITPIX
  }else{
    naxis1 = x$keyvalues$NAXIS1
    naxis2 = x$keyvalues$NAXIS2
    naxis3 = x$keyvalues$NAXIS3
    naxis4 = x$keyvalues$NAXIS4
    datatype = x$keyvalues$BITPIX
  }
  
  cat('Class: Rfits_array\n')
  cat('File path:',x$filename,'\n')
  cat('Ext num:',x$ext,'\n')
  cat('Ext name:',x$keyvalues[['EXTNAME']],'\n')
  cat('RAM size:',round(object.size(x)/(2^20),4),'MB\n')
  cat('BITPIX:',datatype,'\n')
  cat('NAXIS1:',naxis1,'\n')
  cat('NAXIS2:',naxis2,'\n')
  cat('NAXIS3:',naxis3,'\n')
  cat('NAXIS4:',naxis4,'\n')
  cat('Key N:',length(x$keyvalues),'\n')
}

print.Rfits_pointer=function(x , ...){
  cat('File path:',x$filename,'\n')
  cat('Ext num:',x$ext,'\n')
  cat('Ext name:',x$keyvalues[['EXTNAME']],'\n')
  cat('Class: Rfits_pointer\n')
  cat('Type:',x$type,'\n')
  cat('Dim:',x$dim,'\n')
  cat('Disk size:',round(file.size(x$filename)/(2^20),4),'MB\n')
  cat('BITPIX:',x$keyvalues[['BITPIX']],'\n')
  cat('Key N:',length(x$keyvalues),'\n')
}

print.Rfits_header=function(x, ...){
  cat(x$header[1:min(8,length(x$header))], sep='\n')
  cat("...", sep='\n')
  cat('Key N:',length(x$keyvalues),'\n')
}

# print.Rfits_keylist=function(x, ...){
#   cat(x[1:min(8,length(x))], sep='\n')
#   cat("...", sep='\n')
#   cat('Key N:',length(x),'\n')
# }

print.Rfits_list=function(x , ...){
  
  ext_name = names(x)
  if(is.null(ext_name)){
    ext_name = rep(NA, length(x))
  }
  ext_dim = {}
  ext_class = {}
  ext_mode = {}
  ext_type = {}
  ext_size = {}
  ext_keyN = {}
  
  for(i in 1:length(x)){
    if(inherits(x[[i]], 'Rfits_vector')){
      ext_dim = c(ext_dim, length(x[[i]]))
    }else{
      if(is.null(dim(x[[i]]))){
        if(is.null(length(x[[i]]))){
          ext_dim = c(ext_dim, 'NA')
        }else{
          ext_dim = c(ext_dim, length(x[[i]]))
        }
      }else{
        ext_dim = c(ext_dim, paste(dim(x[[i]]), collapse=' x '))
      }
    }
    
    ext_class = c(ext_class, class(x[[i]])[1])
    ext_mode = c(ext_mode, mode(x[[i]]))
    ext_type = c(ext_type, typeof(x[[i]]))
    ext_size = c(ext_size, round(object.size(x[[i]])/(2^20),4))
    if(inherits(x[[i]], what=c('Rfits_image', 'Rfits_cube', 'Rfits_array', 'Rfits_vector', 'Rfits_pointer', 'Rfits_header'))){
      ext_keyN = c(ext_keyN, length(x[[i]]$keyvalues))
    }else if(inherits(x[[i]], what='Rfits_table')){
      ext_keyN = c(ext_keyN, length(attributes(x[[i]])$keyvalues))
    }else{
      ext_keyN = c(ext_keyN, NA)
    }
  }
  
  summarytable = data.frame(
    'Ext'= 1:length(x),
    'Name' = ext_name,
    'Class' = ext_class,
    'Mode' = ext_mode,
    'Type' = ext_type,
    'Dim' = ext_dim,
    'Size/MB' = ext_size,
    'Key.N' = ext_keyN
  )
  
  cat('Multi-extension FITS loaded with Rfits_read of class Rfits_list\n\n')
  cat('File location:',attributes(x)$filename,'\n\n')
  cat('Summary of extension contents:\n\n')
  print(summarytable)
}

#length

length.Rfits_vector=function(x){
  return(dim(x))
}

length.Rfits_image=function(x){
  if(is.null(dim(x))){
    return(length(x$imDat))
  }else{
    return(prod(dim(x)))
  }
}

length.Rfits_cube = length.Rfits_image
length.Rfits_array = length.Rfits_image
length.Rfits_pointer = length.Rfits_image
length.Rfits_header = length.Rfits_image
#length.Rfits_keylist = length.Rfits_image I think length might be confusing (probably just want to now the number of entries)

#dim

dim.Rfits_image = function(x){
  if(inherits(x$imDat, 'array')){
    return(dim(x$imDat))
  }else{
    return(length(x$imDat))
  }
}

dim.Rfits_vector = dim.Rfits_image
dim.Rfits_cube = dim.Rfits_image
dim.Rfits_array = dim.Rfits_image

dim.Rfits_pointer=function(x){
  return(x$dim)
}

dim.Rfits_header=function(x){
  if(isTRUE(x$keyvalues$ZIMAGE)){
    if(!is.null(x$keyvalues$ZNAXIS1)){
      NAXIS1 = x$keyvalues$ZNAXIS1
    }else{
      NAXIS1 = NULL
    }
    if(!is.null(x$keyvalues$ZNAXIS2)){
      NAXIS2 = x$keyvalues$ZNAXIS2
    }else{
      NAXIS2 = NULL
    }
    if(!is.null(x$keyvalues$ZNAXIS3)){
      NAXIS3 = x$keyvalues$ZNAXIS3
    }else{
      NAXIS3 = NULL
    }
    if(!is.null(x$keyvalues$ZNAXIS4)){
      NAXIS4 = x$keyvalues$ZNAXIS4
    }else{
      NAXIS4 = NULL
    }
  }else{
    if(!is.null(x$keyvalues$NAXIS1)){
      NAXIS1 = x$keyvalues$NAXIS1
    }else{
      NAXIS1 = NULL
    }
    if(!is.null(x$keyvalues$NAXIS2)){
      NAXIS2 = x$keyvalues$NAXIS2
    }else{
      NAXIS2 = NULL
    }
    if(!is.null(x$keyvalues$NAXIS3)){
      NAXIS3 = x$keyvalues$NAXIS3
    }else{
      NAXIS3 = NULL
    }
    if(!is.null(x$keyvalues$NAXIS4)){
      NAXIS4 = x$keyvalues$NAXIS4
    }else{
      NAXIS4 = NULL
    }
  }
  return(c(NAXIS1, NAXIS2, NAXIS3, NAXIS4))
}

dim.Rfits_keylist=function(x){
  if(isTRUE(x$ZIMAGE)){
    if(!is.null(x$ZNAXIS1)){
      NAXIS1 = x$ZNAXIS1
    }else{
      NAXIS1 = NULL
    }
    if(!is.null(x$ZNAXIS2)){
      NAXIS2 = x$ZNAXIS2
    }else{
      NAXIS2 = NULL
    }
    if(!is.null(x$ZNAXIS3)){
      NAXIS3 = x$ZNAXIS3
    }else{
      NAXIS3 = NULL
    }
    if(!is.null(x$ZNAXIS4)){
      NAXIS4 = x$ZNAXIS4
    }else{
      NAXIS4 = NULL
    }
  }else{
    if(!is.null(x$NAXIS1)){
      NAXIS1 = x$NAXIS1
    }else{
      NAXIS1 = NULL
    }
    if(!is.null(x$NAXIS2)){
      NAXIS2 = x$NAXIS2
    }else{
      NAXIS2 = NULL
    }
    if(!is.null(x$NAXIS3)){
      NAXIS3 = x$NAXIS3
    }else{
      NAXIS3 = NULL
    }
    if(!is.null(x$NAXIS4)){
      NAXIS4 = x$NAXIS4
    }else{
      NAXIS4 = NULL
    }
  }
  return(c(NAXIS1, NAXIS2, NAXIS3, NAXIS4))
}
