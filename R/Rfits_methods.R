Rfits_point=function(filename='temp.fits', ext=1, header=FALSE){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  assertAccess(filename, access='r')
  assertIntegerish(ext, len=1)
  assertFlag(header)
  
  keyvalues = Rfits_read_header(filename=filename, ext=ext)$keyvalues
  
  output = list(filename=filename, ext=ext, keyvalues=keyvalues, header=header)
  class(output) = 'Rfits_image_pointer'
  return(invisible(output))
}

print.Rfits_vector=function(x , ...){
  cat('Class: Rfits_image\n')
  cat('File path:',x$filename,'\n')
  cat('Ext num:',x$ext,'\n')
  cat('Ext name:',x$keyvalues[['EXTNAME']],'\n')
  cat('RAM size:',round(object.size(x)/(2^20),4),'MB\n')
  cat('BITPIX:',x$keyvalues[['BITPIX']],'\n')
  cat('NAXIS1:',x$keyvalues[['NAXIS1']],'\n')
}

print.Rfits_image=function(x , ...){
  cat('Class: Rfits_image\n')
  cat('File path:',x$filename,'\n')
  cat('Ext num:',x$ext,'\n')
  cat('Ext name:',x$keyvalues[['EXTNAME']],'\n')
  cat('RAM size:',round(object.size(x)/(2^20),4),'MB\n')
  cat('BITPIX:',x$keyvalues[['BITPIX']],'\n')
  cat('NAXIS1:',x$keyvalues[['NAXIS1']],'\n')
  cat('NAXIS2:',x$keyvalues[['NAXIS2']],'\n')
}

print.Rfits_cube=function(x , ...){
  cat('Class: Rfits_cube\n')
  cat('File path:',x$filename,'\n')
  cat('Ext num:',x$ext,'\n')
  cat('Ext name:',x$keyvalues[['EXTNAME']],'\n')
  cat('RAM size:',round(object.size(x)/(2^20),4),'MB\n')
  cat('BITPIX:',x$keyvalues[['BITPIX']],'\n')
  cat('NAXIS1:',x$keyvalues[['NAXIS1']],'\n')
  cat('NAXIS2:',x$keyvalues[['NAXIS2']],'\n')
  cat('NAXIS3:',x$keyvalues[['NAXIS3']],'\n')
}

print.Rfits_array=function(x , ...){
  cat('Class: Rfits_array\n')
  cat('File path:',x$filename,'\n')
  cat('Ext num:',x$ext,'\n')
  cat('Ext name:',x$keyvalues[['EXTNAME']],'\n')
  cat('RAM size:',round(object.size(x)/(2^20),4),'MB\n')
  cat('BITPIX:',x$keyvalues[['BITPIX']],'\n')
  cat('NAXIS1:',x$keyvalues[['NAXIS1']],'\n')
  cat('NAXIS2:',x$keyvalues[['NAXIS2']],'\n')
  cat('NAXIS3:',x$keyvalues[['NAXIS3']],'\n')
  cat('NAXIS4:',x$keyvalues[['NAXIS4']],'\n')
}

print.Rfits_image_pointer=function(x , ...){
  cat('File path:',x$filename,'\n')
  cat('Ext num:',x$ext,'\n')
  cat('Ext name:',x$keyvalues[['EXTNAME']],'\n')
  cat('Class: Rfits_image_pointer\n')
  cat('Disk size:',round(file.size(x$filename)/(2^20),4),'MB\n')
  cat('BITPIX:',x$keyvalues[['BITPIX']],'\n')
  cat('NAXIS1:',x$keyvalues[['NAXIS1']],'\n')
  cat('NAXIS2:',x$keyvalues[['NAXIS2']],'\n')
}

print.Rfits_header=function(x, ...){
  cat(x$header[1:8], sep='\n')
}

print.Rfits_list=function(x , ...){
  
  ext_name={}
  for(i in 1:length(x)){
    if(is.null(x[[i]]$keyvalues$EXTNAME)){
      ext_name = c(ext_name, 'NA')
    }else{
      ext_name = c(ext_name, x[[i]]$keyvalues$EXTNAME)
    }
  }
  
  ext_class={}
  for(i in 1:length(x)){
    ext_class = c(ext_class, class(x[[i]])[1])
  }
  
  ext_dim={}
  for(i in 1:length(x)){
    if(is.null(dim(x[[i]]))){
      ext_dim = c(ext_dim, 'NA')
    }else{
      ext_dim = c(ext_dim, paste(dim(x[[i]]), collapse=' x '))
    }
  }
  
  ext_size={}
  for(i in 1:length(x)){
    ext_size = c(ext_size, round(object.size(x[[i]])/(2^20),4))
  }
  
  summarytable = data.frame(
    'Ext'= 1:length(x),
    'Name' = ext_name,
    'Class' = ext_class,
    'Dim' = ext_dim,
    'Size/MB' = ext_size
  )
  
  cat('Multi-extension FITS loaded with Rfits_read of class Rfits_list\n\n')
  cat('File location:',attributes(x)$filename,'\n\n')
  cat('Summary of extension contents:\n\n')
  print(summarytable)
}

print.Rfits_header=function(x, ...){
  cat(x$header[1:min(8,length(x$header))], sep='\n')
}

length.Rfits_vector=function(x){
  return(length(x$imDat))
}

dim.Rfits_image=function(x){
  return(dim(x$imDat))
}

dim.Rfits_cube=function(x){
  return(dim(x$imDat))
}

dim.Rfits_array=function(x){
  return(dim(x$imDat))
}

dim.Rfits_image_pointer=function(x){
  return(c(x$keyvalues$NAXIS1, x$keyvalues$NAXIS2))
}

`[.Rfits_vector` = function(x, i){
  return(x$imDat[i])
}

`[.Rfits_image` = function(x, i, j){
  return(x$imDat[i,j])
}

`[.Rfits_cube` = function(x, i, j, k){
  return(x$imDat[i,j,k])
}

`[.Rfits_array` = function(x, i, j, k, m){
  return(x$imDat[i,j,k,m])
}

`[.Rfits_image_pointer` = function(x, i, j, k, m){
  if(!missing(i)){
    if(is.null(x$keyvalues$NAXIS1)){stop('NAXIS1 is NULL: specifying too many dimensions!')}
    xlo=min(i)
    xhi=max(i)
    if(xlo < 1 | xhi < 1){stop('All i must be >= 1')}
    if(xlo > x$keyvalues$NAXIS1 | xhi > x$keyvalues$NAXIS1){stop('All i must be <=', x$keyvalues$NAXIS1)}
  }else{
    xlo=NULL
    xhi=NULL
  }
  if(!missing(j)){
    if(is.null(x$keyvalues$NAXIS2)){stop('NAXIS2 is NULL: specifying too many dimensions!')}
    ylo=min(j)
    yhi=max(j)
    if(ylo < 1 | yhi < 1){stop('All j must be >= 1')}
    if(ylo > x$keyvalues$NAXIS2 | yhi > x$keyvalues$NAXIS2){stop('All j must be <=', x$keyvalues$NAXIS2)}
  }else{
    ylo=NULL
    yhi=NULL
  }
  if(!missing(k)){
    if(is.null(x$keyvalues$NAXIS3)){stop('NAXIS3 is NULL: specifying too many dimensions!')}
    zlo=min(k)
    zhi=max(k)
    if(zlo < 1 | zhi < 1){stop('All k must be >= 1')}
    if(zlo > x$keyvalues$NAXIS3 | zhi > x$keyvalues$NAXIS3){stop('All j must be <=', x$keyvalues$NAXIS3)}
  }else{
    zlo=NULL
    zhi=NULL
  }
  if(!missing(m)){
    if(is.null(x$keyvalues$NAXIS4)){stop('NAXIS4 is NULL: specifying too many dimensions!')}
    tlo=min(m)
    thi=max(m)
    if(tlo < 1 | thi < 1){stop('All m must be >= 1')}
    if(tlo > x$keyvalues$NAXIS4 | thi > x$keyvalues$NAXIS4){stop('All j must be <=', x$keyvalues$NAXIS4)}
  }else{
    tlo=NULL
    thi=NULL
  }
  return(Rfits_read_image(filename=x$filename, ext=x$ext, header=x$header,
                          xlo=xlo, xhi=xhi, ylo=ylo, yhi=yhi, zlo=zlo, zhi=zhi,
                          tlo=tlo, thi=thi))
}

`&.Rfits_image_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image_pointer'))
    e2 = e2[,]
  e1[,] & e2
}

`|.Rfits_image_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image_pointer'))
    e2 = e2[,]
  e1[,] | e2
}

`!=.Rfits_image_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image_pointer'))
    e2 = e2[,]
  e1[,] != e2
}

`==.Rfits_image_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image_pointer'))
    e2 = e2[,]
  e1[,] == e2
}

`<.Rfits_image_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image_pointer'))
    e2 = e2[,]
  e1[,] < e2
}

`<=.Rfits_image_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image_pointer'))
    e2 = e2[,]
  e1[,] <= e2
}

`>.Rfits_image_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image_pointer'))
    e2 = e2[,]
  e1[,] > e2
}

`>=.Rfits_image_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image_pointer'))
    e2 = e2[,]
  e1[,] >= e2
}

`+.Rfits_image_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image_pointer'))
    e2 = e2[,]
  e1[,] + e2
}

`-.Rfits_image_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image_pointer'))
    e2 = e2[,]
  e1[,] - e2
}

`*.Rfits_image_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image_pointer'))
    e2 = e2[,]
  e1[,] * e2
}

`/.Rfits_image_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image_pointer'))
    e2 = e2[,]
  e1[,] / e2
}

`^.Rfits_image_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image_pointer'))
    e2 = e2[,]
  e1[,] ^ e2
}

`%%.Rfits_image_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image_pointer'))
    e2 = e2[,]
  e1[,] %% e2
}

`%/%.Rfits_image_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image_pointer'))
    e2 = e2[,]
  e1[,] %/% e2
}

`%*%.Rfits_image_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image_pointer'))
    e2 = e2[,]
  e1[,] %*% e2
}
