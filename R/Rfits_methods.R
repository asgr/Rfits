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

print.Rfits_image=function(x , ...){
  cat('File path:',x$filename,'\n')
  cat('Ext num:',x$ext,'\n')
  cat('Ext name:',x$keyvalues[['EXTNAME']],'\n')
  cat('Class: Rfits_image\n')
  cat('RAM size:',round(object.size(x)/(2^20),4),'Mb\n')
  cat('BITPIX:',x$keyvalues[['BITPIX']],'\n')
  cat('NAXIS1:',x$keyvalues[['NAXIS1']],'\n')
  cat('NAXIS2:',x$keyvalues[['NAXIS2']],'\n')
}

print.Rfits_cube=function(x , ...){
  cat('File path:',x$filename,'\n')
  cat('Ext num:',x$ext,'\n')
  cat('Ext name:',x$keyvalues[['EXTNAME']],'\n')
  cat('Class: Rfits_image\n')
  cat('RAM size:',round(object.size(x)/(2^20),4),'Mb\n')
  cat('BITPIX:',x$keyvalues[['BITPIX']],'\n')
  cat('NAXIS1:',x$keyvalues[['NAXIS1']],'\n')
  cat('NAXIS2:',x$keyvalues[['NAXIS2']],'\n')
  cat('NAXIS3:',x$keyvalues[['NAXIS3']],'\n')
}

print.Rfits_image_pointer=function(x , ...){
  cat('File path:',x$filename,'\n')
  cat('Ext num:',x$ext,'\n')
  cat('Ext name:',x$keyvalues[['EXTNAME']],'\n')
  cat('Class: Rfits_image_pointer\n')
  cat('Disk size:',round(file.size(x$filename)/(2^20),4),'Mb\n')
  cat('BITPIX:',x$keyvalues[['BITPIX']],'\n')
  cat('NAXIS1:',x$keyvalues[['NAXIS1']],'\n')
  cat('NAXIS2:',x$keyvalues[['NAXIS2']],'\n')
}

dim.Rfits_image=function(x){
  return(dim(x$imDat))
}

dim.Rfits_cube=function(x){
  return(dim(x$imDat))
}

dim.Rfits_image_pointer=function(x){
  return(c(x$keyvalues$NAXIS1, x$keyvalues$NAXIS2))
}

`[.Rfits_image_pointer` = function(x, i, j){
  if(!missing(i)){
    xlo=min(i)
    xhi=max(i)
    if(xlo < 1 | xhi < 1){stop('All i must be >= 1')}
    if(xlo > x$keyvalues$NAXIS1 | xhi > x$keyvalues$NAXIS1){stop('All i must be <=', x$keyvalues$NAXIS1)}
  }else{
    xlo=NULL
    xhi=NULL
  }
  if(!missing(j)){
    ylo=min(j)
    yhi=max(j)
    if(ylo < 1 | yhi < 1){stop('All i must be >= 1')}
    if(ylo > x$keyvalues$NAXIS2 | yhi > x$keyvalues$NAXIS2){stop('All j must be <=', x$keyvalues$NAXIS2)}
  }else{
    ylo=NULL
    yhi=NULL
  }
  return(Rfits_read_image(filename=x$filename, ext=x$ext, header=x$header,  xlo=xlo, xhi=xhi, ylo=ylo, yhi=yhi))
}

`[.Rfits_image` = function(x, i, j){
  return(x$imDat[i,j])
}

`[.Rfits_cube` = function(x, i, j, k){
  return(x$imDat[i,j,k])
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
