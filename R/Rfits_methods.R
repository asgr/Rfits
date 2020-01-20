Rfits_point=function(filename, ext=1, header=FALSE){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  assertAccess(filename, access='r')
  assertIntegerish(ext, len = 1)
  assertLogical(header)
  
  keyvalues = Rfits_read_header(filename = filename, ext = ext)$keyvalues
  
  output = list(filename=filename, ext=ext, keyvalues=keyvalues, header=header)
  class(output) = 'Rfits_image_pointer'
  return(invisible(output))
}

print.Rfits_image_pointer=function(x , ...){
  cat('File path:',x$filename,'\n')
  cat('Extension:',x$ext,'\n')
  cat('BITPIX:',x$keyvalues[['BITPIX']],'\n')
  cat('NAXIS1:',x$keyvalues[['NAXIS1']],'\n')
  cat('NAXIS2:',x$keyvalues[['NAXIS2']],'\n')
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