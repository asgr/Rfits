Rfits_point=function(filename, ext=1, header=FALSE){
  assertCharacter(filename, max.len=1)
  filename=path.expand(filename)
  assertAccess(filename, access='r')
  assertIntegerish(ext, len = 1)
  assertLogical(header)
  
  output = list(filename=filename, ext=ext, header=header)
  class(output) = 'Rfits_image'
  return(invisible(output))
}

`[.Rfits_image` = function(x, i, j){
  if(!missing(i)){
    xlo=min(i)
    xhi=max(i)
  }else{
    xlo=NULL
    xhi=NULL
  }
  if(!missing(j)){
    ylo=min(j)
    yhi=max(j)
  }else{
    ylo=NULL
    yhi=NULL
  }
  return(Rfits_read_image(filename=x$filename, ext=x$ext, header=x$header,  xlo=xlo, xhi=xhi, ylo=ylo, yhi=yhi))
}

`&.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image'))
    e2 = e2[,]
  e1[,] & e2
}

`|.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image'))
    e2 = e2[,]
  e1[,] | e2
}

`!=.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image'))
    e2 = e2[,]
  e1[,] != e2
}

`==.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image'))
    e2 = e2[,]
  e1[,] == e2
}

`<.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image'))
    e2 = e2[,]
  e1[,] < e2
}

`<=.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image'))
    e2 = e2[,]
  e1[,] <= e2
}

`>.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image'))
    e2 = e2[,]
  e1[,] > e2
}

`>=.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image'))
    e2 = e2[,]
  e1[,] >= e2
}

`+.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image'))
    e2 = e2[,]
  e1[,] + e2
}

`-.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image'))
    e2 = e2[,]
  e1[,] - e2
}

`*.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image'))
    e2 = e2[,]
  e1[,] * e2
}

`/.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image'))
    e2 = e2[,]
  e1[,] / e2
}

`^.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image'))
    e2 = e2[,]
  e1[,] ^ e2
}

`%%.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image'))
    e2 = e2[,]
  e1[,] %% e2
}

`%/%.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image'))
    e2 = e2[,]
  e1[,] %/% e2
}

`%*%.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image'))
    e2 = e2[,]
  e1[,] %*% e2
}