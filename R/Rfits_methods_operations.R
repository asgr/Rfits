`&.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat & e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat & e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat & e2
  }
  return(e1)
}

`|.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat | e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat | e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat | e2
  }
  return(e1)
}

`!=.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat != e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat != e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat != e2
  }
  return(e1)
}

`==.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat == e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat == e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat == e2
  }
  return(e1)
}

`<.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat < e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat < e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat < e2
  }
  return(e1)
}

`<=.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat <= e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat <= e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat <= e2
  }
  return(e1)
}

`>.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat > e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat > e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat > e2
  }
  return(e1)
}

`>=.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat >= e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat >= e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat >= e2
  }
  return(e1)
}

`+.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat + e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat + e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat + e2
  }
  return(e1)
}

`-.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat - e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat - e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat - e2
  }
  return(e1)
}

`*.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat * e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat * e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat * e2
  }
  return(e1)
}

`/.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat / e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat / e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat / e2
  }
  return(e1)
}

`^.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat ^ e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat ^ e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat ^ e2
  }
  return(e1)
}

`%%.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat %% e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat %% e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat %% e2
  }
  return(e1)
}

`%*%.Rfits_image`=function(x, y){
  if (missing(y)) 
    return(x)
  if (inherits(y, 'Rfits_image')){
    x$imDat =  x$imDat %*% y$imDat
  }else if (inherits(y, 'Rfits_pointer')){
    x$imDat =  x$imDat %*% y[,]$imDat
  }else{
    x$imDat =  x$imDat %*% y
  }
  return(x)
}

`%/%.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat %/% e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat %/% e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat %/% e2
  }
  return(e1)
}

`&.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] & e2[,])
  }else{
    return(e1[,] & e2)
  }
}

`|.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] | e2[,])
  }else{
    return(e1[,] | e2)
  }
}

`!=.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] != e2[,])
  }else{
    return(e1[,] != e2)
  }
}

`==.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] == e2[,])
  }else{
    return(e1[,] == e2)
  }
}

`<.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] < e2[,])
  }else{
    return(e1[,] < e2)
  }
}

`<=.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] <= e2[,])
  }else{
    return(e1[,] <= e2)
  }
}

`>.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] > e2[,])
  }else{
    return(e1[,] > e2)
  }
}

`>=.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] >= e2[,])
  }else{
    return(e1[,] >= e2)
  }
}

`+.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] + e2[,])
  }else{
    return(e1[,] + e2)
  }
}

`-.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] - e2[,])
  }else{
    return(e1[,] - e2)
  }
}

`*.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] * e2[,])
  }else{
    return(e1[,] * e2)
  }
}

`/.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] / e2[,])
  }else{
    return(e1[,] / e2)
  }
}

`^.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] ^ e2[,])
  }else{
    return(e1[,] ^ e2)
  }
}

`%%.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] %% e2[,])
  }else{
    return(e1[,] %% e2)
  }
}

`%*%.Rfits_pointer`=function(x, y){
  if (inherits(y, 'Rfits_pointer')){
    return(x[,] %*% y[,])
  }else{
    return(x[,] %*% y)
  }
}

`%/%.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] %/% e2[,])
  }else{
    return(e1[,] %/% e2)
  }
}
