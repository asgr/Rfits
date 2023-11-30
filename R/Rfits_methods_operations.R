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

`%*%.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat %*% e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat %*% e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat %*% e2
  }
  return(e1)
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

`%*%.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] %*% e2[,])
  }else{
    return(e1[,] %*% e2)
  }
}

`%/%.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] %/% e2[,])
  }else{
    return(e1[,] %/% e2)
  }
}
