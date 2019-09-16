Rfits_read_table=function(file, ext=2, data.table=TRUE){
  ncol=Rfits_read_ncol(file)
  output=list()
  
  for(i in 1:ncol){
    output[[i]]=Rfits_read_col(file,colref=i)
  }
  
  if(data.table){
    output=data.table::as.data.table(output)
    colnames(output)=Rfits_read_colname(file, ext=ext)
  }else{
    output=as.data.frame(output)
    colnames(output)=Rfits_read_colname(file, ext=ext)
  }
  
  return(invisible(output))
}

Rfits_write_table=function(data, file, ext=2){
  nrow=dim(data)[1]
  ncol=dim(data)[2]
  check.int=sapply(data,is.integer)
  check.integer64=sapply(data,is.integer64)
  check.double=sapply(data,is.numeric) & (! check.integer64)
  check.char=sapply(data,is.character)
  
  tform=character(ncol)
  tform[check.int]="1J"
  tform[check.integer64]='1K'
  tform[check.double]="1D"
  tfrom[check.char]=paste(sapply(data[,check.char],function(x) max(nchar(x))), 'A', sep='')
  
}