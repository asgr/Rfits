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

