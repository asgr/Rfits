Rfits_read_table=function(filename, ext=2, data.table=TRUE){
  assertCharacter(filename, max.len=1)
  filename=path.expand(filename)
  assertAccess(filename, access='r')
  assertIntegerish(ext, len = 1)
  assertFlag(data.table)
  
  ncol=Cfits_read_ncol(filename)
  output=list()
  
  for(i in 1:ncol){
    output[[i]]=Cfits_read_col(filename,colref=i)
  }
  
  if(data.table){
    output=data.table::as.data.table(output)
    colnames(output)=Cfits_read_colname(filename, ext=ext)
  }else{
    output=as.data.frame(output)
    colnames(output)=Cfits_read_colname(filename, ext=ext)
  }
  
  return(invisible(output))
}

Rfits_write_table=function(data, filename, extname='Main', tunits=rep('\01', dim(data)[2]), overwrite=TRUE){
  assertDataFrame(data, min.rows = 1, min.cols = 1)
  assertCharacter(filename, max.len = 1)
  filename=path.expand(filename)
  assertPathForOutput(filename, overwrite=overwrite)
  if(testFileExists(filename) & overwrite){
    file.remove(filename)
  }
  assertCharacter(extname, max.len = 1)
  
  nrow=dim(data)[1]
  ncol=dim(data)[2]
  
  ttypes=colnames(data)
  
  check.int=sapply(data,is.integer)
  check.integer64=sapply(data,is.integer64)
  check.double=sapply(data,is.numeric) & (! check.int) & (! check.integer64)
  check.char=sapply(data,is.character)
  
  tforms=character(ncol)
  tforms[check.int]="1J" # will become typecode = TINT = 31
  tforms[check.integer64]='1K' # will become typecode = TLONGLONG = 81
  tforms[check.double]="1D" # will become typecode = TDOUBLE = 82
  tforms[check.char]=paste(sapply(data[,check.char],function(x) max(nchar(x))+1), 'A', sep='') # will become typecode = TSTRING = 16
  
  if(length(grep('1K|1J|1D|A',tforms)) != ncol){
    stop(paste('Unrecognised column data type in column',which(!1:ncol %in% grep('1K|1J|1D|A',tforms))))
  }
  
  typecode=rep(0, ncol)
  typecode[check.int]=31
  typecode[check.integer64]=81
  typecode[check.double]=82
  typecode[check.char]=16
    
  assertCharacter(ttypes, len = ncol)
  assertCharacter(tforms, len = ncol)
  assertCharacter(tunits, len = ncol)
  
  Cfits_create_bintable(filename, tfields=ncol, ttypes=ttypes, tforms=tforms, tunits=tunits, extname=extname)
  for(i in 1:ncol){
    Cfits_write_col(filename = filename, data = data[[i]], nrow = nrow, colref = i, ext = 2, typecode = typecode[i])
  }
}
