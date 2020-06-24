Rfits_read_all=function(filename='temp.fits', pointer=FALSE){
  info = Rfits_info(filename)
  
  data = rep(list(), length(info$summary))
  
  #images
  
  if(!is.null(info$headers[[1]]$keyvalues$NAXIS1) & !is.null(info$headers[[1]]$keyvalues$NAXIS2)){
    if(pointer){
      data[[1]] = Rfits_point(filename, ext=1)
    }else{
      data[[1]] = Rfits_read_image(filename, ext=1)
    }
  }
  
  sel_images = grep('IMAGE',info$summary)
  sel_images = sel_images[sel_images>1]
  if(length(sel_images)>0){
    for(i in sel_images){
      if(pointer){
        data[[i]] = Rfits_point(filename, ext=i, header=TRUE)
      }else{
        data[[i]] = Rfits_read_image(filename, ext=i, header=TRUE)
      }
    }
  }
  
  #tables
  
  sel_tables = grep('TABLE',info$summary)
  if(length(sel_tables)>0){
    for(i in sel_tables){
      data[[i]] = Rfits_read_table(filename, ext=i, header=TRUE)
    }
  }
  
  class(data) = 'Rfits_list'
  return(invisible(data))
}

Rfits_read = Rfits_read_all

Rfits_write_all=function(data, filename='temp.fits'){
  assertList(data)
  
  create_file = TRUE
  overwrite_file = TRUE
  
  for(i in 1:length(data)){

    if(!is.null(data[[i]])){
      if(is.array(data[[i]]$imDat)){
        Rfits_write_image(data=data[[i]], filename=filename, ext=i,
                            create_file=create_file, overwrite_file=overwrite_file)
      }else if(is.data.frame(data[[i]])){
        Rfits_write_table(table=data[[i]], filename=filename, ext=i,
                          create_file=create_file, overwrite_file=overwrite_file)
      }else{
        stop("Data type in extension ",i," is not supported!")
      }
      create_file = FALSE
      overwrite_file = FALSE
    }else{
      message('Extension ',i,' is NULL and will not be written to FITS!')
    }
  }
}

Rfits_write = Rfits_write_all
