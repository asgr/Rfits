Rfits_read_all=function(filename='temp.fits', pointer=FALSE){
  assertCharacter(filename, max.len=1)
  filename=path.expand(filename)
  assertFlag(pointer)
  
  info = Rfits_info(filename)
  
  data = vector(mode='list', length=length(info$summary))
  
  #images
  
  if(!is.null(info$headers[[1]]$keyvalues$NAXIS1)){
    if(pointer){
      data[[1]] = Rfits_point(filename, ext=1)
    }else{
      data[[1]] = Rfits_read_image(filename, ext=1)
    }
  }
  
  sel_images = which(grepl('IMAGE',info$summary) | grepl('HDRLET',info$summary))
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
  
  #NULL
  
  if(length(data) == 0){
    data = list(Rfits_read_header(filename, ext=1))
  }else{
    for(i in 1:length(data)){
      if(is.null(data[[i]])){
        data[[i]] = Rfits_read_header(filename, ext=i)
      }
    }
  }
  
  #names
  
  names(data) = rep(NA, length(data))
  for(i in 1:length(data)){
    if(!is.null(data[[i]]$keyvalues$EXTNAME)){
      names(data)[i] = data[[i]]$keyvalues$EXTNAME
    }
  }
  
  class(data) = 'Rfits_list'
  attributes(data)$filename = filename
  return(invisible(data))
}

Rfits_read = Rfits_read_all

.flatten <- function(x) {
  if (!inherits(x, "list")) return(list(x))
  else return(unlist(c(lapply(x, .flatten)), recursive = FALSE))
}

Rfits_write_all=function(data, filename='temp.fits'){
  assertList(data)
  assertCharacter(filename, max.len=1)
  
  create_file = TRUE
  overwrite_file = TRUE
  
  data = .flatten(data)
  
  for(i in 1:length(data)){
    if(inherits(data[[i]], c('Rfits_image', 'Rfits_image_pointer', 'array', 'matrix', 'integer', 'numeric'))){
      Rfits_write_image(data=data[[i]], filename=filename, ext=i,
                        create_file=create_file, overwrite_file=overwrite_file)
      if(is.list(data[[i]])){
        if(is.null(data[[i]]$keyvalues$EXTNAME)){
          Rfits_write_key(filename=filename, keyname='EXTNAME', keyvalue=names(data)[i], ext=i)
        }
      }else{
        Rfits_write_key(filename=filename, keyname='EXTNAME', keyvalue=names(data)[i], ext=i)
      }
      create_file = FALSE
      overwrite_file = FALSE
    }else if(inherits(data[[i]], c('Rfits_table', 'data.frame', 'data.table'))){
      Rfits_write_table(table=data[[i]], filename=filename, ext=i,
                        create_file=create_file, overwrite_file=overwrite_file)
      if(is.list(data[[i]])){
        if(is.null(data[[i]]$keyvalues$EXTNAME)){
          Rfits_write_key(filename=filename, keyname='EXTNAME', keyvalue=names(data)[i], ext=i)
        }
      }else{
        Rfits_write_key(filename=filename, keyname='EXTNAME', keyvalue=names(data)[i], ext=i)
      }
      create_file = FALSE
      overwrite_file = FALSE
    }else if(inherits(data[[i]], 'Rfits_header')){
      Rfits_write_header(filename=filename, keyvalues=data[[i]]$keyvalues, keycomments=data[[i]]$keycomments,
                         comment=data[[i]]$comments, history=data[[i]]$history, create_ext=TRUE,
                         create_file=create_file, overwrite_file=overwrite_file)
      if(is.list(data[[i]])){
        if(is.null(data[[i]]$keyvalues$EXTNAME)){
          Rfits_write_key(filename=filename, keyname='EXTNAME', keyvalue=names(data)[i], ext=i)
        }
      }else{
        Rfits_write_key(filename=filename, keyname='EXTNAME', keyvalue=names(data)[i], ext=i)
      }
      create_file = FALSE
      overwrite_file = FALSE
    }else{
      message('Extension ',i,' is not recognised and will not be written to FITS!')
    }
  }
}

Rfits_write = Rfits_write_all
