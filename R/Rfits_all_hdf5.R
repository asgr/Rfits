Rfits_read_all_hdf5=function(filename='temp.h5', header=TRUE, data.table=TRUE){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  assertLogical(header)
  assertFlag(data.table)

  file.h5 = hdf5r::H5File$new(filename, mode='a')
  extnames = file.h5$names
  dataclass = as.character(file.h5$ls()$dataset.type_class)
  file.h5$close_all()
  
  data = vector(mode='list', length=length(extnames))
  
  if(length(header) == 1){
    header = rep(header, length(data))
  }
  
  #images
  
  sel_images = which(dataclass != 'H5T_COMPOUND')
  if(length(sel_images)>0){
    for(i in sel_images){
      data[[i]] = Rfits_read_image_hdf5(filename, ext=i, header=header[i])
    }
  }
  
  #tables
  
  sel_tables = which(dataclass == 'H5T_COMPOUND')
  if(length(sel_tables)>0){
    for(i in sel_tables){
      data[[i]] = Rfits_read_table_hdf5(filename, ext=i, header=header[i], data.table=data.table)
    }
  }
  
  #names
  
  names(data) = extnames
  
  class(data) = 'Rfits_list'
  attributes(data)$filename = filename
  return(invisible(data))
}

Rfits_read_hdf5 = Rfits_read_all_hdf5

Rfits_write_all_hdf5=function(data, filename='temp.h5', flatten=FALSE, list_sub=NULL){
  if(is.list(data)){
    data_len = length(data)
  }else{
    data_len = 1
  }
  assertCharacter(filename, max.len=1)
  assertFlag(flatten)

  overwrite_file = TRUE
  
  if(flatten){
    data = .flatten(data)
  }
  
  extnames = names(data)
  if(!is.null(extnames) & length(unique(names(data))) == length(extnames)){
    goodnames = TRUE
  }else{
    goodnames = FALSE
  }
  
  for(i in 1:data_len){
    if(is.null(list_sub) | isTRUE(names(data)[i] %in% list_sub)){ #easy way to limit outputs to named list components
      if(inherits(data[[i]], c('Rfits_image', 'Rfits_image_pointer', 'array', 'matrix', 'integer', 'numeric'))){
        if(goodnames){
          extname = extnames[i]
        }else{
          extname=paste0('data',i)
        }
        Rfits_write_image_hdf5(data=data[[i]], filename=filename, extname=extname,
                          overwrite_file=overwrite_file)
        overwrite_file = FALSE
      }else if(inherits(data[[i]], c('Rfits_table', 'data.frame', 'data.table'))){
        if(goodnames){
          extname = extnames[i]
        }else{
          extname=paste0('table',i)
        }
        Rfits_write_table_hdf5(table=data[[i]], filename=filename, extname=extname,
                          overwrite_file=overwrite_file)
        overwrite_file = FALSE
      }else{
        message('List item ',i,' (',names(data)[i],') is not recognised and will not be written to HDF5!')
      }
    }
  }
}

Rfits_write_hdf5 = Rfits_write_all_hdf5
