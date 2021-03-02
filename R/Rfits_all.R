Rfits_read_all=function(filename='temp.fits', pointer=FALSE, header=TRUE, data.table=TRUE, anycompress=TRUE){
  assertCharacter(filename, max.len=1)
  filename=path.expand(filename)
  assertFlag(pointer)
  
  info = Rfits_info(filename)
  
  data = vector(mode='list', length=length(info$summary))
  
  #images
  
  if(!is.null(info$headers[[1]]$keyvalues$NAXIS1)){
    if(pointer){
      data[[1]] = Rfits_point(filename, ext=1, header=header)
    }else{
      data[[1]] = Rfits_read_image(filename, ext=1, header=header)
    }
  }
  
  sel_images = which(grepl('IMAGE',info$summary) | grepl('HDRLET',info$summary))
  sel_images = sel_images[sel_images>1]
  if(length(sel_images)>0){
    for(i in sel_images){
      if(pointer){
        data[[i]] = Rfits_point(filename, ext=i, header=header)
      }else{
        data[[i]] = Rfits_read_image(filename, ext=i, header=header)
      }
    }
  }
  
  #tables
  
  sel_tables = grep('TABLE',info$summary)
  if(length(sel_tables)>0){
    for(i in sel_tables){
      if(anycompress==FALSE | is.null(info$headers[[i]]$keyvalues$ZIMAGE) | isFALSE(info$headers[[i]]$keyvalues$ZIMAGE)){ #this is to catch for standard compressed images (stored as tables)
        data[[i]] = Rfits_read_table(filename, ext=i, header=header, data.table=data.table)
      }else{
        if(pointer){
          data[[i]] = Rfits_point(filename, ext=i, header=header)
        }else{
          data[[i]] = Rfits_read_image(filename, ext=i, header=header)
        }
      }
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
    if(!is.null(info$headers[[i]]$keyvalues$EXTNAME)){
      names(data)[i] = info$headers[[i]]$keyvalues$EXTNAME
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

Rfits_write_all=function(data, filename='temp.fits', flatten=FALSE, overwrite_Main=TRUE){
  assertList(data)
  assertCharacter(filename, max.len=1)
  
  create_file = TRUE
  overwrite_file = TRUE
  
  if(flatten){
    data = .flatten(data)
  }
  
  # EXTNAMES = NULL
  # EXTCOMMENTS = NULL
  # ignoreEXT = NULL
  
  for(i in 1:length(data)){
    # if(is.list(data[[i]])){
    #   if(is.null(data[[i]]$keyvalues$EXTNAME)){
    #     if(is.null(attributes(data[[i]])$keycomments$EXTNAME)){
    #       EXTNAMES = c(EXTNAMES, names(data)[i])
    #       EXTCOMMENTS = c(EXTCOMMENTS, '')
    #     }else{
    #       EXTNAMES = c(EXTNAMES, attributes(data[[i]])$keyvalues$EXTNAME)
    #       EXTCOMMENTS = c(EXTCOMMENTS, attributes(data[[i]])$keycomments$EXTNAME)
    #     }
    #   }else{
    #     EXTNAMES = c(EXTNAMES, data[[i]]$keyvalues$EXTNAME)
    #     EXTCOMMENTS = c(EXTCOMMENTS, data[[i]]$keycomments$EXTNAME)
    #   }
    # }else{
    #   if(is.null(names(data)[i])){
    #     #EXTNAMES = c(EXTNAMES, paste0('EXT',i))
    #     EXTNAMES = c(EXTNAMES, NA)
    #     EXTCOMMENTS = c(EXTCOMMENTS, '')
    #   }else{
    #     EXTNAMES = c(EXTNAMES, names(data)[i])
    #     EXTCOMMENTS = c(EXTCOMMENTS, '')
    #   }
    # }
    
    if(i > 1){
      ext = Rfits_nhdu(filename)
    }else{
      ext = 1
    }
    
    if(inherits(data[[i]], c('Rfits_image', 'Rfits_image_pointer', 'array', 'matrix', 'integer', 'numeric'))){
      Rfits_write_image(data=data[[i]], filename=filename, ext=ext,
                        create_file=create_file, overwrite_file=overwrite_file)
      create_file = FALSE
      overwrite_file = FALSE
    }else if(inherits(data[[i]], c('Rfits_table', 'data.frame', 'data.table'))){
      Rfits_write_table(table=data[[i]], filename=filename, ext=ext,
                        create_file=create_file, overwrite_file=overwrite_file)
      create_file = FALSE
      overwrite_file = FALSE
    }else if(inherits(data[[i]], 'Rfits_header')){
      #ignoreEXT = c(ignoreEXT,i)
      Rfits_write_header(filename=filename, keyvalues=data[[i]]$keyvalues, keycomments=data[[i]]$keycomments,
                         comment=data[[i]]$comments, history=data[[i]]$history, create_ext=TRUE,
                         create_file=create_file, overwrite_file=overwrite_file)
      create_file = FALSE
      overwrite_file = FALSE
    }else{
      #ignoreEXT = c(ignoreEXT,i)
      message('List item ',i,' is not recognised and will not be written to FITS!')
    }
    
    if(!is.null(names(data)[i])){
      if(!is.na(names(data)[i])){
        if(!names(data)[i]==''){
          ext = Rfits_nhdu(filename) # in case first object was a table
          check_head = Rfits_read_header(filename=filename, ext=ext)
          if(is.null(check_head$keyvalues$EXTNAME)){
              Rfits_write_key(filename=filename, keyname='EXTNAME', keyvalue=names(data)[i], keycomment='', ext=ext)
          }else{
            if(is.na(check_head$keyvalues$EXTNAME)){
              Rfits_write_key(filename=filename, keyname='EXTNAME', keyvalue=names(data)[i], keycomment='', ext=ext)
            }else if(check_head$keyvalues$EXTNAME=='Main' & overwrite_Main){
              Rfits_write_key(filename=filename, keyname='EXTNAME', keyvalue=names(data)[i], keycomment='', ext=ext)
            }
          }
        }
      }
    }
  }
  
  # if(length(ignoreEXT) > 0){
  #   EXTNAMES = EXTNAMES[-ignoreEXT]
  #   EXTCOMMENTS = EXTCOMMENTS[-ignoreEXT]
  # }
  # 
  # if(length(EXTNAMES) > 0){
  #   for(i in 1:length(EXTNAMES)){
  #     if(! is.na(EXTNAMES[i])){
  #       Rfits_write_key(filename=filename, keyname='EXTNAME', keyvalue=EXTNAMES[i], keycomment=EXTCOMMENTS[i], ext=ext)
  #     }
  #   }
  # }
}

Rfits_write = Rfits_write_all
