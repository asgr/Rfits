Rfits_read_all=function(filename='temp.fits', pointer='auto', header=TRUE, data.table=TRUE, anycompress=TRUE, bad=NULL, zap=NULL){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  filename = Rfits_gunzip(filename)
  if(is.character(pointer)){
    if(pointer=='auto'){
      size = file.size(filename)/2^20 # to get to MB
      if(size > 100){ #If total file size if more than 100 MB then use pointers to access by default
        pointer = TRUE
      }else{
        pointer = FALSE
      }
    }
  }
  assertFlag(pointer)
  assertLogical(header)
  assertFlag(data.table)
  assertFlag(anycompress)
  assertNumeric(bad, null.ok=TRUE)
  assertCharacter(zap, null.ok=TRUE)
  
  info = Rfits_info(filename)
  
  data = vector(mode='list', length=length(info$summary))
  
  if(length(header) == 1){
    header = rep(header, length(data))
  }
  
  if(length(bad) == 1){
    bad = rep(bad, length(data))
  }
  
  #images
  
  if(!is.null(info$headers[[1]]$keyvalues$NAXIS)){
    if(info$headers[[1]]$keyvalues$NAXIS > 0){
      if(pointer){
        data[[1]] = Rfits_point(filename, ext=1, header=header[1], zap=zap)
      }else{
        data[[1]] = Rfits_read_image(filename, ext=1, header=header[1], bad=bad[1], zap=zap)
      }
    }
  }
  
  sel_images = which(grepl('IMAGE',info$summary) | grepl('HDRLET',info$summary))
  sel_images = sel_images[sel_images>1]
  if(length(sel_images)>0){
    for(i in sel_images){
      if(pointer){
        data[[i]] = Rfits_point(filename, ext=i, header=header[i], zap=zap)
      }else{
        data[[i]] = Rfits_read_image(filename, ext=i, header=header[i], bad=bad[i], zap=zap)
      }
    }
  }
  
  #tables
  
  sel_tables = grep('TABLE',info$summary)
  if(length(sel_tables)>0){
    for(i in sel_tables){
      if(anycompress==FALSE | is.null(info$headers[[i]]$keyvalues$ZIMAGE) | isFALSE(info$headers[[i]]$keyvalues$ZIMAGE)){ #this is to catch for standard compressed images (stored as tables)
        data[[i]] = Rfits_read_table(filename, ext=i, header=header[i], data.table=data.table, zap=zap)
      }else{
        if(pointer){
          #This is where we read compressed images
          data[[i]] = Rfits_point(filename, ext=i, header=header[i], zap=zap)
        }else{
          data[[i]] = Rfits_read_image(filename, ext=i, header=header[i], bad=bad[i], zap=zap)
        }
      }
    }
  }
  
  #NULL
  
  if(length(data) == 0){
    data = list(Rfits_read_header(filename, ext=1, zap=zap))
  }else{
    for(i in 1:length(data)){
      if(is.null(data[[i]])){
        data[[i]] = Rfits_read_header(filename, ext=i, zap=zap)
      }
    }
  }
  
  #names
  
  names(data) = rep('', length(data))
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

Rfits_write_all=function(data, filename='temp.fits', flatten=FALSE, overwrite_Main=TRUE,
                         compress=FALSE, bad_compress=0, list_sub=NULL){
  if(is.list(data)){
    data_len = length(data)
  }else{
    data_len = 1
  }
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  assertFlag(flatten)
  assertFlag(overwrite_Main)

  create_file = TRUE
  overwrite_file = TRUE
  
  if(flatten){
    data = .flatten(data)
  }
  
  if(!is.logical(compress)){
    compress_vec = {}
    for(i in 1:data_len){
      if(inherits(data[[i]], 'Rfits_header')){
        compress_vec = c(compress_vec, FALSE)
      }else{
        compress_vec = c(compress_vec, length(data[[i]]) > compress)
      }
    }
    compress = compress_vec
  }
  
  if(length(compress) == 1){
    compress = rep(compress, data_len)
  }
  
  if(length(bad_compress) == 1){
    bad_compress = rep(bad_compress, data_len)
  }
  
  for(i in 1:data_len){
    if(is.null(list_sub) | isTRUE(names(data)[i] %in% list_sub)){ #easy way to limit outputs to named list components
      
      if(i > 1){
        ext = Rfits_nhdu(filename)
      }else{
        ext = 1
      }
      
      if(inherits(data[[i]], c('Rfits_vector','Rfits_image','Rfits_cube','Rfits_array', 'array', 'matrix', 'integer', 'numeric'))){
        Rfits_write_image(data=data[[i]], filename=filename, ext=ext,
                          create_file=create_file, overwrite_file=overwrite_file, compress=compress[i], bad_compress=bad_compress[i])
        create_file = FALSE
        overwrite_file = FALSE
      }else if(inherits(data[[i]], c('Rfits_table', 'data.frame', 'data.table'))){
        Rfits_write_table(table=data[[i]], filename=filename, ext=ext,
                          create_file=create_file, overwrite_file=overwrite_file)
        create_file = FALSE
        overwrite_file = FALSE
      }else if(inherits(data[[i]], 'Rfits_header')){
        Rfits_write_header(filename=filename, keyvalues=data[[i]]$keyvalues, keycomments=data[[i]]$keycomments,
                           comment=data[[i]]$comments, history=data[[i]]$history, create_ext=TRUE,
                           create_file=create_file, overwrite_file=overwrite_file)
        create_file = FALSE
        overwrite_file = FALSE
      }else{
        message('List item ',i,' (',names(data)[i],') is not recognised and will not be written to FITS!')
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
  }
}

Rfits_write = Rfits_write_all

Rfits_make_list = function(filelist=NULL, dirlist=NULL, extlist=1, pattern='.fits$',
                           recursive=TRUE, header=TRUE, pointer=TRUE, cores=4, ...){
  if(is.null(filelist)){
    if(is.null(dirlist)){
      stop('Missing filelist and dirlist')
    }
    filelist = {}
    for(i in 1:length(dirlist)){
      filelist = c(filelist,
                   list.files(dirlist[i], pattern=pattern, full.names=TRUE, recursive=recursive))
    }
  }
  
  registerDoParallel(cores=cores)
  
  filelist = grep(pattern=pattern, filelist, value=TRUE)
  filelist = normalizePath(filelist)
  
  if(length(extlist) == 1){
    extlist = rep(extlist, length(filelist))
  }
  
  data = foreach(i=1:length(filelist))%dopar%{
    if(pointer){
      return(Rfits_point(filelist[i], ext=extlist[i], header=TRUE, ...))
    }else{
      return(Rfits_read_image(filelist[i], ext=extlist[i], header=TRUE, ...))
    }
  }
  
  class(data) = 'Rfits_list'
  attributes(data)$filename = filelist
  return(data)
}