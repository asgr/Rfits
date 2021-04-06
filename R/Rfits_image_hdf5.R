Rfits_write_image_hdf5 = function(data, filename='temp.h5', extname='data1', create_ext=TRUE, overwrite_file=FALSE){
  if(requireNamespace("hdf5r", quietly = TRUE)){
    assertCharacter(filename, max.len=1)
    assertCharacter(extname, max.len=1)
    filename = path.expand(filename)
    assertFlag(overwrite_file)
    if(overwrite_file & create_ext==FALSE){
      assertFileExists(filename)
      assertAccess(filename, access='w')
      file.remove(filename)
    }else{
      assertPathForOutput(filename, overwrite=TRUE)
    }
    
    file.h5 = hdf5r::H5File$new(filename, mode='a')
    
    try({
      file.h5[[extname]] = data$imDat
      hdf5r::h5attr(file.h5[[extname]], 'header') = data$header
    })
    
    file.h5$close_all()
  }else{
    stop('The hdf5r package is needed for smoothing to work. Please install from CRAN.', call. = FALSE)
  }
}

Rfits_read_image_hdf5 = function(filename='temp.h5', extname='data1', ext=NULL, header=TRUE,
                                 xlo=NULL, xhi=NULL, ylo=NULL, yhi=NULL, remove_HIERARCH=FALSE){
  if(requireNamespace("hdf5r", quietly = TRUE)){
    assertCharacter(filename, max.len=1)
    filename = path.expand(filename)
    assertAccess(filename, access='r')
    assertCharacter(extname, max.len=1)
    assertFlag(header)
    assertIntegerish(xlo, null.ok=TRUE)
    assertIntegerish(xhi, null.ok=TRUE)
    assertIntegerish(ylo, null.ok=TRUE)
    assertIntegerish(yhi, null.ok=TRUE)
    assertFlag(remove_HIERARCH)
    
    file.h5 = hdf5r::H5File$new(filename, mode='r')
    
    output = NULL
    
    try({
      if(!is.null(ext)){
        extname = file.h5$names[ext]
      }
      
      dim = file.h5[[extname]]$dims
      naxis1 = dim[1]
      naxis2 = dim[2]
      
      if(is.null(xlo)){xlo = 1}
      if(is.null(xhi)){xhi = naxis1}
      if(is.null(ylo)){ylo = 1}
      if(is.null(yhi)){yhi = naxis2}
      
      assertIntegerish(xlo, lower=1, upper=naxis1, len=1)
      assertIntegerish(xhi, lower=1, upper=naxis1, len=1)
      assertIntegerish(ylo, lower=1, upper=naxis2, len=1)
      assertIntegerish(yhi, lower=1, upper=naxis2, len=1)
      
      if(xhi<xlo){stop('xhi must be larger than xlo')}
      if(yhi<ylo){stop('yhi must be larger than ylo')}
      
      if(header){
        #raw header
        header = hdf5r::h5attr(file.h5[[extname]], 'header')
        
        #remove comments for parsing
        loc_comment = grep('COMMENT', header)
        loc_history = grep('HISTORY', header)
        
        if(length(loc_comment)>0){
          comment = gsub('COMMENT ', '', header[loc_comment])
          #comment = gsub('  ','',comment) #not sure this works generically, probably better to keep it raw-er.
        }else{
          comment = NULL
        }
        
        if(length(loc_history)>0){
          history = gsub('HISTORY ', '', header[loc_history])
          #history = gsub('  ','',history) #not sure this works generically, probably better to keep it raw-er.
        }else{
          history = NULL
        }
        
        if(length(loc_comment)>0 | length(loc_history)>0){
          headertemp = header[-c(loc_comment, loc_history)]
        }else{
          headertemp = header
        }
        
        #hdr vector
        hdr = Rfits_header_to_hdr(headertemp, remove_HIERARCH=remove_HIERARCH)
        
        #keyword list
        keyvalues = Rfits_hdr_to_keyvalues(hdr)
        keynames = names(keyvalues)
        
        loc_HIERARCH = grep('HIERARCH', keynames)
        if(length(loc_HIERARCH)>0){
          keynames_goodhead = keynames[-loc_HIERARCH] 
          pattern_goodhead = paste(c(paste0(format(keynames_goodhead, width=8), '='), 'HIERARCH'), collapse = '|')
        }else{
          pattern_goodhead = paste(paste0(format(keynames, width=8), '='), collapse = '|')
        }
        loc_goodhead = grep(pattern_goodhead, headertemp)
        
        headertemp = headertemp[loc_goodhead]
        #comments list
        keycomments = lapply(strsplit(headertemp,'/ '),function(x) x[2])
        names(keycomments) = keynames
        
        ext = which(file.h5$names == extname)
        
        output = list(imDat = file.h5[[extname]][xlo:xhi,ylo:yhi],
                      header = header,
                      hdr = hdr,
                      keyvalues = keyvalues,
                      keycomments = keycomments,
                      keynames = keynames,
                      comment = comment,
                      history = history,
                      filename = filename,
                      ext = ext,
                      extname = extname
                      )
        
        class(output) = c('Rfits_image', class(output))
      }else{
        output = file.h5[[extname]][xlo:xhi,ylo:yhi]
      }
    })
    
    file.h5$close_all()
    
    if(!is.null(output)){
      return(output)  
    }
  }else{
    stop('The hdf5r package is needed for smoothing to work. Please install from CRAN.', call. = FALSE)
  }
}
