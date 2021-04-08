Rfits_read_table_hdf5 = function(filename='temp.h5', extname='table1', ext=NULL, data.table=TRUE, header=FALSE, remove_HIERARCH=FALSE){
  if(requireNamespace("hdf5r", quietly = TRUE)){
    
    file.h5 = hdf5r::H5File$new(filename, mode='a')
    
    output = NULL
    
    try({
      if(!is.null(ext)){
        extname = file.h5$names[ext]
      }
      
      output = file.h5[[extname]][]
      
      if(data.table){
        output = data.table::as.data.table(output)
        
      }
      
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
        
        hdr = list(header = header,
                   hdr = hdr,
                   keyvalues = keyvalues,
                   keycomments = keycomments,
                   keynames = keynames,
                   comment = comment,
                   history = history
        )
        
        attributes(output) = c(attributes(output), 
                               hdr,
                               filename = filename,
                               ext = ext,
                               extname = hdr$keyvalues$EXTNAME
        )
        
        class(output) = c('Rfits_table', class(output))
        
      }
    })
    
    #file.h5$close_all()
    
    return(output)
  }else{
    stop('The hdf5r package is needed for reading to work. Please install from CRAN.', call. = FALSE)
  }
}

Rfits_write_table_hdf5 = function(table, filename='temp.h5', extname='table1', create_ext=TRUE, overwrite_file=FALSE){
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
      if(inherits(table, what=c('Rfits_table'))){
        header = attributes(table)$header
        
        file.h5[[extname]] = table
        hdf5r::h5attr(file.h5[[extname]], 'header') = header
      }else{
        file.h5[[extname]] = table
      }
    })
    
    #file.h5$close_all()
  }else{
    stop('The hdf5r package is needed for writing to work. Please install from CRAN.', call. = FALSE)
  }
}
