Rfits_read_image_hdf5 = function(filename='temp.h5', extname='data1', ext=NULL, header=TRUE,
                                 xlo=NULL, xhi=NULL, ylo=NULL, yhi=NULL, zlo=NULL, zhi=NULL,
                                 tlo=NULL, thi=NULL, remove_HIERARCH=FALSE, force_logical=FALSE){
  if(!requireNamespace("hdf5r", quietly = TRUE)){
    stop('The hdf5r package is needed for writing to work. Please install from CRAN.', call. = FALSE)
  }
  
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
      
      Ndim = length(dim)
      
      naxis1 = naxis2 = naxis3 = naxis4 = 1
      
      if(Ndim >= 1){naxis1 = dim[1]}
      if(Ndim >= 2){naxis2 = dim[2]}
      if(Ndim >= 3){naxis3 = dim[3]}
      if(Ndim == 4){naxis4 = dim[4]}
      
      subset = FALSE
      
      if(is.null(xlo)){xlo = 1}else{subset=TRUE}
      if(is.null(xhi)){xhi = naxis1}else{subset=TRUE}
      if(is.null(ylo)){ylo = 1}else{subset=TRUE}
      if(is.null(yhi)){yhi = naxis2}else{subset=TRUE}
      if(is.null(zlo)){zlo = 1}else{subset=TRUE}
      if(is.null(zhi)){zhi = naxis3}else{subset=TRUE}
      if(is.null(tlo)){tlo = 1}else{subset=TRUE}
      if(is.null(thi)){thi = naxis4}else{subset=TRUE}
      
      if(subset){
        safex = .safedim(1,naxis1,xlo,xhi)
        safey = .safedim(1,naxis2,ylo,yhi)
        safez = .safedim(1,naxis3,zlo,zhi)
        safet = .safedim(1,naxis4,tlo,thi)
        xlo = min(safex$orig)
        xhi = max(safex$orig)
        ylo = min(safey$orig)
        yhi = max(safey$orig)
        zlo = min(safez$orig)
        zhi = max(safez$orig)
        tlo = min(safet$orig)
        thi = max(safet$orig)
        
        if(safex$safe){
          assertIntegerish(xlo, lower=1, upper=naxis1, len=1)
          assertIntegerish(xhi, lower=1, upper=naxis1, len=1)
        }
        if(safey$safe){
          assertIntegerish(ylo, lower=1, upper=naxis2, len=1)
          assertIntegerish(yhi, lower=1, upper=naxis2, len=1)
        }
        if(safez$safe){
          assertIntegerish(zlo, lower=1, upper=naxis3, len=1)
          assertIntegerish(zhi, lower=1, upper=naxis3, len=1)
        }
        if(safet$safe){
          assertIntegerish(tlo, lower=1, upper=naxis4, len=1)
          assertIntegerish(thi, lower=1, upper=naxis4, len=1)
        }
        
        if(xhi < xlo){stop('xhi must be larger than xlo')}
        if(yhi < ylo){stop('yhi must be larger than ylo')}
        if(zhi < zlo){stop('zhi must be larger than zlo')}
        if(thi < tlo){stop('thi must be larger than tlo')}
      
        if(safex$safe & safey$safe & safez$safe & safet$safe){
          if(Ndim == 1){temp_image = file.h5[[extname]][xlo:xhi]}
          if(Ndim == 2){temp_image = file.h5[[extname]][xlo:xhi,ylo:yhi]}
          if(Ndim == 3){temp_image = file.h5[[extname]][xlo:xhi,ylo:yhi,zlo:zhi]}
          if(Ndim == 4){temp_image = file.h5[[extname]][xlo:xhi,ylo:yhi,zlo:zhi,tlo:thi]}
        }
        if(Ndim==1){
          image = rep(NA, safex$len_tar)
          if(safex$safe){
            image[safex$tar] = temp_image
          }
        }
        if(Ndim==2){
          image = array(NA, c(safex$len_tar, safey$len_tar))
          if(safex$safe & safey$safe){
            image[safex$tar,safey$tar] = temp_image
          }
        }
        if(Ndim==3){
          image = array(NA, c(safex$len_tar, safey$len_tar, safez$len_tar))
          if(safex$safe & safey$safe & safez$safe){
            image[safex$tar,safey$tar,safez$tar] = temp_image
          }
        }
        if(Ndim==4){
          image = array(NA, c(safex$len_tar, safey$len_tar, safez$len_tar, safet$len_tar))
          if(safex$safe & safey$safe & safez$safe & safet$safe){
            image[safex$tar,safey$tar,safez$tar,safet$tar] = temp_image
          }
        }
        
      }else{
        if(Ndim == 1){image = file.h5[[extname]][]}
        if(Ndim == 2){image = file.h5[[extname]][,]}
        if(Ndim == 3){image = file.h5[[extname]][,,]}
        if(Ndim == 4){image = file.h5[[extname]][,,,]}
      }
      
      if(force_logical & is.integer(image)){
        image = as.logical(image)
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
        
        if(subset){
          #Dim 1
          hdr[which(hdr=='NAXIS1')+1] = safex$len_tar
          hdr[which(hdr=='CRPIX1')+1] = as.character(keyvalues$CRPIX1 - safex$lo_tar + 1)
          keyvalues$NAXIS1 = safex$len_tar
          keyvalues$CRPIX1 = keyvalues$CRPIX1 - safex$lo_tar + 1
          keycomments$NAXIS1 = paste(keycomments$NAXIS1, 'SUBMOD')
          keycomments$CRPIX1 = paste(keycomments$CRPIX1, 'SUBMOD')
          header[grep('NAXIS1', header)] = paste(formatC('NAXIS1', width=8,flag="-"),'=',formatC(keyvalues$NAXIS1, width=21),' / ',keycomments$NAXIS1,sep='')
          header[grep('CRPIX1', header)] = paste(formatC('CRPIX1', width=8,flag="-"),'=',formatC(keyvalues$CRPIX1, width=21),' / ',keycomments$CRPIX1,sep='')
          #Dim 2
          if(Ndim >= 2){
            hdr[which(hdr=='NAXIS2')+1] = safey$len_tar
            hdr[which(hdr=='CRPIX2')+1] = as.character(keyvalues$CRPIX2 - safey$lo_tar + 1)
            keyvalues$NAXIS2 = safey$len_tar
            keyvalues$CRPIX2 = keyvalues$CRPIX2 - safey$lo_tar + 1
            keycomments$NAXIS2 = paste(keycomments$NAXIS2, 'SUBMOD')
            keycomments$CRPIX2 = paste(keycomments$CRPIX2, 'SUBMOD')
            header[grep('NAXIS2', header)] = paste(formatC('NAXIS2', width=8,flag="-"),'=',formatC(keyvalues$NAXIS2, width=21),' / ',keycomments$NAXIS2,sep='')
            header[grep('CRPIX2', header)] = paste(formatC('CRPIX2', width=8,flag="-"),'=',formatC(keyvalues$CRPIX2, width=21),' / ',keycomments$CRPIX2,sep='')
          }
          #Dim 3
          if(Ndim >= 3){
            hdr[which(hdr=='NAXIS3')+1] = safez$len_tar
            hdr[which(hdr=='CRPIX3')+1] = as.character(keyvalues$CRPIX3 - safez$lo_tar + 1)
            keyvalues$NAXIS3 = safez$len_tar
            keyvalues$CRPIX3 = keyvalues$CRPIX3 - safez$lo_tar + 1
            keycomments$NAXIS3 = paste(keycomments$NAXIS3, 'SUBMOD')
            keycomments$CRPIX3 = paste(keycomments$CRPIX3, 'SUBMOD')
            header[grep('NAXIS3', header)] = paste(formatC('NAXIS3', width=8,flag="-"),'=',formatC(keyvalues$NAXIS3, width=21),' / ',keycomments$NAXIS3,sep='')
            header[grep('CRPIX3', header)] = paste(formatC('CRPIX3', width=8,flag="-"),'=',formatC(keyvalues$CRPIX3, width=21),' / ',keycomments$CRPIX3,sep='')
          }
          #Dim 4
          if(Ndim >= 4){
            hdr[which(hdr=='NAXIS4')+1] = safet$len_tar
            hdr[which(hdr=='CRPIX4')+1] = as.character(keyvalues$CRPIX4 - safet$lo_tar + 1)
            keyvalues$NAXIS4 = safet$len_tar
            keyvalues$CRPIX4 = keyvalues$CRPIX4 - safet$lo_tar + 1
            keycomments$NAXIS4 = paste(keycomments$NAXIS4, 'SUBMOD')
            keycomments$CRPIX4 = paste(keycomments$CRPIX4, 'SUBMOD')
            header[grep('NAXIS4', header)] = paste(formatC('NAXIS4', width=8,flag="-"),'=',formatC(keyvalues$NAXIS4, width=21),' / ',keycomments$NAXIS4,sep='')
            header[grep('CRPIX4', header)] = paste(formatC('CRPIX4', width=8,flag="-"),'=',formatC(keyvalues$CRPIX4, width=21),' / ',keycomments$CRPIX4,sep='')
          }
        }
        
        output = list(imDat = image,
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
        
        if(Ndim == 1){class(output) = c('Rfits_vector', class(output))}
        if(Ndim == 2){class(output) = c('Rfits_image', class(output))}
        if(Ndim == 3){class(output) = c('Rfits_cube', class(output))}
        if(Ndim == 4){class(output) = c('Rfits_array', class(output))}
        
      }else{
        output = image
      }
    })
    
    #file.h5$close_all()
    
    if(!is.null(output)){
      return(invisible(output))
    }
}

Rfits_read_vector_hdf5 = Rfits_read_image_hdf5
Rfits_read_cube_hdf5 = Rfits_read_image_hdf5
Rfits_read_array_hdf5 = Rfits_read_image_hdf5

Rfits_write_image_hdf5 = function(data, filename='temp.h5', extname='data1', create_ext=TRUE, overwrite_file=FALSE){
  if(!requireNamespace("hdf5r", quietly = TRUE)){
    stop('The hdf5r package is needed for writing to work. Please install from CRAN.', call. = FALSE)
  }
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
      if(inherits(data, what=c('Rfits_vector', 'Rfits_image', 'Rfits_cube', 'Rfits_array'))){
        header = data$header
        data = data$imDat
        
        file.h5[[extname]] = data
        hdf5r::h5attr(file.h5[[extname]], 'header') = header
      }else{
        file.h5[[extname]] = data
      }
    })
    
    #file.h5$close_all()
}

Rfits_write_vector_hdf5 = Rfits_write_image_hdf5
Rfits_write_cube_hdf5 = Rfits_write_image_hdf5
Rfits_write_array_hdf5 = Rfits_write_image_hdf5

Rfits_point_hdf5 = function(filename='temp.h5', extname='data1', ext=NULL, header=TRUE){
  output = list(filename=filename, extname=extname, ext=ext, header=header)
  class(output) = 'Rfits_pointer_hdf5'
  return(invisible(output))
}

`[.Rfits_pointer_hdf5` = function(x, i=NULL, j=NULL, k=NULL, m=NULL, header=x$header){
  
  if(!missing(i)){
    if(is.vector(i)){
      xlo = ceiling(min(i))
      xhi = ceiling(max(i))
    }
  }else{
    xlo = NULL
    xhi = NULL
  }
  if(!missing(j)){
    ylo = ceiling(min(j))
    yhi = ceiling(max(j))
  }else{
    ylo = NULL
    yhi = NULL
  }
  if(!missing(k)){
    zlo = ceiling(min(k))
    zhi = ceiling(max(k))
  }else{
    zlo = NULL
    zhi = NULL
  }
  if(!missing(m)){
    tlo = ceiling(min(m))
    thi = ceiling(max(m))
  }else{
    tlo = NULL
    thi = NULL
  }
  
  data = Rfits_read_image_hdf5(x$filename, extname=x$extname, ext=x$ext,
                               xlo=xlo, xhi=xhi, ylo=ylo, yhi=yhi, zlo=zlo, zhi=zhi,
                               tlo=tlo, thi=thi, header=header)
  return(data)
}

dim.Rfits_pointer_hdf5 = function(x){
  if(!requireNamespace("hdf5r", quietly = TRUE)){
    stop('The hdf5r package is needed for writing to work. Please install from CRAN.', call. = FALSE)
  }
  
  data = hdf5r::H5File$new(x, mode="r")
  return(data[['image']]$dims)
}
