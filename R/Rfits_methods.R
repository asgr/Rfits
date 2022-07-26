.safedim = function(lo_orig=1L, hi_orig=1L, lo_tar=1L, hi_tar=1L){
  len_orig = hi_orig - lo_orig + 1L
  len_tar = hi_tar - lo_tar + 1L
  
  out_lo_orig = max(lo_tar, 1L)
  out_hi_orig = min(hi_tar, len_orig)
  diff = (1L - lo_tar)
  out_lo_tar = out_lo_orig + diff
  out_hi_tar = out_hi_orig + diff
  safe = (out_hi_tar >= out_lo_tar) & (out_hi_orig & out_lo_orig)
  return(list(orig = out_lo_orig:out_hi_orig, tar = out_lo_tar:out_hi_tar, len_orig=len_orig,
              len_tar=len_tar, safe=safe, lo_orig=lo_orig, hi_orig=hi_orig, lo_tar=lo_tar,
              hi_tar=hi_tar, diff=diff))
}

Rfits_point=function(filename='temp.fits', ext=1, header=FALSE, zap=NULL){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  assertAccess(filename, access='r')
  filename = Rfits_gunzip(filename)
  assertIntegerish(ext, len=1)
  assertFlag(header)
  
  temp = Rfits_read_header(filename=filename, ext=ext, zap=zap)
  keyvalues = temp$keyvalues
  raw = temp$raw
  
  if(isTRUE(keyvalues$ZIMAGE)){
    naxis1 = keyvalues$ZNAXIS1
    naxis2 = keyvalues$ZNAXIS2
    naxis3 = keyvalues$ZNAXIS3
    naxis4 = keyvalues$ZNAXIS4
    datatype = keyvalues$ZBITPIX
  }else{
    naxis1 = keyvalues$NAXIS1
    naxis2 = keyvalues$NAXIS2
    naxis3 = keyvalues$NAXIS3
    naxis4 = keyvalues$NAXIS4
    datatype = keyvalues$BITPIX
  }
  
  dim = c(naxis1); type='vector'
  if(!is.null(naxis2)){dim = c(dim, naxis2); type='image'}
  if(!is.null(naxis3)){dim = c(dim, naxis3); type='cube'}
  if(!is.null(naxis4)){dim = c(dim, naxis4); type='array'}
  
  output = list(filename=filename, ext=ext, keyvalues=keyvalues, raw=raw, header=header, dim=dim, type=type)
  class(output) = 'Rfits_pointer'
  return(invisible(output))
}

print.Rfits_vector=function(x , ...){
  
  if(isTRUE(x$keyvalues$ZIMAGE)){
    naxis1 = x$keyvalues$ZNAXIS1
    datatype = x$keyvalues$ZBITPIX
  }else{
    naxis1 = x$keyvalues$NAXIS1
    datatype = x$keyvalues$BITPIX
  }
  
  cat('Class: Rfits_vector\n')
  cat('File path:',x$filename,'\n')
  cat('Ext num:',x$ext,'\n')
  cat('Ext name:',x$keyvalues[['EXTNAME']],'\n')
  cat('RAM size:',round(object.size(x)/(2^20),4),'MB\n')
  cat('BITPIX:',datatype,'\n')
  cat('NAXIS1:',naxis1,'\n')
  cat('Key N:',length(x$keyvalues),'\n')
}

print.Rfits_image=function(x , ...){
  
  if(isTRUE(x$keyvalues$ZIMAGE)){
    naxis1 = x$keyvalues$ZNAXIS1
    naxis2 = x$keyvalues$ZNAXIS2
    datatype = x$keyvalues$ZBITPIX
  }else{
    naxis1 = x$keyvalues$NAXIS1
    naxis2 = x$keyvalues$NAXIS2
    datatype = x$keyvalues$BITPIX
  }
  
  cat('Class: Rfits_image\n')
  cat('File path:',x$filename,'\n')
  cat('Ext num:',x$ext,'\n')
  cat('Ext name:',x$keyvalues[['EXTNAME']],'\n')
  cat('RAM size:',round(object.size(x)/(2^20),4),'MB\n')
  cat('BITPIX:',datatype,'\n')
  cat('NAXIS1:',naxis1,'\n')
  cat('NAXIS2:',naxis2,'\n')
  cat('Key N:',length(x$keyvalues),'\n')
}

print.Rfits_cube=function(x , ...){
  
  if(isTRUE(x$keyvalues$ZIMAGE)){
    naxis1 = x$keyvalues$ZNAXIS1
    naxis2 = x$keyvalues$ZNAXIS2
    naxis3 = x$keyvalues$ZNAXIS3
    datatype = x$keyvalues$ZBITPIX
  }else{
    naxis1 = x$keyvalues$NAXIS1
    naxis2 = x$keyvalues$NAXIS2
    naxis3 = x$keyvalues$NAXIS3
    datatype = x$keyvalues$BITPIX
  }
  
  cat('Class: Rfits_cube\n')
  cat('File path:',x$filename,'\n')
  cat('Ext num:',x$ext,'\n')
  cat('Ext name:',x$keyvalues[['EXTNAME']],'\n')
  cat('RAM size:',round(object.size(x)/(2^20),4),'MB\n')
  cat('BITPIX:',datatype,'\n')
  cat('NAXIS1:',naxis1,'\n')
  cat('NAXIS2:',naxis2,'\n')
  cat('NAXIS3:',naxis3,'\n')
  cat('Key N:',length(x$keyvalues),'\n')
}

print.Rfits_array=function(x , ...){
  
  if(isTRUE(x$keyvalues$ZIMAGE)){
    naxis1 = x$keyvalues$ZNAXIS1
    naxis2 = x$keyvalues$ZNAXIS2
    naxis3 = x$keyvalues$ZNAXIS3
    naxis4 = x$keyvalues$ZNAXIS4
    datatype = x$keyvalues$ZBITPIX
  }else{
    naxis1 = x$keyvalues$NAXIS1
    naxis2 = x$keyvalues$NAXIS2
    naxis3 = x$keyvalues$NAXIS3
    naxis4 = x$keyvalues$NAXIS4
    datatype = x$keyvalues$BITPIX
  }
  
  cat('Class: Rfits_array\n')
  cat('File path:',x$filename,'\n')
  cat('Ext num:',x$ext,'\n')
  cat('Ext name:',x$keyvalues[['EXTNAME']],'\n')
  cat('RAM size:',round(object.size(x)/(2^20),4),'MB\n')
  cat('BITPIX:',datatype,'\n')
  cat('NAXIS1:',naxis1,'\n')
  cat('NAXIS2:',naxis2,'\n')
  cat('NAXIS3:',naxis3,'\n')
  cat('NAXIS4:',naxis4,'\n')
  cat('Key N:',length(x$keyvalues),'\n')
}

print.Rfits_pointer=function(x , ...){
  cat('File path:',x$filename,'\n')
  cat('Ext num:',x$ext,'\n')
  cat('Ext name:',x$keyvalues[['EXTNAME']],'\n')
  cat('Class: Rfits_pointer\n')
  cat('Type:',x$type,'\n')
  cat('Dim:',x$dim,'\n')
  cat('Disk size:',round(file.size(x$filename)/(2^20),4),'MB\n')
  cat('BITPIX:',x$keyvalues[['BITPIX']],'\n')
  cat('Key N:',length(x$keyvalues),'\n')
}

print.Rfits_header=function(x, ...){
  cat(x$header[1:8], sep='\n')
  cat("...", sep='\n')
  cat('Key N:',length(x$keyvalues),'\n')
}

print.Rfits_list=function(x , ...){
  
  ext_name = names(x)
  ext_dim = {}
  ext_class = {}
  ext_mode = {}
  ext_type = {}
  ext_size = {}
  ext_keyN = {}
  
  for(i in 1:length(x)){
    if(inherits(x[[i]], 'Rfits_vector')){
      ext_dim = c(ext_dim, length(x[[i]]))
    }else{
      if(is.null(dim(x[[i]]))){
        if(is.null(length(x[[i]]))){
          ext_dim = c(ext_dim, 'NA')
        }else{
          ext_dim = c(ext_dim, length(x[[i]]))
        }
      }else{
        ext_dim = c(ext_dim, paste(dim(x[[i]]), collapse=' x '))
      }
    }
    
    ext_class = c(ext_class, class(x[[i]])[1])
    ext_mode = c(ext_mode, mode(x[[i]]))
    ext_type = c(ext_type, typeof(x[[i]]))
    ext_size = c(ext_size, round(object.size(x[[i]])/(2^20),4))
    if(inherits(x[[i]], what=c('Rfits_image', 'Rfits_cube', 'Rfits_array', 'Rfits_vector', 'Rfits_pointer', 'Rfits_header'))){
      ext_keyN = c(ext_keyN, length(x[[i]]$keyvalues))
    }else if(inherits(x[[i]], what='Rfits_table')){
      ext_keyN = c(ext_keyN, length(attributes(x[[i]])$keyvalues))
    }else{
      ext_keyN = c(ext_keyN, NA)
    }
  }
  
  summarytable = data.frame(
    'Ext'= 1:length(x),
    'Name' = ext_name,
    'Class' = ext_class,
    'Mode' = ext_mode,
    'Type' = ext_type,
    'Dim' = ext_dim,
    'Size/MB' = ext_size,
    'Key.N' = ext_keyN
  )
  
  cat('Multi-extension FITS loaded with Rfits_read of class Rfits_list\n\n')
  cat('File location:',attributes(x)$filename,'\n\n')
  cat('Summary of extension contents:\n\n')
  print(summarytable)
}

print.Rfits_header=function(x, ...){
  cat(x$header[1:min(8,length(x$header))], sep='\n')
}

length.Rfits_vector=function(x){
  return(length(x$imDat))
}

length.Rfits_image=function(x){
  return(length(x$imDat))
}

length.Rfits_cube=function(x){
  return(length(x$imDat))
}

length.Rfits_array=function(x){
  return(length(x$imDat))
}

length.Rfits_pointer=function(x){
  return(prod(x$dim))
}

dim.Rfits_vector=function(x){
  return(length(x$imDat))
}

dim.Rfits_image=function(x){
  return(dim(x$imDat))
}

dim.Rfits_cube=function(x){
  return(dim(x$imDat))
}

dim.Rfits_array=function(x){
  return(dim(x$imDat))
}

dim.Rfits_pointer=function(x){
  return(x$dim)
}

dim.Rfits_header=function(x){
  if(!is.null(x$keyvalues$NAXIS1)){
    NAXIS1 = x$keyvalues$NAXIS1
  }else{
    NAXIS1 = NULL
  }
  if(!is.null(x$keyvalues$NAXIS2)){
    NAXIS2 = x$keyvalues$NAXIS2
  }else{
    NAXIS2 = NULL
  }
  if(!is.null(x$keyvalues$NAXIS3)){
    NAXIS3 = x$keyvalues$NAXIS3
  }else{
    NAXIS3 = NULL
  }
  if(!is.null(x$keyvalues$NAXIS4)){
    NAXIS4 = x$keyvalues$NAXIS4
  }else{
    NAXIS4 = NULL
  }
  return(c(NAXIS1, NAXIS2, NAXIS3, NAXIS4))
}

`[.Rfits_vector` = function(x, i, keepWCS=TRUE){
  
  if(missing(i)){i = c(1,length(x$imDat))}
  
  safedim_i = .safedim(1, length(x$imDat), min(i), max(i))
  
  tar = array(NA, dim=safedim_i$len_tar)
  if(safedim_i$safe){
    tar[safedim_i$tar] = x$imDat[safedim_i$orig]
  }
    
  if(keepWCS){
    if(!isTRUE(x$keyvalues$ZIMAGE)){
      x$keyvalues$NAXIS1 = safedim_i$len_tar
    }else{
      x$keyvalues$ZNAXIS1 = safedim_i$len_tar
    }
    if(!is.null(x$keyvalues$CRPIX1)){
      x$keyvalues$CRPIX1 = x$keyvalues$CRPIX1 - safedim_i$lo_tar + 1L
    }
    
    #New keyvalues being added
    x$keyvalues$XCUTLO = safedim_i$lo_tar
    x$keyvalues$XCUTHI = safedim_i$hi_tar
    
    #New keycomments being added
    x$keycomments$XCUTLO = 'Low image x range'
    x$keycomments$XCUTHI = 'High image x range'
    
    #New keynames being added
    x$keynames = c(x$keynames, 'XCUTLO', 'XCUTHI') 
    
    #New history being added
    x$history = c(x$history, paste0('Subset of original image: x= ',safedim_i$lo_tar,':',safedim_i$hi_tar))
    
    x$header = Rfits_keyvalues_to_header(x$keyvalues, x$keycomments, x$comment, x$history)
    x$raw = Rfits_header_to_raw(x$header)
    x$hdr = Rfits_keyvalues_to_hdr(x$keyvalues)
    
    output = list(
      imDat = tar,
      keyvalues = x$keyvalues,
      keycomments = x$keycomments,
      keynames = x$keynames,
      header = x$header,
      hdr = x$hdr,
      raw = x$raw,
      comment = x$comment,
      history = x$history,
      filename = x$filename,
      ext = x$ext
    )
    class(output) = "Rfits_image"
    return(output)
  }else{
    return(tar)
  }
}

`[.Rfits_image` = function(x, i, j, box=201, type='pix', keepWCS=TRUE){
  
  if(!missing(i)){
    if(length(i)==2 & missing(j)){
      if(i[2]-i[1] !=1){
        j = as.numeric(i[2])
        i = as.numeric(i[1])
      }
    }
  }
  
  if(missing(i)){i = c(1,dim(x$imDat)[1])}
  if(missing(j)){j = c(1,dim(x$imDat)[2])}
  
  if(type=='coord'){
    if(requireNamespace("Rwcs", quietly=TRUE)){
      assertNumeric(i,len=1)
      assertNumeric(j,len=1)
      ij = Rwcs::Rwcs_s2p(i,j,keyvalues=x$keyvalues,pixcen='R',header=x$raw)[1,]
      i = ceiling(ij[1])
      j = ceiling(ij[2])
    }else{
      message('The Rwcs package is needed to use type=coord.')
    }
  }
  
  if(length(box) == 1){box = c(box,box)}
  if(length(i) == 1){i = ceiling(i + c(-(box[1]-1L)/2, (box[1]-1L)/2))}
  if(length(j) == 1){j = ceiling(j + c(-(box[2]-1L)/2, (box[2]-1L)/2))}
  
  safedim_i = .safedim(1L, dim(x$imDat)[1], min(i), max(i))
  safedim_j = .safedim(1L, dim(x$imDat)[2], min(j), max(j))
  
  tar = array(NA, dim=c(safedim_i$len_tar, safedim_j$len_tar))
  if(safedim_i$safe & safedim_j$safe){
    tar[safedim_i$tar,safedim_j$tar] = x$imDat[safedim_i$orig,safedim_j$orig]
  }

  if(keepWCS){
    if(!isTRUE(x$keyvalues$ZIMAGE)){
      x$keyvalues$NAXIS1 = safedim_i$len_tar
      x$keyvalues$NAXIS2 = safedim_j$len_tar
    }else{
      x$keyvalues$ZNAXIS1 = safedim_i$len_tar
      x$keyvalues$ZNAXIS2 = safedim_j$len_tar
    }
    if(!is.null(x$keyvalues$CRPIX1)){
      x$keyvalues$CRPIX1 = x$keyvalues$CRPIX1 - safedim_i$lo_tar + 1L
    }
    if(!is.null(x$keyvalues$CRPIX2)){
      x$keyvalues$CRPIX2 = x$keyvalues$CRPIX2 - safedim_j$lo_tar + 1L
    }
    
    #New keyvalues being added
    x$keyvalues$XCUTLO = safedim_i$lo_tar
    x$keyvalues$XCUTHI = safedim_i$hi_tar
    x$keyvalues$YCUTLO = safedim_j$lo_tar
    x$keyvalues$YCUTHI = safedim_j$hi_tar
    
    #New keycomments being added
    x$keycomments$XCUTLO = 'Low image x range'
    x$keycomments$XCUTHI = 'High image x range'
    x$keycomments$YCUTLO = 'Low image y range'
    x$keycomments$YCUTHI = 'High image y range'
    
    #New keynames being added
    x$keynames = c(x$keynames, 'XCUTLO', 'XCUTHI', 'YCUTLO', 'YCUTHI') 
    
    #New history being added
    x$history = c(x$history, paste0('Subset of original image: x= ',safedim_i$lo_tar,':',safedim_i$hi_tar, ' / y= ', safedim_j$lo_tar,':',safedim_j$hi_tar))
    
    x$header = Rfits_keyvalues_to_header(x$keyvalues, x$keycomments, x$comment, x$history)
    x$raw = Rfits_header_to_raw(x$header)
    x$hdr = Rfits_keyvalues_to_hdr(x$keyvalues)
    
    #Now I don't think we need this, so prefer to remove it
    #detect minimal update:
    # updateloc_key = grep('NAXIS[1-2]|CRPIX[1-2]|ZNAXIS[1-2]', x$keynames)
    # updateloc_header = grep(paste(x$keynames[updateloc_key],collapse='|'), x$header)
    # if(length(updateloc_header) > 0){
    #   x$header[updateloc_header] = Rfits_keyvalues_to_header(x$keyvalues[updateloc_key], x$keycomments[updateloc_key])
    #   x$raw = Rfits_header_to_raw(x$header)
    # }
    
    output = list(
      imDat = tar,
      keyvalues = x$keyvalues,
      keycomments = x$keycomments,
      keynames = x$keynames,
      header = x$header,
      hdr = x$hdr,
      raw = x$raw,
      comment = x$comment,
      history = x$history,
      filename = x$filename,
      ext = x$ext
    )
    class(output) = "Rfits_image"
    return(output)
  }else{
    return(tar)
  }
}

`[.Rfits_cube` = function(x, i, j, k, keepWCS=TRUE, collapse=TRUE){
  
  if(missing(i)){i = c(1,dim(x$imDat)[1])}
  if(missing(j)){j = c(1,dim(x$imDat)[2])}
  if(missing(k)){k = c(1,dim(x$imDat)[3])}
  
  safedim_i = .safedim(1, dim(x$imDat)[1], min(i), max(i))
  safedim_j = .safedim(1, dim(x$imDat)[2], min(j), max(j))
  safedim_k = .safedim(1, dim(x$imDat)[3], min(k), max(k))
  
  tar = array(NA, dim=c(safedim_i$len_tar, safedim_j$len_tar, safedim_k$len_tar))
  if(safedim_i$safe & safedim_j$safe & safedim_k$safe){
    tar[safedim_i$tar,safedim_j$tar,safedim_k$tar] = x$imDat[safedim_i$orig,safedim_j$orig,safedim_k$orig]
  }
  
  if(keepWCS){
    if(!isTRUE(x$keyvalues$ZIMAGE)){
      x$keyvalues$NAXIS1 = safedim_i$len_tar
      x$keyvalues$NAXIS2 = safedim_j$len_tar
      x$keyvalues$NAXIS3 = safedim_k$len_tar
    }else{
      x$keyvalues$ZNAXIS1 = safedim_i$len_tar
      x$keyvalues$ZNAXIS2 = safedim_j$len_tar
      x$keyvalues$ZNAXIS3 = safedim_k$len_tar
    }
    if(!is.null(x$keyvalues$CRPIX1)){
      x$keyvalues$CRPIX1 = x$keyvalues$CRPIX1 - safedim_i$lo_tar + 1L
    }
    if(!is.null(x$keyvalues$CRPIX2)){
      x$keyvalues$CRPIX2 = x$keyvalues$CRPIX2 - safedim_j$lo_tar + 1L
    }
    if(!is.null(x$keyvalues$CRPIX3)){
      x$keyvalues$CRPIX3 = x$keyvalues$CRPIX3 - safedim_k$lo_tar + 1L
    }

    if(x$keyvalues$NAXIS3 == 1L & collapse){
      tar = tar[,,1]
      x$keyvalues$NAXIS3 = NULL
      x$keyvalues$CRPIX3 = NULL
      x$keycomments$NAXIS3 = NULL
      x$keycomments$CRPIX3 = NULL
      class_out = "Rfits_image"
    }else{
      class_out = "Rfits_cube"
    }
    
    #New keyvalues being added
    x$keyvalues$XCUTLO = safedim_i$lo_tar
    x$keyvalues$XCUTHI = safedim_i$hi_tar
    x$keyvalues$YCUTLO = safedim_j$lo_tar
    x$keyvalues$YCUTHI = safedim_j$hi_tar
    x$keyvalues$ZCUTLO = safedim_k$lo_tar
    x$keyvalues$ZCUTHI = safedim_k$hi_tar
    
    #New keycomments being added
    x$keycomments$XCUTLO = 'Low image x range'
    x$keycomments$XCUTHI = 'High image x range'
    x$keycomments$YCUTLO = 'Low image y range'
    x$keycomments$YCUTHI = 'High image y range'
    x$keycomments$ZCUTLO = 'Low image z range'
    x$keycomments$ZCUTHI = 'High image z range'
    
    #New keynames being added
    x$keynames = c(x$keynames, 'XCUTLO', 'XCUTHI', 'YCUTLO', 'YCUTHI', 'ZCUTLO', 'ZCUTHI') 
    
    #New history being added
    x$history = c(x$history, paste0('Subset of original image: x= ',safedim_i$lo_tar,':',safedim_i$hi_tar, ' / y= ', safedim_j$lo_tar,':',safedim_j$hi_tar, ' / z= ', safedim_k$lo_tar,':',safedim_k$hi_tar))
    
    x$header = Rfits_keyvalues_to_header(x$keyvalues, x$keycomments, x$comment, x$history)
    x$raw = Rfits_header_to_raw(x$header)
    x$hdr = Rfits_keyvalues_to_hdr(x$keyvalues)
    
    #detect minimal update:
    # updateloc_key = grep('NAXIS[1-3]|CRPIX[1-3]|ZNAXIS[1-3]', x$keynames)
    # updateloc_header = grep(paste(x$keynames[updateloc_key],collapse='|'), x$header)
    # if(length(updateloc_header) > 0){
    #   x$header[updateloc_header] = Rfits_keyvalues_to_header(x$keyvalues[updateloc_key], x$keycomments[updateloc_key])
    #   x$raw = Rfits_header_to_raw(x$header)
    # }
    
    output = list(
      imDat = tar,
      keyvalues = x$keyvalues,
      keycomments = x$keycomments,
      keynames = names(x$keyvalues),
      header = x$header,
      hdr = x$hdr,
      raw = x$raw,
      comment = x$comment,
      history = x$history,
      filename = x$filename,
      ext = x$ext
    )
    
    class(output) = class_out
    
    return(output)
  }else{
    return(tar)
  }
}

`[.Rfits_array` = function(x, i, j, k, m, keepWCS=TRUE, collapse=TRUE){
  
  if(missing(i)){i = c(1,dim(x$imDat)[1])}
  if(missing(j)){j = c(1,dim(x$imDat)[2])}
  if(missing(k)){k = c(1,dim(x$imDat)[3])}
  if(missing(m)){m = c(1,dim(x$imDat)[4])}
  
  safedim_i = .safedim(1, dim(x$imDat)[1], min(i), max(i))
  safedim_j = .safedim(1, dim(x$imDat)[2], min(j), max(j))
  safedim_k = .safedim(1, dim(x$imDat)[3], min(k), max(k))
  safedim_m = .safedim(1, dim(x$imDat)[4], min(m), max(m))
  
  tar = array(NA, dim=c(safedim_i$len_tar, safedim_j$len_tar, safedim_k$len_tar, safedim_m$len_tar))
  if(safedim_i$safe & safedim_j$safe & safedim_k$safe & safedim_m$safe){
    tar[safedim_i$tar,safedim_j$tar,safedim_k$tar,safedim_m$tar] = x$imDat[safedim_i$orig,safedim_j$orig,safedim_k$orig,safedim_m$orig]
  }
  
  if(keepWCS){
    if(!isTRUE(x$keyvalues$ZIMAGE)){
      x$keyvalues$NAXIS1 = safedim_i$len_tar
      x$keyvalues$NAXIS2 = safedim_j$len_tar
      x$keyvalues$NAXIS3 = safedim_k$len_tar
      x$keyvalues$NAXIS4 = safedim_m$len_tar
    }else{
      x$keyvalues$ZNAXIS1 = safedim_i$len_tar
      x$keyvalues$ZNAXIS2 = safedim_j$len_tar
      x$keyvalues$ZNAXIS3 = safedim_k$len_tar
      x$keyvalues$ZNAXIS4 = safedim_m$len_tar
    }
    if(!is.null(x$keyvalues$CRPIX1)){
      x$keyvalues$CRPIX1 = x$keyvalues$CRPIX1 - safedim_i$lo_tar + 1L
    }
    if(!is.null(x$keyvalues$CRPIX2)){
      x$keyvalues$CRPIX2 = x$keyvalues$CRPIX2 - safedim_j$lo_tar + 1L
    }
    if(!is.null(x$keyvalues$CRPIX3)){
      x$keyvalues$CRPIX3 = x$keyvalues$CRPIX3 - safedim_k$lo_tar + 1L
    }
    if(!is.null(x$keyvalues$CRPIX4)){
      x$keyvalues$CRPIX4 = x$keyvalues$CRPIX4 - safedim_m$lo_tar + 1L
    }
    
    if(x$keyvalues$NAXIS3 == 1L & x$keyvalues$NAXIS4 == 1L & collapse){
      tar = tar[,,1,1]
      x$keyvalues$NAXIS3 = NULL
      x$keyvalues$CRPIX3 = NULL
      x$keyvalues$CRPIX4 = NULL
      x$keyvalues$NAXIS4 = NULL
      
      x$keycomments$NAXIS3 = NULL
      x$keycomments$CRPIX3 = NULL
      x$keycomments$CRPIX4 = NULL
      x$keycomments$NAXIS4 = NULL
      class_out = "Rfits_image"
    }else if(x$keyvalues$NAXIS4 == 1L & collapse){
      tar = tar[,,,1]
      x$keyvalues$NAXIS4 = NULL
      x$keyvalues$CRPIX4 = NULL
      
      x$keycomments$NAXIS4 = NULL
      x$keycomments$CRPIX4 = NULL
      class_out = "Rfits_cube"
    }else{
      class_out = "Rfits_array"
    }
    
    #New keyvalues being added
    x$keyvalues$XCUTLO = safedim_i$lo_tar
    x$keyvalues$XCUTHI = safedim_i$hi_tar
    x$keyvalues$YCUTLO = safedim_j$lo_tar
    x$keyvalues$YCUTHI = safedim_j$hi_tar
    x$keyvalues$ZCUTLO = safedim_k$lo_tar
    x$keyvalues$ZCUTHI = safedim_k$hi_tar
    x$keyvalues$TCUTLO = safedim_m$lo_tar
    x$keyvalues$TCUTHI = safedim_m$hi_tar
    
    #New keycomments being added
    x$keycomments$XCUTLO = 'Low image x range'
    x$keycomments$XCUTHI = 'High image x range'
    x$keycomments$YCUTLO = 'Low image y range'
    x$keycomments$YCUTHI = 'High image y range'
    x$keycomments$ZCUTLO = 'Low image z range'
    x$keycomments$ZCUTHI = 'High image z range'
    x$keycomments$TCUTLO = 'Low image t range'
    x$keycomments$TCUTHI = 'High image t range'
    
    #New keynames being added
    x$keynames = c(x$keynames, 'XCUTLO', 'XCUTHI', 'YCUTLO', 'YCUTHI', 'ZCUTLO', 'ZCUTHI', 'TCUTLO', 'TCUTHI') 
    
    #New history being added
    x$history = c(x$history, paste0('Subset of original image: x= ',safedim_i$lo_tar,':',safedim_i$hi_tar, ' / y= ', safedim_j$lo_tar,':',safedim_j$hi_tar, ' / z= ', safedim_k$lo_tar,':',safedim_k$hi_tar, ' / t= ', safedim_m$lo_tar,':',safedim_m$hi_tar))
    
    x$header = Rfits_keyvalues_to_header(x$keyvalues, x$keycomments, x$comment, x$history)
    x$raw = Rfits_header_to_raw(x$header)
    x$hdr = Rfits_keyvalues_to_hdr(x$keyvalues)
    
    #detect minimal update:
    # updateloc_key = grep('NAXIS[1-4]|CRPIX[1-4]|ZNAXIS[1-4]', x$keynames)
    # updateloc_header = grep(paste(x$keynames[updateloc_key],collapse='|'), x$header)
    # if(length(updateloc_header) > 0){
    #   x$header[updateloc_header] = Rfits_keyvalues_to_header(x$keyvalues[updateloc_key], x$keycomments[updateloc_key])
    #   x$raw = Rfits_header_to_raw(x$header)
    # }
    
    output = list(
      imDat = tar,
      keyvalues = x$keyvalues,
      keycomments = x$keycomments,
      keynames = names(x$keyvalues),
      header = x$header,
      hdr = x$hdr,
      raw = x$raw,
      comment = x$comment,
      history = x$history,
      filename = x$filename,
      ext = x$ext
    )
    
    class(output) = class_out
    
    return(output)
  }else{
    return(tar)
  }
}

`[.Rfits_pointer` = function(x, i, j, k, m, box=201, type='pix', header=x$header){
  
  if(!missing(i)){
    if(length(i)==2 & missing(j)){
      if(i[2]-i[1] != 1){
        j = ceiling(i[2])
        i = ceiling(i[1])
      }
    }
  }
  
  if(type=='coord'){
    if(requireNamespace("Rwcs", quietly=TRUE)){
      assertNumeric(i,len=1)
      assertNumeric(j,len=1)
      ij = Rwcs::Rwcs_s2p(i,j,keyvalues=x$keyvalues,pixcen='R',header=x$raw)[1,]
      i = ceiling(ij[1])
      j = ceiling(ij[2])
    }else{
      message('The Rwcs package is needed to use type=coord.')
    }
  }
  
  if(isTRUE(x$keyvalues$ZIMAGE)){
    naxis1 = x$keyvalues$ZNAXIS1
    naxis2 = x$keyvalues$ZNAXIS2
    naxis3 = x$keyvalues$ZNAXIS3
    naxis4 = x$keyvalues$ZNAXIS4
    datatype = x$keyvalues$ZBITPIX
  }else{
    naxis1 = x$keyvalues$NAXIS1
    naxis2 = x$keyvalues$NAXIS2
    naxis3 = x$keyvalues$NAXIS3
    naxis4 = x$keyvalues$NAXIS4
    datatype = x$keyvalues$BITPIX
  }
  
  Ndim = 1
  if(!is.null(naxis2)){Ndim = 2}
  if(!is.null(naxis3)){Ndim = 3}
  if(!is.null(naxis4)){Ndim = 4}
  
  if(Ndim == 2){
    if(length(box) == 1){box = c(box,box)}
    if(!missing(i)){
      if(length(i) == 1){i = ceiling(i + c(-(box[1]-1)/2, (box[1]-1)/2))}
    }
    if(!missing(j)){
      if(length(j) == 1){j = ceiling(j + c(-(box[2]-1)/2, (box[2]-1)/2))}
    }
  }
  
  if(!missing(i)){
    if(is.null(naxis1)){stop('NAXIS1 is NULL: specifying too many dimensions!')}
    xlo = ceiling(min(i))
    xhi = ceiling(max(i))
  }else{
    xlo = NULL
    xhi = NULL
  }
  if(!missing(j)){
    if(is.null(naxis2)){stop('NAXIS2 is NULL: specifying too many dimensions!')}
    ylo = ceiling(min(j))
    yhi = ceiling(max(j))
  }else{
    ylo = NULL
    yhi = NULL
  }
  if(!missing(k)){
    if(is.null(naxis3)){stop('NAXIS3 is NULL: specifying too many dimensions!')}
    zlo = ceiling(min(k))
    zhi = ceiling(max(k))
  }else{
    zlo = NULL
    zhi = NULL
  }
  if(!missing(m)){
    if(is.null(naxis4)){stop('NAXIS4 is NULL: specifying too many dimensions!')}
    tlo = ceiling(min(m))
    thi = ceiling(max(m))
  }else{
    tlo = NULL
    thi = NULL
  }
  
  return(Rfits_read_image(filename=x$filename, ext=x$ext, header=header,
                          xlo=xlo, xhi=xhi, ylo=ylo, yhi=yhi, zlo=zlo, zhi=zhi,
                          tlo=tlo, thi=thi))
}

`&.Rfits_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_pointer'))
  e1[,header=FALSE] & e2[,header=FALSE]
}

`|.Rfits_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_pointer'))
  e1[,header=FALSE] | e2[,header=FALSE]
}

`!=.Rfits_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_pointer'))
  e1[,header=FALSE] != e2[,header=FALSE]
}

`==.Rfits_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_pointer'))
  e1[,header=FALSE] == e2[,header=FALSE]
}

`<.Rfits_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_pointer'))
  e1[,header=FALSE] < e2[,header=FALSE]
}

`<=.Rfits_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_pointer'))
  e1[,header=FALSE] <= e2[,header=FALSE]
}

`>.Rfits_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_pointer'))
  e1[,header=FALSE] > e2[,header=FALSE]
}

`>=.Rfits_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_pointer'))
  e1[,header=FALSE] >= e2[,header=FALSE]
}

`+.Rfits_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_pointer'))
  e1[,header=FALSE] + e2[,header=FALSE]
}

`-.Rfits_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_pointer'))
  e1[,header=FALSE] - e2[,header=FALSE]
}

`*.Rfits_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_pointer'))
  e1[,header=FALSE] * e2[,header=FALSE]
}

`/.Rfits_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_pointer'))
  e1[,header=FALSE] / e2[,header=FALSE]
}

`^.Rfits_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_pointer'))
  e1[,header=FALSE] ^ e2[,header=FALSE]
}

`%%.Rfits_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_pointer'))
  e1[,header=FALSE] %% e2[,header=FALSE]
}

`%/%.Rfits_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_pointer'))
  e1[,header=FALSE] %/% e2[,header=FALSE]
}

`%*%.Rfits_pointer`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_pointer'))
  e1[,header=FALSE] %*% e2[,header=FALSE]
}
