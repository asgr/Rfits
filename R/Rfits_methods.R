.safedim = function(lo_orig=1L, hi_orig=1L, lo_tar=1L, hi_tar=1L){
  len_orig = hi_orig - lo_orig + 1L
  len_tar = hi_tar - lo_tar + 1L
  
  out_lo_orig = max(lo_tar, 1L)
  out_hi_orig = min(hi_tar, len_orig)
  diff = (1L - lo_tar)
  out_lo_tar = out_lo_orig + diff
  out_hi_tar = out_hi_orig + diff
  safe = (out_hi_tar >= out_lo_tar) & (out_hi_orig >= out_lo_orig)
  return(list(orig = out_lo_orig:out_hi_orig, tar = out_lo_tar:out_hi_tar, len_orig=len_orig,
              len_tar=len_tar, safe=safe, lo_orig=lo_orig, hi_orig=hi_orig, lo_tar=lo_tar,
              hi_tar=hi_tar, diff=diff))
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
  cat(x$header[1:min(8,length(x$header))], sep='\n')
  cat("...", sep='\n')
  cat('Key N:',length(x$keyvalues),'\n')
}

# print.Rfits_keylist=function(x, ...){
#   cat(x[1:min(8,length(x))], sep='\n')
#   cat("...", sep='\n')
#   cat('Key N:',length(x),'\n')
# }

print.Rfits_list=function(x , ...){
  
  ext_name = names(x)
  if(is.null(ext_name)){
    ext_name = rep(NA, length(x))
  }
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

length.Rfits_vector=function(x){
  return(dim(x))
}

length.Rfits_image=function(x){
  if(is.null(dim(x))){
    return(length(x$imDat))
  }else{
    return(prod(dim(x)))
  }
}

length.Rfits_cube = length.Rfits_image
length.Rfits_array = length.Rfits_image
length.Rfits_pointer = length.Rfits_image
length.Rfits_header = length.Rfits_image
#length.Rfits_keylist = length.Rfits_image I think length might be confusing (probably just want to now the number of entries)

Rfits_length = function(filename, ext=1){
  temp_header = Rfits_read_header(filename=filename, ext=ext)
  return(length(temp_header))
}

dim.Rfits_image = function(x){
  if(inherits(x$imDat, 'array')){
    return(dim(x$imDat))
  }else{
    return(length(x$imDat))
  }
}

dim.Rfits_vector = dim.Rfits_image
dim.Rfits_cube = dim.Rfits_image
dim.Rfits_array = dim.Rfits_image

dim.Rfits_pointer=function(x){
  return(x$dim)
}

dim.Rfits_header=function(x){
  if(isTRUE(x$keyvalues$ZIMAGE)){
    if(!is.null(x$keyvalues$ZNAXIS1)){
      NAXIS1 = x$keyvalues$ZNAXIS1
    }else{
      NAXIS1 = NULL
    }
    if(!is.null(x$keyvalues$ZNAXIS2)){
      NAXIS2 = x$keyvalues$ZNAXIS2
    }else{
      NAXIS2 = NULL
    }
    if(!is.null(x$keyvalues$ZNAXIS3)){
      NAXIS3 = x$keyvalues$ZNAXIS3
    }else{
      NAXIS3 = NULL
    }
    if(!is.null(x$keyvalues$ZNAXIS4)){
      NAXIS4 = x$keyvalues$ZNAXIS4
    }else{
      NAXIS4 = NULL
    }
  }else{
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
  }
  return(c(NAXIS1, NAXIS2, NAXIS3, NAXIS4))
}

dim.Rfits_keylist=function(x){
  if(isTRUE(x$ZIMAGE)){
    if(!is.null(x$ZNAXIS1)){
      NAXIS1 = x$ZNAXIS1
    }else{
      NAXIS1 = NULL
    }
    if(!is.null(x$ZNAXIS2)){
      NAXIS2 = x$ZNAXIS2
    }else{
      NAXIS2 = NULL
    }
    if(!is.null(x$ZNAXIS3)){
      NAXIS3 = x$ZNAXIS3
    }else{
      NAXIS3 = NULL
    }
    if(!is.null(x$ZNAXIS4)){
      NAXIS4 = x$ZNAXIS4
    }else{
      NAXIS4 = NULL
    }
  }else{
    if(!is.null(x$NAXIS1)){
      NAXIS1 = x$NAXIS1
    }else{
      NAXIS1 = NULL
    }
    if(!is.null(x$NAXIS2)){
      NAXIS2 = x$NAXIS2
    }else{
      NAXIS2 = NULL
    }
    if(!is.null(x$NAXIS3)){
      NAXIS3 = x$NAXIS3
    }else{
      NAXIS3 = NULL
    }
    if(!is.null(x$NAXIS4)){
      NAXIS4 = x$NAXIS4
    }else{
      NAXIS4 = NULL
    }
  }
  return(c(NAXIS1, NAXIS2, NAXIS3, NAXIS4))
}

Rfits_dim = function(filename, ext=1){
  temp_header = Rfits_read_header(filename=filename, ext=ext)
  return(dim(temp_header))
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
    #x$keynames = c(x$keynames, 'XCUTLO', 'XCUTHI') 
    x$keynames['XCUTLO'] = 'XCUTLO'
    x$keynames['XCUTHI'] = 'XCUTHI'
    
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
  
  if(length(i) == 1 & length(j) == 1){
    if(length(box) == 1){box = c(box,box)}
    i = ceiling(i + c(-(box[1]-1L)/2, (box[1]-1L)/2))
    j = ceiling(j + c(-(box[2]-1L)/2, (box[2]-1L)/2))
  }
  
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
    #x$keynames = c(x$keynames, 'XCUTLO', 'XCUTHI', 'YCUTLO', 'YCUTHI') 
    #x$keynames['XCUTLO'] = 'XCUTLO'
    #x$keynames['XCUTHI'] = 'XCUTHI'
    #x$keynames['YCUTLO'] = 'YCUTLO'
    #x$keynames['YCUTHI'] = 'YCUTHI'
    x$keynames = names(x$keyvalues)
    
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
    #x$keynames = c(x$keynames, 'XCUTLO', 'XCUTHI', 'YCUTLO', 'YCUTHI', 'ZCUTLO', 'ZCUTHI') 
    x$keynames['XCUTLO'] = 'XCUTLO'
    x$keynames['XCUTHI'] = 'XCUTHI'
    x$keynames['YCUTLO'] = 'YCUTLO'
    x$keynames['YCUTHI'] = 'YCUTHI'
    x$keynames['ZCUTLO'] = 'ZCUTLO'
    x$keynames['ZCUTHI'] = 'ZCUTHI'
    
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
    #x$keynames = c(x$keynames, 'XCUTLO', 'XCUTHI', 'YCUTLO', 'YCUTHI', 'ZCUTLO', 'ZCUTHI', 'TCUTLO', 'TCUTHI') 
    x$keynames['XCUTLO'] = 'XCUTLO'
    x$keynames['XCUTHI'] = 'XCUTHI'
    x$keynames['YCUTLO'] = 'YCUTLO'
    x$keynames['YCUTHI'] = 'YCUTHI'
    x$keynames['ZCUTLO'] = 'ZCUTLO'
    x$keynames['ZCUTHI'] = 'ZCUTHI'
    x$keynames['TCUTLO'] = 'TCUTLO'
    x$keynames['TCUTHI'] = 'TCUTHI'
    
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

`[.Rfits_pointer` = function(x, i, j, k, m, box=201, type='pix', header=x$header, sparse=x$sparse){
  
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
    if(!missing(i) & !missing(j)){
      if(length(i) == 1 & length(j) == 1){
        if(length(box) == 1){box = c(box,box)}
        i = ceiling(i + c(-(box[1]-1L)/2, (box[1]-1L)/2))
        j = ceiling(j + c(-(box[2]-1L)/2, (box[2]-1L)/2))
      }
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
                          tlo=tlo, thi=thi, sparse=sparse))
}

`[<-.Rfits_pointer` = function(x, i, j, k, m, allow_write=x$allow_write, value){
  if(allow_write == FALSE){
    stop('allow_write = FALSE!')
  }
  
  dims = x$dim
  
  if(length(dim(value)) > length(dims)){
    stop('Replacement object has more dimensions than target FITS!')
  }
  
  if(!missing(i)){
    if(dim(value)[1] != diff(range(i)) + 1L){
      stop('dim x (1) of replacement does not match subset selection!')
    }
    Npix = diff(range(i)) + 1L
  }else{
    if(length(dims) >= 1){
      i = 1:dims[1]
      Npix = diff(range(i)) + 1L
    }
  }
  if(!missing(j)){
    if(dim(value)[2] != diff(range(j)) + 1L){
      stop('dim y (2) of replacement does not match subset selection!')
    }
    Npix = Npix*(diff(range(j)) + 1L)
  }else{
    if(length(dims) >= 2){
      j = 1:dims[2]
      Npix = Npix*(diff(range(j)) + 1L)
    }
  }
  if(!missing(k)){
    if(dim(value)[3] != diff(range(k)) + 1L){
      stop('dim z (3) of replacement does not match subset selection!')
    }
    Npix = Npix*(diff(range(k)) + 1L)
  }else{
    if(length(dims) >= 3){
      k = 1:dims[3]
      Npix = Npix*(diff(range(k)) + 1L)
    }
  }
  if(!missing(m)){
    if(dim(value)[4] != diff(range(m)) + 1L){
      stop('dim t (4) of replacement does not match subset selection!')
    }
    Npix = Npix*(diff(range(m)) + 1L)
  }else{
    if(length(dims) == 4){
      m = 1:dims[4]
      Npix = Npix*(diff(range(m)) + 1L)
    }
  }
  
  if(length(value) != Npix){
    stop('Number of replacement pixels mismatches number of subset pixels!')
  }
  
  if(x$type == 'vector'){
    Rfits_write_pix(data=value, filename=x$filename, ext=x$ext, xlo=min(i, na.rm=TRUE))
  }
  if(x$type == 'image'){
    Rfits_write_pix(data=value, filename=x$filename, ext=x$ext, xlo=min(j, na.rm=TRUE), ylo=min(j, na.rm=TRUE))
  }
  if(x$type == 'cube'){
    Rfits_write_pix(data=value, filename=x$filename, ext=x$ext, xlo=min(j, na.rm=TRUE), ylo=min(j, na.rm=TRUE), zlo=min(k, na.rm=TRUE))
  }
  if(x$type == 'array'){
    Rfits_write_pix(data=value, filename=x$filename, ext=x$ext, xlo=min(j, na.rm=TRUE), ylo=min(j, na.rm=TRUE), zlo=min(k, na.rm=TRUE), tlo=min(m, na.rm=TRUE))
  }
  
  return(Rfits_point(filename=x$filename, ext=x$ext, header=x$header, zap=x$zap, allow_write=x$allow_write))
}

`&.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat & e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat & e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat & e2
  }
  return(e1)
}

`|.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat | e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat | e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat | e2
  }
  return(e1)
}

`!=.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat != e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat != e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat != e2
  }
  return(e1)
}

`==.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat == e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat == e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat == e2
  }
  return(e1)
}

`<.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat < e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat < e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat < e2
  }
  return(e1)
}

`<=.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat <= e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat <= e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat <= e2
  }
  return(e1)
}

`>.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat > e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat > e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat > e2
  }
  return(e1)
}

`>=.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat >= e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat >= e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat >= e2
  }
  return(e1)
}

`+.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat + e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat + e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat + e2
  }
  return(e1)
}

`-.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat - e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat - e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat - e2
  }
  return(e1)
}

`*.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat * e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat * e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat * e2
  }
  return(e1)
}

`/.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat / e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat / e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat / e2
  }
  return(e1)
}

`^.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat ^ e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat ^ e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat ^ e2
  }
  return(e1)
}

`%%.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat %% e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat %% e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat %% e2
  }
  return(e1)
}

`%*%.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat %*% e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat %*% e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat %*% e2
  }
  return(e1)
}

`%/%.Rfits_image`=function(e1, e2){
  if (missing(e2)) 
    return(e1)
  if (inherits(e2, 'Rfits_image')){
    e1$imDat =  e1$imDat %/% e2$imDat
  }else if (inherits(e2, 'Rfits_pointer')){
    e1$imDat =  e1$imDat %/% e2[,]$imDat
  }else{
    e1$imDat =  e1$imDat %/% e2
  }
  return(e1)
}

`&.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] & e2[,])
  }else{
    return(e1[,] & e2)
  }
}

`|.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] | e2[,])
  }else{
    return(e1[,] | e2)
  }
}

`!=.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] != e2[,])
  }else{
    return(e1[,] != e2)
  }
}

`==.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] == e2[,])
  }else{
    return(e1[,] == e2)
  }
}

`<.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] < e2[,])
  }else{
    return(e1[,] < e2)
  }
}

`<=.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] <= e2[,])
  }else{
    return(e1[,] <= e2)
  }
}

`>.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] > e2[,])
  }else{
    return(e1[,] > e2)
  }
}

`>=.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] >= e2[,])
  }else{
    return(e1[,] >= e2)
  }
}

`+.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] + e2[,])
  }else{
    return(e1[,] + e2)
  }
}

`-.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] - e2[,])
  }else{
    return(e1[,] - e2)
  }
}

`*.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] * e2[,])
  }else{
    return(e1[,] * e2)
  }
}

`/.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] / e2[,])
  }else{
    return(e1[,] / e2)
  }
}

`^.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] ^ e2[,])
  }else{
    return(e1[,] ^ e2)
  }
}

`%%.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] %% e2[,])
  }else{
    return(e1[,] %% e2)
  }
}

`%*%.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] %*% e2[,])
  }else{
    return(e1[,] %*% e2)
  }
}

`%/%.Rfits_pointer`=function(e1, e2){
  if (inherits(e2, 'Rfits_pointer')){
    return(e1[,] %/% e2[,])
  }else{
    return(e1[,] %/% e2)
  }
}
