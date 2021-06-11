.safedim = function(lo_orig=1, hi_orig=1, lo_tar=1, hi_tar=1){
  len_orig = hi_orig - lo_orig + 1
  len_tar = hi_tar - lo_tar + 1
  
  out_lo_orig = max(lo_tar, 1)
  out_hi_orig = min(hi_tar, len_orig)
  diff = (1 - lo_tar)
  out_lo_tar = out_lo_orig + diff
  out_hi_tar = out_hi_orig + diff
  safe = (out_hi_tar >= out_lo_tar) & (out_hi_orig & out_lo_orig)
  return(list(orig = out_lo_orig:out_hi_orig, tar = out_lo_tar:out_hi_tar, len_orig=len_orig,
              len_tar=len_tar, safe=safe, lo_orig=lo_orig, hi_orig=hi_orig, lo_tar=lo_tar,
              hi_tar=hi_tar, diff=diff))
}

Rfits_point=function(filename='temp.fits', ext=1, header=FALSE){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  assertAccess(filename, access='r')
  assertIntegerish(ext, len=1)
  assertFlag(header)
  
  keyvalues = Rfits_read_header(filename=filename, ext=ext)$keyvalues
  
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
  
  output = list(filename=filename, ext=ext, keyvalues=keyvalues, header=header, dim=dim, type=type)
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
  
  cat('Class: Rfits_image\n')
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
    if(inherits(x[[i]], what=c('Rfits_image', 'Rfits_cube', 'Rfits_array', 'Rfits_vector', 'Rfits_pointer'))){
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

`[.Rfits_vector` = function(x, i, keepWCS=TRUE){
  
  if(missing(i)){i = c(1,length(x$imDat))}
  
  safedim_i = .safedim(1, length(x$imDat), min(i), max(i))
  
  tar = array(NA, dim=safedim_i$len_tar)
  if(safedim_i$safe){
    tar[safedim_i$tar] = x$imDat[safedim_i$orig]
  }
    
  if(keepWCS){
    keyvalues = x$keyvalues
    keyvalues$NAXIS1 = safedim_i$len_tar
    keyvalues$CRPIX1 = keyvalues$CRPIX1 - safedim_i$lo_tar + 1
    output = list(
      imDat = tar,
      keyvalues = keyvalues,
      keycomments = x$keycomments,
      keynames = x$keynames,
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
  if(length(i) == 1){i = ceiling(i + c(-(box[1]-1)/2, (box[1]-1)/2))}
  if(length(j) == 1){j = ceiling(j + c(-(box[2]-1)/2, (box[2]-1)/2))}
  
  safedim_i = .safedim(1, dim(x$imDat)[1], min(i), max(i))
  safedim_j = .safedim(1, dim(x$imDat)[2], min(j), max(j))
  
  tar = array(NA, dim=c(safedim_i$len_tar, safedim_j$len_tar))
  if(safedim_i$safe & safedim_j$safe){
    tar[safedim_i$tar,safedim_j$tar] = x$imDat[safedim_i$orig,safedim_j$orig]
  }

  if(keepWCS){
    keyvalues = x$keyvalues
    keyvalues$NAXIS1 = safedim_i$len_tar
    keyvalues$NAXIS2 = safedim_j$len_tar
    keyvalues$CRPIX1 = keyvalues$CRPIX1 - safedim_i$lo_tar + 1
    keyvalues$CRPIX2 = keyvalues$CRPIX2 - safedim_j$lo_tar + 1
    
    #New keyvalues being added
    keyvalues$XCUTLO = safedim_i$lo_orig
    keyvalues$XCUTHI = safedim_i$hi_orig
    keyvalues$YCUTLO = safedim_j$lo_orig
    keyvalues$YCUTHI = safedim_j$hi_orig
    
    #New keycomments being added
    x$keycomments$XCUTLO = 'Low image x range'
    x$keycomments$XCUTHI = 'High image x range'
    x$keycomments$YCUTLO = 'Low image y range'
    x$keycomments$YCUTHI = 'High image y range'
    
    #New keynames being added
    x$keynames = c(x$keynames, 'XCUTLO', 'XCUTHI', 'YCUTLO', 'YCUTHI') 
    
    x$history = c(x$history, paste0('Subset of original image: x= ',safedim_i$lo_orig,':',safedim_i$hi_orig, ' / y= ', safedim_j$lo_orig,':',safedim_j$hi_orig))
    
    #header = Rfits_keyvalues_to_header(keyvalues, x$keycomments, x$comment, x$history)
    
    hdr = Rfits_keyvalues_to_hdr(keyvalues)
    header = Rfits_keyvalues_to_header(keyvalues, x$keycomments, x$comment, x$history)
    raw = Rfits_header_to_raw(header)
    
    output = list(
      imDat = tar,
      keyvalues = keyvalues,
      keycomments = x$keycomments,
      keynames = x$keynames,
      header = header,
      hdr = hdr,
      raw = raw,
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

`[.Rfits_cube` = function(x, i, j, k, keepWCS=TRUE){
  
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
    keyvalues = x$keyvalues
    keyvalues$NAXIS1 = safedim_i$len_tar
    keyvalues$NAXIS2 = safedim_j$len_tar
    keyvalues$NAXIS3 = safedim_k$len_tar
    keyvalues$CRPIX1 = keyvalues$CRPIX1 - safedim_i$lo_tar + 1
    keyvalues$CRPIX2 = keyvalues$CRPIX2 - safedim_j$lo_tar + 1
    keyvalues$CRPIX3 = keyvalues$CRPIX3 - safedim_k$lo_tar + 1
    
    output = list(
      imDat = tar,
      keyvalues = keyvalues,
      keycomments = x$keycomments,
      keynames = x$keynames,
      comment = x$comment,
      history = x$history,
      filename = x$filename,
      ext = x$ext
    )
    class(output) = "Rfits_cube"
    return(output)
  }else{
    return(x$imDat[i,j,k])
  }
}

`[.Rfits_array` = function(x, i, j, k, m, keepWCS=TRUE){
  
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
    keyvalues = x$keyvalues
    keyvalues$NAXIS1 = safedim_i$len_tar
    keyvalues$NAXIS2 = safedim_j$len_tar
    keyvalues$NAXIS3 = safedim_k$len_tar
    keyvalues$NAXIS4 = safedim_m$len_tar
    keyvalues$CRPIX1 = keyvalues$CRPIX1 - safedim_i$lo_tar + 1
    keyvalues$CRPIX2 = keyvalues$CRPIX2 - safedim_j$lo_tar + 1
    keyvalues$CRPIX3 = keyvalues$CRPIX3 - safedim_k$lo_tar + 1
    keyvalues$CRPIX4 = keyvalues$CRPIX4 - safedim_m$lo_tar + 1
    
    output = list(
      imDat = tar,
      keyvalues = keyvalues,
      keycomments = x$keycomments,
      keynames = x$keynames,
      comment = x$comment,
      history = x$history,
      filename = x$filename,
      ext = x$ext
    )
    class(output) = "Rfits_array"
    return(output)
  }else{
    return(tar)
  }
}

`[.Rfits_pointer` = function(x, i, j, k, m, box=201, type='pix', header=x$header){
  
  if(!missing(i)){
    if(length(i)==2 & missing(j)){
      if(i[2]-i[1] !=1){
        j = as.numeric(i[2])
        i = as.numeric(i[1])
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
    xlo = min(i)
    xhi = max(i)
  }else{
    xlo = NULL
    xhi = NULL
  }
  if(!missing(j)){
    if(is.null(naxis2)){stop('NAXIS2 is NULL: specifying too many dimensions!')}
    ylo = min(j)
    yhi = max(j)
  }else{
    ylo = NULL
    yhi = NULL
  }
  if(!missing(k)){
    if(is.null(naxis3)){stop('NAXIS3 is NULL: specifying too many dimensions!')}
    zlo = min(k)
    zhi = max(k)
  }else{
    zlo = NULL
    zhi = NULL
  }
  if(!missing(m)){
    if(is.null(naxis4)){stop('NAXIS4 is NULL: specifying too many dimensions!')}
    tlo = min(m)
    thi = max(m)
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
