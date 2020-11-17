.safedim = function(lo_orig=1, hi_orig=1, lo_tar=1, hi_tar=1){
  len_orig = hi_orig - lo_orig + 1
  len_tar = hi_tar - lo_tar + 1
  
  out_lo_orig = max(lo_tar, 1)
  out_hi_orig = min(hi_tar, len_orig)
  diff = (1 - lo_tar)
  tar_lo = out_lo_orig + diff
  tar_hi = out_hi_orig + diff
  safe = tar_hi >= tar_lo
  return(list(orig = out_lo_orig:out_hi_orig, tar = tar_lo:tar_hi, len_orig=len_orig,
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
  
  dim = c(keyvalues$NAXIS1); type='vector'
  if(!is.null(keyvalues$NAXIS2)){dim = c(dim, keyvalues$NAXIS2); type='image'}
  if(!is.null(keyvalues$NAXIS3)){dim = c(dim, keyvalues$NAXIS3); type='cube'}
  if(!is.null(keyvalues$NAXIS4)){dim = c(dim, keyvalues$NAXIS4); type='array'}
  
  output = list(filename=filename, ext=ext, keyvalues=keyvalues, header=header, dim=dim, type=type)
  class(output) = 'Rfits_pointer'
  return(invisible(output))
}

print.Rfits_vector=function(x , ...){
  cat('Class: Rfits_image\n')
  cat('File path:',x$filename,'\n')
  cat('Ext num:',x$ext,'\n')
  cat('Ext name:',x$keyvalues[['EXTNAME']],'\n')
  cat('RAM size:',round(object.size(x)/(2^20),4),'MB\n')
  cat('BITPIX:',x$keyvalues[['BITPIX']],'\n')
  cat('NAXIS1:',x$keyvalues[['NAXIS1']],'\n')
}

print.Rfits_image=function(x , ...){
  cat('Class: Rfits_image\n')
  cat('File path:',x$filename,'\n')
  cat('Ext num:',x$ext,'\n')
  cat('Ext name:',x$keyvalues[['EXTNAME']],'\n')
  cat('RAM size:',round(object.size(x)/(2^20),4),'MB\n')
  cat('BITPIX:',x$keyvalues[['BITPIX']],'\n')
  cat('NAXIS1:',x$keyvalues[['NAXIS1']],'\n')
  cat('NAXIS2:',x$keyvalues[['NAXIS2']],'\n')
}

print.Rfits_cube=function(x , ...){
  cat('Class: Rfits_cube\n')
  cat('File path:',x$filename,'\n')
  cat('Ext num:',x$ext,'\n')
  cat('Ext name:',x$keyvalues[['EXTNAME']],'\n')
  cat('RAM size:',round(object.size(x)/(2^20),4),'MB\n')
  cat('BITPIX:',x$keyvalues[['BITPIX']],'\n')
  cat('NAXIS1:',x$keyvalues[['NAXIS1']],'\n')
  cat('NAXIS2:',x$keyvalues[['NAXIS2']],'\n')
  cat('NAXIS3:',x$keyvalues[['NAXIS3']],'\n')
}

print.Rfits_array=function(x , ...){
  cat('Class: Rfits_array\n')
  cat('File path:',x$filename,'\n')
  cat('Ext num:',x$ext,'\n')
  cat('Ext name:',x$keyvalues[['EXTNAME']],'\n')
  cat('RAM size:',round(object.size(x)/(2^20),4),'MB\n')
  cat('BITPIX:',x$keyvalues[['BITPIX']],'\n')
  cat('NAXIS1:',x$keyvalues[['NAXIS1']],'\n')
  cat('NAXIS2:',x$keyvalues[['NAXIS2']],'\n')
  cat('NAXIS3:',x$keyvalues[['NAXIS3']],'\n')
  cat('NAXIS4:',x$keyvalues[['NAXIS4']],'\n')
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
}

print.Rfits_header=function(x, ...){
  cat(x$header[1:8], sep='\n')
}

print.Rfits_list=function(x , ...){
  
  # ext_name = {}
  # for(i in 1:length(x)){
  #   if(is.null(x[[i]]$keyvalues$EXTNAME)){
  #     if(is.null(attributes(x[[i]])$keyvalues$EXTNAME)){
  #       ext_name = c(ext_name, 'NA')
  #     }else{
  #       ext_name = c(ext_name, attributes(x[[i]])$keyvalues$EXTNAME)
  #     }
  #   }else{
  #     ext_name = c(ext_name, x[[i]]$keyvalues$EXTNAME)
  #   }
  # }
  ext_name = names(x)
  
  ext_class = {}
  for(i in 1:length(x)){
    ext_class = c(ext_class, class(x[[i]])[1])
  }
  
  ext_dim = {}
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
  }
  
  ext_size = {}
  for(i in 1:length(x)){
    ext_size = c(ext_size, round(object.size(x[[i]])/(2^20),4))
  }
  
  summarytable = data.frame(
    'Ext'= 1:length(x),
    'Name' = ext_name,
    'Class' = ext_class,
    'Dim' = ext_dim,
    'Size/MB' = ext_size
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

`[.Rfits_vector` = function(x, i, keepWCS=FALSE){
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

`[.Rfits_image` = function(x, i, j, keepWCS=FALSE){
  
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

`[.Rfits_cube` = function(x, i, j, k, keepWCS=FALSE){
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

`[.Rfits_array` = function(x, i, j, k, m, keepWCS=FALSE){
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

`[.Rfits_pointer` = function(x, i, j, k, m, header=x$header){
  if(!missing(i)){
    if(is.null(x$keyvalues$NAXIS1)){stop('NAXIS1 is NULL: specifying too many dimensions!')}
    xlo=min(i)
    xhi=max(i)
    #if(xlo < 1 | xhi < 1){stop('All i must be >= 1')}
    #if(xlo > x$keyvalues$NAXIS1 | xhi > x$keyvalues$NAXIS1){stop('All i must be <=', x$keyvalues$NAXIS1)}
  }else{
    xlo=NULL
    xhi=NULL
  }
  if(!missing(j)){
    if(is.null(x$keyvalues$NAXIS2)){stop('NAXIS2 is NULL: specifying too many dimensions!')}
    ylo=min(j)
    yhi=max(j)
    #if(ylo < 1 | yhi < 1){stop('All j must be >= 1')}
    #if(ylo > x$keyvalues$NAXIS2 | yhi > x$keyvalues$NAXIS2){stop('All j must be <=', x$keyvalues$NAXIS2)}
  }else{
    ylo=NULL
    yhi=NULL
  }
  if(!missing(k)){
    if(is.null(x$keyvalues$NAXIS3)){stop('NAXIS3 is NULL: specifying too many dimensions!')}
    zlo=min(k)
    zhi=max(k)
    #if(zlo < 1 | zhi < 1){stop('All k must be >= 1')}
    #if(zlo > x$keyvalues$NAXIS3 | zhi > x$keyvalues$NAXIS3){stop('All j must be <=', x$keyvalues$NAXIS3)}
  }else{
    zlo=NULL
    zhi=NULL
  }
  if(!missing(m)){
    if(is.null(x$keyvalues$NAXIS4)){stop('NAXIS4 is NULL: specifying too many dimensions!')}
    tlo=min(m)
    thi=max(m)
    #if(tlo < 1 | thi < 1){stop('All m must be >= 1')}
    #if(tlo > x$keyvalues$NAXIS4 | thi > x$keyvalues$NAXIS4){stop('All j must be <=', x$keyvalues$NAXIS4)}
  }else{
    tlo=NULL
    thi=NULL
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
