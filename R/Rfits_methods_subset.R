`[.Rfits_vector` = function(x, i, header=TRUE){
  
  if(missing(i)){i = c(1,length(x$imDat))}
  
  safedim_i = .safedim(1, length(x$imDat), min(i), max(i))
  
  tar = array(NA, dim=safedim_i$len_tar)
  if(safedim_i$safe){
    tar[safedim_i$tar] = x$imDat[safedim_i$orig]
  }
  
  if(header){
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

`[.Rfits_image` = function(x, i, j, box=201, type='pix', header=TRUE){
  
  xdim = dim(x)[1]
  ydim = dim(x)[2]
  
  if(!missing(box) & missing(i) & missing(j)){
    i = ceiling(xdim/2)
    j = ceiling(ydim/2)
  }
  
  if(!missing(i)){
    express = as.character(substitute(i))
    
    if(express[1] == ':' & length(express) == 3L){
      if(grepl('end',substitute(i))[3]){
        start = express[2]
        end = xdim
        #i = eval(parse(text=paste0(start,':',end)))
        i = c(start, end)
      }
    }
  }
  
  if(!missing(j)){
    express = as.character(substitute(j))
    
    if(express[1] == ':' & length(express) == 3L){
      if(grepl('end',substitute(j))[3]){
        start = express[2]
        end = ydim
        #j = eval(parse(text=paste0(start,':',end)))
        j = c(start, end)
      }
    }
  }
  
  
  if(!missing(i)){
    if(length(i)==2 & missing(j)){
      if(i[2]-i[1] !=1){
        j = as.numeric(i[2])
        i = as.numeric(i[1])
      }
    }
  }
  
  if(missing(i)){i = c(1,xdim)}
  if(missing(j)){j = c(1,ydim)}
  
  # if(min(i) == 1L){
  #   if(max(i) == xdim){
  #     if(min(j) == 1L){
  #       if(max(j) == ydim){
  #         return(x) #do nothing!
  #       }
  #     }
  #   }
  # }
  
  #This is Rigo's version of the above (a bit more maintainable):
  arrays = list(i, j)
  upper_limits = list(xdim, ydim)
  if(all(mapply(.spans_up_to, arrays, upper_limits))){return(x)}
  
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
  
  safedim_i = .safedim(1L, xdim, min(i), max(i))
  safedim_j = .safedim(1L, ydim, min(j), max(j))
  
  tar = array(NA, dim=c(safedim_i$len_tar, safedim_j$len_tar))
  if(safedim_i$safe & safedim_j$safe){
    tar[safedim_i$tar,safedim_j$tar] = x$imDat[safedim_i$orig,safedim_j$orig]
  }
  
  if(header){
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

`[.Rfits_cube` = function(x, i, j, k, header=TRUE, collapse=TRUE){
  
  xdim = dim(x)[1]
  ydim = dim(x)[2]
  zdim = dim(x)[3]
  
  if(!missing(i)){
    express = as.character(substitute(i))
    
    if(express[1] == ':' & length(express) == 3L){
      if(grepl('end',substitute(i))[3]){
        start = express[2]
        end = xdim
        i = eval(parse(text=paste0(start,':',end)))
      }
    }
  }
  
  if(!missing(j)){
    express = as.character(substitute(j))
    
    if(express[1] == ':' & length(express) == 3L){
      if(grepl('end',substitute(j))[3]){
        start = express[2]
        end = ydim
        j = eval(parse(text=paste0(start,':',end)))
      }
    }
  }
  
  if(!missing(k)){
    express = as.character(substitute(k))
    
    if(express[1] == ':' & length(express) == 3L){
      if(grepl('end',substitute(k))[3]){
        start = express[2]
        end = zdim
        k = eval(parse(text=paste0(start,':',end)))
      }
    }
    k_prov = TRUE
  }else{
    k_prov = FALSE
  }
  
  if(missing(i)){i = c(1,xdim)}
  if(missing(j)){j = c(1,ydim)}
  if(missing(k)){k = c(1,zdim)}
  
  # if(min(i) == 1L){
  #   if(max(i) == xdim){
  #     if(min(j) == 1L){
  #       if(max(j) == ydim){
  #         if(min(k) == 1L){
  #           if(max(k) == zdim){
  #             return(x) #do nothing!
  #           }
  #         }
  #       }
  #     }
  #   }
  # }
  
  #This is Rigo's version of the above (a bit more maintainable):
  arrays = list(i, j, k)
  upper_limits = list(xdim, ydim, zdim)
  if(all(mapply(.spans_up_to, arrays, upper_limits))){return(x)}
  
  safedim_i = .safedim(1, xdim, min(i), max(i))
  safedim_j = .safedim(1, ydim, min(j), max(j))
  safedim_k = .safedim(1, zdim, min(k), max(k))
  
  tar = array(NA, dim=c(safedim_i$len_tar, safedim_j$len_tar, safedim_k$len_tar))
  if(safedim_i$safe & safedim_j$safe & safedim_k$safe){
    tar[safedim_i$tar,safedim_j$tar,safedim_k$tar] = x$imDat[safedim_i$orig,safedim_j$orig,safedim_k$orig]
  }
  
  if(header){
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
    
    if(safedim_k$len_tar == 1L & k_prov & collapse){
      dim(tar) = dim(tar)[1:2]
      
      x$keyvalues$NAXIS = 2L
      x$keyvalues$NAXIS3 = NULL
      x$keyvalues$CRPIX3 = NULL
      
      x$keycomments$NAXIS3 = NULL
      x$keycomments$CRPIX3 = NULL
      
      if(isTRUE(x$keyvalues$ZIMAGE)){
        x$keyvalues$ZNAXIS = 2L
        x$keyvalues$ZNAXIS3 = NULL
        x$keycomments$ZNAXIS3 = NULL
      }
      
      rm_key = c(
        grep('3_[1-3]', names(x$keyvalues)),
        grep('[1-3]_03', names(x$keyvalues)),
        grep('03_0[1-3]', names(x$keyvalues)),
        grep('0[1-3]_03', names(x$keyvalues)),
        
        grep('CTYPE3', names(x$keyvalues)),
        grep('CRVAL3', names(x$keyvalues)),
        grep('CDELT3', names(x$keyvalues)),
        grep('CUNIT3', names(x$keyvalues))
      )
      
      if(length(rm_key) > 0){
        x$keyvalues = x$keyvalues[-rm_key]
        x$keycomments = x$keycomments[-rm_key]
      }
      
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
    
    if(safedim_k$len_tar == 1L & k_prov & collapse){
      dim(tar) = dim(tar)[1:2]
    }
    
    return(tar)
  }
}

`[.Rfits_array` = function(x, i, j, k, m, header=TRUE, collapse=TRUE){
  
  xdim = dim(x)[1]
  ydim = dim(x)[2]
  zdim = dim(x)[3]
  tdim = dim(x)[4]
  
  if(!missing(i)){
    express = as.character(substitute(i))
    
    if(express[1] == ':' & length(express) == 3L){
      if(grepl('end',substitute(i))[3]){
        start = express[2]
        end = xdim
        i = eval(parse(text=paste0(start,':',end)))
      }
    }
  }
  
  if(!missing(j)){
    express = as.character(substitute(j))
    
    if(express[1] == ':' & length(express) == 3L){
      if(grepl('end',substitute(j))[3]){
        start = express[2]
        end = ydim
        j = eval(parse(text=paste0(start,':',end)))
      }
    }
  }
  
  if(!missing(k)){
    express = as.character(substitute(k))
    
    if(express[1] == ':' & length(express) == 3L){
      if(grepl('end',substitute(k))[3]){
        start = express[2]
        end = zdim
        k = eval(parse(text=paste0(start,':',end)))
      }
    }
    k_prov = TRUE
  }else{
    k_prov = FALSE
  }
  
  if(!missing(m)){
    express = as.character(substitute(m))
    
    if(express[1] == ':' & length(express) == 3L){
      if(grepl('end',substitute(m))[3]){
        start = express[2]
        end = tdim
        m = eval(parse(text=paste0(start,':',end)))
      }
    }
    m_prov = TRUE
  }else{
    m_prov = FALSE
  }
  
  if(missing(i)){i = c(1,xdim)}
  if(missing(j)){j = c(1,ydim)}
  if(missing(k)){k = c(1,zdim)}
  if(missing(m)){m = c(1,tdim)}
  
  # if(min(i) == 1L){
  #   if(max(i) == xdim){
  #     if(min(j) == 1L){
  #       if(max(j) == ydim){
  #         if(min(k) == 1L){
  #           if(max(k) == zdim){
  #             if(min(m) == 1L){
  #               if(max(m) == zdim){
  #                 return(x) #do nothing!
  #               }
  #             }
  #           }
  #         }
  #       }
  #     }
  #   }
  # }
  
  #This is Rigo's version of the above (a bit more maintainable):
  arrays = list(i, j, k, m)
  upper_limits = list(xdim, ydim, zdim, zdim)
  if(all(mapply(.spans_up_to, arrays, upper_limits)) & (upper_limits[[3]] > 1 | upper_limits[[4]] > 1)){
    return(x)
  }
  
  safedim_i = .safedim(1, xdim, min(i), max(i))
  safedim_j = .safedim(1, ydim, min(j), max(j))
  safedim_k = .safedim(1, zdim, min(k), max(k))
  safedim_m = .safedim(1, tdim, min(m), max(m))
  
  tar = array(NA, dim=c(safedim_i$len_tar, safedim_j$len_tar, safedim_k$len_tar, safedim_m$len_tar))
  if(safedim_i$safe & safedim_j$safe & safedim_k$safe & safedim_m$safe){
    tar[safedim_i$tar,safedim_j$tar,safedim_k$tar,safedim_m$tar] = x$imDat[safedim_i$orig,safedim_j$orig,safedim_k$orig,safedim_m$orig]
  }
  
  if(header){
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
    
    if(safedim_k$len_tar == 1L & safedim_m$len_tar == 1L & k_prov & m_prov & collapse){
      dim(tar) = dim(tar)[1:2]
      
      x$keyvalues$NAXIS = 2L
      
      x$keyvalues$NAXIS3 = NULL
      x$keyvalues$NAXIS4 = NULL
      x$keyvalues$CRPIX3 = NULL
      x$keyvalues$CRPIX4 = NULL
      
      x$keycomments$NAXIS3 = NULL
      x$keycomments$NAXIS4 = NULL
      x$keycomments$CRPIX3 = NULL
      x$keycomments$CRPIX4 = NULL
      
      if(isTRUE(x$keyvalues$ZIMAGE)){
        x$keyvalues$ZNAXIS = 2L
        x$keyvalues$ZNAXIS3 = NULL
        x$keyvalues$ZNAXIS4 = NULL
        
        x$keycomments$ZNAXIS3 = NULL
        x$keycomments$ZNAXIS4 = NULL
      }
      
      rm_key = c(
        grep('3_[1-4]', names(x$keyvalues)),
        grep('[1-4]_3', names(x$keyvalues)),
        grep('03_0[1-4]', names(x$keyvalues)),
        grep('0[1-4]_03', names(x$keyvalues)),
        
        grep('4_[1-4]', names(x$keyvalues)),
        grep('[1-4]_4', names(x$keyvalues)),
        grep('04_0[1-4]', names(x$keyvalues)),
        grep('0[1-4]_04', names(x$keyvalues)),
        
        grep('CTYPE3', names(x$keyvalues)),
        grep('CRVAL3', names(x$keyvalues)),
        grep('CDELT3', names(x$keyvalues)),
        grep('CUNIT3', names(x$keyvalues)),
        
        grep('CTYPE4', names(x$keyvalues)),
        grep('CRVAL4', names(x$keyvalues)),
        grep('CDELT4', names(x$keyvalues)),
        grep('CUNIT4', names(x$keyvalues))
      )
      
      if(length(rm_key) > 0){
        x$keyvalues = x$keyvalues[-rm_key]
        x$keycomments = x$keycomments[-rm_key]
      }
      
      class_out = "Rfits_image"
    }else if(safedim_m$len_tar == 1L & m_prov & collapse){
      dim(tar) = dim(tar)[1:3]
      
      x$keyvalues$NAXIS = 3L
      
      x$keyvalues$NAXIS4 = NULL
      x$keyvalues$CRPIX4 = NULL
      
      x$keycomments$NAXIS4 = NULL
      x$keycomments$CRPIX4 = NULL
      
      if(isTRUE(x$keyvalues$ZIMAGE)){
        x$keyvalues$ZNAXIS = 3L
        x$keyvalues$ZNAXIS4 = NULL
        
        x$keycomments$ZNAXIS4 = NULL
      }
      
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
    
    if(safedim_k$len_tar == 1L & safedim_m$len_tar == 1L & k_prov & m_prov & collapse){
      dim(tar) = dim(tar)[1:2]
    }else if(safedim_m$len_tar == 1L & k_prov & m_prov & collapse){
      dim(tar) = dim(tar)[1:3]
    }
    
    return(tar)
  }
}

`[.Rfits_pointer` = function(x, i, j, k, m, box=201, type='pix', header=x$header,
                             sparse=x$sparse, scale_sparse=x$scale_sparse, collapse=TRUE){
  
  xdim = dim(x)[1]
  ydim = dim(x)[2]
  zdim = dim(x)[3]
  tdim = dim(x)[4]
  
  if(!missing(box) & missing(i) & missing(j)){
    i = ceiling(xdim/2)
    j = ceiling(ydim/2)
  }
  
  if(!missing(i)){
    if(!is.matrix(i)){
      express = as.character(substitute(i))
      
      if(express[1] == ':' & length(express) == 3L){
        if(grepl('end',substitute(i))[3]){
          start = express[2]
          end = xdim
          i = eval(parse(text=paste0(start,':',end)))
        }
      }
    }
  }
  
  if(!missing(j)){
    express = as.character(substitute(j))
    
    if(express[1] == ':' & length(express) == 3L){
      if(grepl('end',substitute(j))[3]){
        start = express[2]
        end = ydim
        j = eval(parse(text=paste0(start,':',end)))
      }
    }
  }
  
  if(!missing(k)){
    express = as.character(substitute(k))
    
    if(express[1] == ':' & length(express) == 3L){
      if(grepl('end',substitute(k))[3]){
        start = express[2]
        end = zdim
        k = eval(parse(text=paste0(start,':',end)))
      }
    }
    k_prov = TRUE
  }else{
    k_prov = FALSE
  }
  
  if(!missing(m)){
    express = as.character(substitute(m))
    
    if(express[1] == ':' & length(express) == 3L){
      if(grepl('end',substitute(m))[3]){
        start = express[2]
        end = tdim
        m = eval(parse(text=paste0(start,':',end)))
      }
    }
    m_prov = TRUE
  }else{
    m_prov = FALSE
  }
  
  if(!missing(i)){
    if(is.vector(i)){
      if(length(i)==2 & missing(j)){
        if(i[2] - i[1] != 1){
          j = ceiling(i[2])
          i = ceiling(i[1])
        }
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
      if(is.vector(i)){
        if(length(i) == 1 & length(j) == 1){
          if(length(box) == 1){box = c(box,box)}
          i = ceiling(i + c(-(box[1]-1L)/2, (box[1]-1L)/2))
          j = ceiling(j + c(-(box[2]-1L)/2, (box[2]-1L)/2))
        }
      }
    }
  }
  
  if(!missing(i)){
    if(is.vector(i)){
      if(is.null(naxis1)){stop('NAXIS1 is NULL: specifying too many dimensions!')}
      xlo = ceiling(min(i))
      xhi = ceiling(max(i))
    }
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
  
  if(!missing(i)){
    if(is.matrix(i)){
      output = foreach(row = 1:dim(i)[1], .combine='c')%do%{
        if(dim(i)[2] == 1){
          return(Cfits_read_img_subset(filename=x$filename, ext=x$ext, datatype=datatype,
                                       fpixel0=i[row,1],
                                       lpixel0=i[row,1]))
        }else if(dim(i)[2] == 2){
          return(Cfits_read_img_subset(filename=x$filename, ext=x$ext, datatype=datatype,
                                       fpixel0=i[row,1], fpixel1=i[row,2],
                                       lpixel0=i[row,1], lpixel1=i[row,2]))
        }else if(dim(i)[2] == 3){
          return(Cfits_read_img_subset(filename=x$filename, ext=x$ext, datatype=datatype,
                                       fpixel0=i[row,1], fpixel1=i[row,2], fpixel2=i[row,3],
                                       lpixel0=i[row,1], lpixel1=i[row,2], lpixel2=i[row,3]))
        }else if(dim(i)[2] == 4){
          return(Cfits_read_img_subset(filename=x$filename, ext=x$ext, datatype=datatype,
                                       fpixel0=i[row,1], fpixel1=i[row,2], fpixel2=i[row,3], fpixel3=i[row,4],
                                       lpixel0=i[row,1], lpixel1=i[row,2], lpixel2=i[row,3], lpixel3=i[row,4]))
        }else{
          stop('Data type not recognised!')
        }
      }
      return(output)
    }
  }
  
  output = Rfits_read_image(filename=x$filename, ext=x$ext, header=header,
                          xlo=xlo, xhi=xhi, ylo=ylo, yhi=yhi, zlo=zlo, zhi=zhi,
                          tlo=tlo, thi=thi, zap=x$zap, zaptype=x$zaptype, sparse=sparse,
                          scale_sparse=scale_sparse, collapse=FALSE)
  
  if(collapse){
    if(length(dim(output)) == 3){
      if(dim(output)[3] == 1L & k_prov){
        output = output[,,1, collapse=TRUE]
      }
    }
    
    if(length(dim(output)) == 4){
      if(dim(output)[3] == 1L & dim(output)[4] == 1L & k_prov & m_prov){
        output = output[,,1,1, collapse=TRUE]
      }else if(dim(output)[4] == 1L & m_prov){
        output = output[,,,1, collapse=TRUE]
      }
    }
  }
  
  return(output)
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
    if(is.vector(i)){
      if(dim(value)[1] != diff(range(i)) + 1L){
        stop('dim x (1) of replacement does not match subset selection!')
      }
      Npix = diff(range(i)) + 1L
    }else if(is.matrix(i)){
      if(length(value) != dim(i)[1]){
        stop('Number of replacement locations does not match values!')
      }
      Npix = dim(i)[1]
    }
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
    if(length(dims) >= 2 & is.vector(i)){
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
    if(length(dims) >= 3 & is.vector(i)){
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
    if(length(dims) == 4 & is.vector(i)){
      m = 1:dims[4]
      Npix = Npix*(diff(range(m)) + 1L)
    }
  }
  
  if(length(value) != Npix){
    stop('Number of replacement pixels mismatches number of subset pixels!')
  }
  
  if(is.vector(i)){
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
  }else if(is.matrix(i)){
    
    datatype = 0
    
    if(is.logical(value[1])){
      datatype = 11
    }
    
    if(datatype == 0 & is.integer(value[1])){
      datatype = 31
    }else if(is.integer64(value[1])){
      datatype = 81
    }
    
    if(datatype==0 & is.numeric(value[1])){
      datatype = 82
    }
    
    for(row in 1:dim(i)[1]){
      if(x$type == 'vector'){
        #Rfits_write_pix(data=value[row], filename=x$filename, ext=x$ext, xlo=i[row,1])
        Cfits_write_img_subset(filename=x$filename, data=value[row], ext=x$ext, datatype=datatype, naxis=1,
                               fpixel0=i[row,1],
                               lpixel0=i[row,1])
      }else if(x$type == 'image'){
        #Rfits_write_pix(data=value[row], filename=x$filename, ext=x$ext, xlo=i[row,1], ylo=i[row,2])
        Cfits_write_img_subset(filename=x$filename, data=value[row], ext=x$ext, datatype=datatype, naxis=2,
                               fpixel0=i[row,1], fpixel1=i[row,2],
                               lpixel0=i[row,1], lpixel1=i[row,2])
      }else if(x$type == 'cube'){
        #Rfits_write_pix(data=value[row], filename=x$filename, ext=x$ext, xlo=i[row,1], ylo=i[row,2], zlo=i[row,3])
        Cfits_write_img_subset(filename=x$filename, data=value[row], ext=x$ext, datatype=datatype, naxis=3,
                               fpixel0=i[row,1], fpixel1=i[row,2], fpixel2=i[row,3],
                               lpixel0=i[row,1], lpixel1=i[row,2], lpixel2=i[row,3])
      }else if(x$type == 'array'){
        #Rfits_write_pix(data=value[row], filename=x$filename, ext=x$ext, xlo=i[row,1], ylo=i[row,2], zlo=i[row,3], tlo=i[row,4])
        Cfits_write_img_subset(filename=x$filename, data=value[row], ext=x$ext, datatype=datatype, naxis=3,
                               fpixel0=i[row,1], fpixel1=i[row,2], fpixel2=i[row,3], fpixel3=i[row,4],
                               lpixel0=i[row,1], lpixel1=i[row,2], lpixel2=i[row,3], lpixel3=i[row,4])
      }else{
        stop('Data type not recognised!')
      }
    }
  }
  
  return(Rfits_point(filename=x$filename, ext=x$ext, header=x$header, zap=x$zap, allow_write=x$allow_write))
}
