# Access modes when opening a FITS file:
#   
#   #define READONLY  0
#   #define READWRITE 1
#   
#   BITPIX data type code values for FITS images:
#   
#   #define BYTE_IMG      8  /*  8-bit unsigned integers */
#   #define SHORT_IMG    16  /* 16-bit   signed integers */
#   #define LONG_IMG     32  /* 32-bit   signed integers */
#   #define LONGLONG_IMG 64  /* 64-bit   signed integers */
#   #define FLOAT_IMG   -32  /* 32-bit single precision floating point */
#   #define DOUBLE_IMG  -64  /* 64-bit double precision floating point */
#   
#   The following 4 data type codes are also supported by CFITSIO:
#   #define SBYTE_IMG  10      /*  8-bit signed integers, equivalent to */
#   /*  BITPIX = 8, BSCALE = 1, BZERO = -128 */
#   #define USHORT_IMG  20     /* 16-bit unsigned integers, equivalent to */
#   /*  BITPIX = 16, BSCALE = 1, BZERO = 32768 */
#   #define ULONG_IMG   40     /* 32-bit unsigned integers, equivalent to */
#   /*  BITPIX = 32, BSCALE = 1, BZERO = 2147483648 */
#   #define ULONGLONG_IMG  80  /* 64-bit unsigned integers, equivalent to */
#   /*  BITPIX = 64, BSCALE = 1, BZERO = 9223372036854775808*/
#           
#   Codes for the data type of binary table columns and/or for the
#   data type of variables when reading or writing keywords or data:
#           
#   DATATYPE               TFORM CODE
#   #define TBIT          1  /*                            'X' */
#   #define TBYTE        11  /* 8-bit unsigned byte,       'B' */
#   #define TLOGICAL     14  /* logicals (int for keywords     */
#   /*  and char for table cols   'L' */
#   #define TSTRING      16  /* ASCII string,              'A' */
#   #define TSHORT       21  /* signed short,              'I' */
#   #define TLONG        41  /* signed long,                   */
#   #define TLONGLONG    81  /* 64-bit long signed integer 'K' */
#   #define TFLOAT       42  /* single precision float,    'E' */
#   #define TDOUBLE      82  /* double precision float,    'D' */
#   #define TCOMPLEX     83  /* complex (pair of floats)   'C' */
#   #define TDBLCOMPLEX 163  /* double complex (2 doubles) 'M' */
#           
#   The following data type codes are also supported by CFITSIO:
#   #define TINT         31  /* int                            */
#   #define TSBYTE       12  /* 8-bit signed byte,         'S' */
#   #define TUINT        30  /* unsigned int               'V' */
#   #define TUSHORT      20  /* unsigned short             'U' */
#   #define TULONG       40  /* unsigned long                  */
#   #define TULONGLONG   80  /* unsigned long long         'W' */
#           
#   The following data type code is only for use with fits\_get\_coltype
#   #define TINT32BIT    41  /* signed 32-bit int,         'J' */

Rfits_read_image=function(filename='temp.fits', ext=1, header=TRUE, xlo=NULL, xhi=NULL, ylo=NULL,
                          yhi=NULL, zlo=NULL, zhi=NULL, tlo=NULL, thi=NULL, remove_HIERARCH=FALSE,
                          force_logical=FALSE, bad=NULL, keypass=FALSE, zap=NULL, zaptype='full', sparse=1L,
                          scale_sparse=FALSE, collapse=FALSE, NAcheck=FALSE, nthreads=1){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  filename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  assertAccess(filename, access='r')
  filename = Rfits_gunzip(filename)
  if(is.character(ext)){ext = Rfits_extname_to_ext(filename, ext)}
  assertIntegerish(ext, len=1)
  assertFlag(header)
  assertIntegerish(xlo, null.ok=TRUE)
  assertIntegerish(xhi, null.ok=TRUE)
  assertIntegerish(ylo, null.ok=TRUE)
  assertIntegerish(yhi, null.ok=TRUE)
  assertIntegerish(zlo, null.ok=TRUE)
  assertIntegerish(zhi, null.ok=TRUE)
  assertIntegerish(tlo, null.ok=TRUE)
  assertIntegerish(thi, null.ok=TRUE)
  assertFlag(remove_HIERARCH)
  assertFlag(force_logical)
  checkNumeric(bad, null.ok=TRUE)
  assertFlag(keypass)
  assertCharacter(zap, null.ok=TRUE)
  assertIntegerish(sparse, null.ok=FALSE)
  
  subset=FALSE
  
  if(!is.null(xlo) | !is.null(xhi) | !is.null(ylo) | !is.null(yhi) | !is.null(zlo) | !is.null(zhi) | !is.null(tlo) | !is.null(thi) | sparse > 1 | header){
    
    hdr = Rfits_read_header(filename=filename, ext=ext, remove_HIERARCH=remove_HIERARCH, keypass=keypass, zap=zap, zaptype=zaptype)
    
    #Have to check for NAXIS1 directly, because I've come across images missing NAXIS :-(
    if(isTRUE(hdr$keyvalues$ZIMAGE)){
      naxis1 = hdr$keyvalues$ZNAXIS1
    }else{
      naxis1 = hdr$keyvalues$NAXIS1
    }
    
    if(ext==1 & is.null(naxis1)){
      message('Missing NAXIS1, usually this means the first image is after ext = 1')
      if(isTRUE(hdr$keyvalues$NAXIS == 0L) & isTRUE(hdr$keyvalues$EXTEND)){
        message('Trying ext = 2')
        ext = 2
        hdr = Rfits_read_header(filename=filename, ext=ext, remove_HIERARCH=remove_HIERARCH, keypass=keypass, zap=zap, zaptype=zaptype)
        if(isTRUE(hdr$keyvalues$NAXIS > 0L)){
          message('New NAXIS > 0, continuing with ext = 2')
        }else{
          stop('Well that did not work either...')
        }
      }
    }
    
    if(isTRUE(hdr$keyvalues$ZIMAGE)){
      naxis1 = hdr$keyvalues$ZNAXIS1
      naxis2 = hdr$keyvalues$ZNAXIS2
      naxis3 = hdr$keyvalues$ZNAXIS3
      naxis4 = hdr$keyvalues$ZNAXIS4
      datatype = hdr$keyvalues$ZBITPIX
    }else{
      naxis1 = hdr$keyvalues$NAXIS1
      naxis2 = hdr$keyvalues$NAXIS2
      naxis3 = hdr$keyvalues$NAXIS3
      naxis4 = hdr$keyvalues$NAXIS4
      datatype = hdr$keyvalues$BITPIX
    }
    
    Ndim = hdr$keyvalues$NAXIS
    
    if(is.null(Ndim)){
      if(!is.null(naxis1)){Ndim = 1L}
      if(!is.null(naxis2)){Ndim = 2L}
      if(!is.null(naxis3)){Ndim = 3L}
      if(!is.null(naxis4)){Ndim = 4L}
    }
    
    if(is.null(naxis1)){
      message('NAXIS1 is missing: this is pretty weird!')
      naxis1 = 1
    }
    if(is.null(naxis2)){
      naxis2 = 1
      if(Ndim == 2){
        message('NAXIS2 is missing, but NAXIS=2: this is pretty weird!')
      }
    }
    if(is.null(naxis3)){
      naxis3 = 1
      if(Ndim == 3){
        message('NAXIS3 is missing, but NAXIS=3: this is pretty weird!')
      }
    }
    if(is.null(naxis4)){
      naxis4 = 1
      if(Ndim == 4){
        message('NAXIS4 is missing, but NAXIS=4: this is pretty weird!')
      }
    }
    
    if(is.null(xlo)){xlo=1}else{subset=TRUE}
    if(is.null(xhi)){xhi=naxis1}else{subset=TRUE}
    if(is.null(ylo)){ylo=1}else{subset=TRUE}
    if(is.null(yhi)){yhi=naxis2}else{subset=TRUE}
    if(is.null(zlo)){zlo=1}else{subset=TRUE}
    if(is.null(zhi)){zhi=naxis3}else{subset=TRUE}
    if(is.null(tlo)){tlo=1}else{subset=TRUE}
    if(is.null(thi)){thi=naxis4}else{subset=TRUE}
    
    if(subset | sparse > 1){
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
      
      if(xhi<xlo){stop('xhi must be larger than xlo')}
      if(yhi<ylo){stop('yhi must be larger than ylo')}
      if(zhi<zlo){stop('zhi must be larger than zlo')}
      if(thi<tlo){stop('thi must be larger than tlo')}
    }
  }
  
  if(subset | sparse > 1){
    if(safex$safe & safey$safe & safez$safe & safet$safe){
      try({
        temp_image = Cfits_read_img_subset(filename=filename, ext=ext, datatype=datatype,
                                           fpixel0=xlo, fpixel1=ylo, fpixel2=zlo, fpixel3=tlo,
                                           lpixel0=xhi, lpixel1=yhi, lpixel2=zhi, lpixel3=thi,
                                           sparse=sparse, nthreads=nthreads)
        
        check64 = inherits(temp_image, 'integer64')
        
        if(naxis2 == 1 & naxis3 == 1 & naxis4 == 1){
          if(scale_sparse){
            temp_image = temp_image*sparse
          }
        }
        
        if(naxis2 > 1 & naxis3 == 1 & naxis4 == 1){
          if(scale_sparse){
            temp_image = temp_image*sparse^2
          }
          #temp_image = matrix(temp_image, floor((xhi - xlo)/sparse) + 1L, floor((yhi - ylo)/sparse) + 1L)
          dim(temp_image) = c(floor((xhi - xlo)/sparse) + 1L, floor((yhi - ylo)/sparse) + 1L)
        }
        
        if(naxis3 > 1 & naxis4 == 1){
          if(scale_sparse){
            temp_image = temp_image*sparse^3
          }
          #temp_image = array(temp_image, dim=c(floor((xhi - xlo)/sparse) + 1L, floor((yhi - ylo)/sparse) + 1L, floor((zhi - zlo)/sparse) + 1L))
          dim(temp_image) = c(floor((xhi - xlo)/sparse) + 1L, floor((yhi - ylo)/sparse) + 1L, floor((zhi - zlo)/sparse) + 1L)
        }
        if(naxis4 > 1){
          if(scale_sparse){
            temp_image = temp_image*sparse^4
          }
          #temp_image = array(temp_image, dim=c(floor((xhi - xlo)/sparse) + 1L, floor((yhi - ylo)/sparse) + 1L, floor((zhi - zlo)/sparse) + 1L, floor((thi - tlo)/sparse) + 1L))
          dim(temp_image) = c(floor((xhi - xlo)/sparse) + 1L, floor((yhi - ylo)/sparse) + 1L, floor((zhi - zlo)/sparse) + 1L, floor((thi - tlo)/sparse) + 1L)
        }
        if(check64){
          attributes(temp_image)$class = "integer64"
        }
      })
      if(sparse > 1){
        if(header){
          if(Ndim == 1){
            hdr$keyvalues$NAXIS1 = length(temp_image)
          }else{
            hdr$keyvalues$NAXIS1 = dim(temp_image)[1]
          }
          # hdr$keyvalues$CRPIX1 = (hdr$keyvalues$CRPIX1 - safex$lo_tar + 1L)/sparse + (sparse - 1L)/sparse
          hdr$keyvalues$CRPIX1 = (hdr$keyvalues$CRPIX1 - xlo)/sparse + 1
          
          if(Ndim >= 2){
            hdr$keyvalues$NAXIS2 = dim(temp_image)[2]
            hdr$keyvalues$CRPIX2 = (hdr$keyvalues$CRPIX2 - ylo)/sparse + 1
          }
          
          if(Ndim >= 3){
            hdr$keyvalues$NAXIS3 = dim(temp_image)[3]
            hdr$keyvalues$CRPIX3 = (hdr$keyvalues$CRPIX3 - zlo)/sparse + 1
          }
          
          if(Ndim >= 4){
            hdr$keyvalues$NAXIS4 = dim(temp_image)[4]
            hdr$keyvalues$CRPIX4 = (hdr$keyvalues$CRPIX4 - tlo)/sparse + 1
          }
          
          hdr$keyvalues$CD1_1 = hdr$keyvalues$CD1_1 * sparse
          hdr$keyvalues$CD1_2 = hdr$keyvalues$CD1_2 * sparse
          hdr$keyvalues$CD2_1 = hdr$keyvalues$CD2_1 * sparse
          hdr$keyvalues$CD2_2 = hdr$keyvalues$CD2_2 * sparse
          
          output = Rfits_create_image(temp_image, keyvalues = hdr$keyvalues) #scale by sparse^2 to get fluxes roughly right
          
          #The below isn't right- need to cutout based on sparse rescale limits
          
          if(Ndim == 1){
            xlo = ceiling((1 - safex$tar[1])/sparse) + 1L
            xhi = xlo + ceiling((safex$hi_tar - safex$lo_tar)/sparse) - 1L
            output = output[c(xlo, xhi)]
          }else if(Ndim >= 2){
            xlo = ceiling((1 - safex$tar[1])/sparse) + 1L
            xhi = xlo + ceiling((safex$hi_tar - safex$lo_tar)/sparse) - 1L
            ylo = ceiling((1 - safey$tar[1])/sparse) + 1L
            yhi = ylo + ceiling((safey$hi_tar - safey$lo_tar)/sparse) - 1L
            output = output[c(xlo, xhi), c(ylo, yhi)]
          }else if(Ndim >= 3){
            xlo = ceiling((1 - safex$tar[1])/sparse) + 1L
            xhi = xlo + ceiling((safex$hi_tar - safex$lo_tar)/sparse) - 1L
            ylo = ceiling((1 - safey$tar[1])/sparse) + 1L
            yhi = ylo + ceiling((safey$hi_tar - safey$lo_tar)/sparse) - 1L
            zlo = ceiling((1 - safez$tar[1])/sparse) + 1L
            zhi = zlo + ceiling((safez$hi_tar - safez$lo_tar)/sparse) - 1L
            output = output[c(xlo, xhi), c(ylo, yhi), c(zlo, zhi)]
          }else if(Ndim >= 4){
            xlo = ceiling((1 - safex$tar[1])/sparse) + 1L
            xhi = xlo + ceiling((safex$hi_tar - safex$lo_tar)/sparse) - 1L
            ylo = ceiling((1 - safey$tar[1])/sparse) + 1L
            yhi = ylo + ceiling((safey$hi_tar - safey$lo_tar)/sparse) - 1L
            zlo = ceiling((1 - safez$tar[1])/sparse) + 1L
            zhi = zlo + ceiling((safez$hi_tar - safez$lo_tar)/sparse) - 1L
            tlo = ceiling((1 - safet$tar[1])/sparse) + 1L
            thi = ylo + ceiling((safet$hi_tar - safet$lo_tar)/sparse) - 1L
            output = output[c(xlo, xhi), c(ylo, yhi), c(zlo, zhi), c(tlo, thi)]
          }
          
          return(output)
        }else{
          return(temp_image)
        }
      }
      if(!is.numeric(temp_image)){
        message(paste0('Image read failed for extension '), ext, '. Replacing values with NA!')
        temp_image = NA
      }
    }
    if(Ndim==1){
      if(safex$len_tar == length(temp_image)){
        image = temp_image
      }else{
        image = rep(NA, safex$len_tar)
        if(safex$safe){
          image[safex$tar] = temp_image
        }
      }
    }
    
    if(Ndim==2){
      if(safex$len_tar == dim(temp_image)[1] & safey$len_tar == dim(temp_image)[2]){
        image = temp_image
      }else{
        image = array(NA, c(safex$len_tar, safey$len_tar))
        if(safex$safe & safey$safe){
          image[safex$tar,safey$tar] = temp_image
        }
      }
    }
    
    if(Ndim==3){
      if(safex$len_tar == dim(temp_image)[1] & safey$len_tar == dim(temp_image)[2] & safez$len_tar == dim(temp_image)[3]){
        image = temp_image
      }else{
        image = array(NA, c(safex$len_tar, safey$len_tar, safez$len_tar))
        if(safex$safe & safey$safe & safez$safe){
          image[safex$tar,safey$tar,safez$tar] = temp_image
        }
      }
    }
    
    if(Ndim==4){
      if(safex$len_tar == dim(temp_image)[1] & safey$len_tar == dim(temp_image)[2] & safez$len_tar == dim(temp_image)[3] & safet$len_tar == dim(temp_image)[4]){
        image = temp_image
      }else{
        image = array(NA, c(safex$len_tar, safey$len_tar, safez$len_tar, safet$len_tar))
        if(safex$safe & safey$safe & safez$safe & safet$safe){
          image[safex$tar,safey$tar,safez$tar,safet$tar] = temp_image
        }
      }
    }
    
    naxis1 = safex$len_tar
    naxis2 = safey$len_tar
    naxis3 = safez$len_tar
    naxis4 = safet$len_tar
  }else{
    # Leave the naxis reads- this is the only way to know the naxis if we aren't given a header!
    naxis = try(Rfits_read_key(filename=filename, keyname='NAXIS', ext=ext), silent=TRUE)
    if(ext == 1 & isFALSE(naxis > 0)){
      message('Missing NAXIS=0, usually this means the first image is after ext = 1')
      extend = try(Rfits_read_key(filename=filename, keyname='EXTEND', ext=ext), silent=TRUE)
      if(isTRUE(extend)){
        message('Trying ext = 2')
        ext = 2
        naxis = try(Rfits_read_key(filename=filename, keyname='NAXIS', ext=ext), silent=TRUE)
        if(isTRUE(naxis > 0L)){
          message('New NAXIS > 0, continuing with ext = 2')
        }else{
          stop('Well that did not work either...')
        }
      }
    }
    
    zimage = try(Rfits_read_key(filename=filename, keyname='ZIMAGE', ext=ext), silent=TRUE)
    if(isTRUE(zimage)){
      naxis1 = try(Rfits_read_key(filename=filename, keyname='ZNAXIS1', ext=ext), silent=TRUE)
      naxis2 = try(Rfits_read_key(filename=filename, keyname='ZNAXIS2', ext=ext), silent=TRUE)
      naxis3 = try(Rfits_read_key(filename=filename, keyname='ZNAXIS3', ext=ext), silent=TRUE)
      naxis4 = try(Rfits_read_key(filename=filename, keyname='ZNAXIS4', ext=ext), silent=TRUE)
      datatype = Rfits_read_key(filename=filename, keyname='ZBITPIX', ext=ext)
    }else{
      naxis1 = try(Rfits_read_key(filename=filename, keyname='NAXIS1', ext=ext), silent=TRUE)
      naxis2 = try(Rfits_read_key(filename=filename, keyname='NAXIS2', ext=ext), silent=TRUE)
      naxis3 = try(Rfits_read_key(filename=filename, keyname='NAXIS3', ext=ext), silent=TRUE)
      naxis4 = try(Rfits_read_key(filename=filename, keyname='NAXIS4', ext=ext), silent=TRUE)
      datatype = Rfits_read_key(filename=filename, keyname='BITPIX', ext=ext)
    }
    if(!is.numeric(naxis1)){
      message('NAXIS1 is missing- this is pretty weird!')
      naxis1 = 1
    }else{
      Ndim = 1L
    }
    if(!is.numeric(naxis2)){
      naxis2 = 1
    }else{
      Ndim = 2L
    }
    if(!is.numeric(naxis3)){
      naxis3 = 1
    }else{
      Ndim = 3L
    }
    if(!is.numeric(naxis4)){
      naxis4 = 1
    }else{
      Ndim = 4L
    }
    try({
      image = Cfits_read_img(filename=filename, ext=ext, datatype=datatype,
                             naxis1=naxis1, naxis2=naxis2, naxis3=naxis3, naxis4=naxis4, nthreads=nthreads)
    })
    if(!is.numeric(image)){
      message(paste0('Image read failed for extension '), ext, '. Replacing values with NA!')
      image = NA
    }
  }
  
  if(is.numeric(image) & (datatype == -16 | datatype == -32) & NAcheck){
    if(anyNA(image)){
      image[is.nan(image)] = NA
    }
  }
  
  if(force_logical & is.integer(image)){
    image = as.logical(image)
  }
  
  if(!is.null(bad)){
    if(any(!is.finite(image))){
      image[!is.finite(image)] = bad
    }
  }
  
  # if(naxis1 == 1 & naxis2 == 1 & naxis3 == 1 & naxis4 == 1){
  #   image = as.vector(image)
  # }else if(naxis2 == 1 & naxis3 == 1 & naxis4 == 1){
  #   image = as.vector(image)
  # }else if(naxis2 > 1 & naxis3 == 1 & naxis4 == 1){
  #   dim(image) =c(naxis1, naxis2)
  # }else if(naxis3 > 1 & naxis4 == 1){
  #   dim(image) =c(naxis1, naxis2, naxis3)
  # }else if(naxis4 > 1){
  #   dim(image) =c(naxis1, naxis2, naxis3, naxis4)
  # }else{
  #   stop('Cannot determine the dimensions of the image!')
  # }
  if(Ndim == 1){
    #image = as.vector(image)
    #do nothing
  }else if(Ndim == 2){
    #dim(image) =c(naxis1, naxis2)
    #attr(image, 'dim') = c(naxis1, naxis2)
    #for reasons not obvious to me, the structure function is the only one that avoids a copy
    image = structure(image, .Dim=c(naxis1, naxis2))
    #attributes(image)$dim = c(naxis1, naxis2)
    #attributes(temp$imDat) = list(dim=c(naxis1, naxis2))
  }else if(Ndim == 3){
    #dim(image) =c(naxis1, naxis2, naxis3)
    #attr(image, 'dim') = c(naxis1, naxis2, naxis3)
    image = structure(image, .Dim=c(naxis1, naxis2, naxis3))
  }else if(Ndim == 4){
    #dim(image) =c(naxis1, naxis2, naxis3, naxis4)
    #attr(image, 'dim') = c(naxis1, naxis2, naxis3, naxis4)
    image = structure(image, .Dim=c(naxis1, naxis2, naxis3, naxis4))
  }else{
    stop('Cannot determine the dimensions of the image, or more than 4!')
  }
  
  if(header){
    if(subset){
      if(!isTRUE(hdr$keyvalues$ZIMAGE)){
        #Dim 1
        hdr$keyvalues$NAXIS1 = naxis1
        hdr$keycomments$NAXIS1 = paste(hdr$keycomments$NAXIS1, 'SUBMOD')
        if(!is.null(hdr$keyvalues$CRPIX1)){
          hdr$keyvalues$CRPIX1 = hdr$keyvalues$CRPIX1 - safex$lo_tar + 1L
          hdr$keycomments$CRPIX1 = paste(hdr$keycomments$CRPIX1, 'SUBMOD')
        }
        #Dim 2
        if(Ndim >= 2){
          hdr$keyvalues$NAXIS2 = naxis2
          hdr$keycomments$NAXIS2 = paste(hdr$keycomments$NAXIS2, 'SUBMOD')
          if(!is.null(hdr$keyvalues$CRPIX2)){
            hdr$keyvalues$CRPIX2 = hdr$keyvalues$CRPIX2 - safey$lo_tar + 1L
            hdr$keycomments$CRPIX2 = paste(hdr$keycomments$CRPIX2, 'SUBMOD')
          }
        }
        #Dim 3
        if(Ndim >= 3){
          hdr$keyvalues$NAXIS3 = naxis3
          hdr$keycomments$NAXIS3 = paste(hdr$keycomments$NAXIS3, 'SUBMOD')
          if(!is.null(hdr$keyvalues$CRPIX3)){
            hdr$keyvalues$CRPIX3 = hdr$keyvalues$CRPIX3 - safez$lo_tar + 1L
            hdr$keycomments$CRPIX3 = paste(hdr$keycomments$CRPIX3, 'SUBMOD')
          }
        }
        #Dim 4
        if(Ndim >= 4){
          hdr$keyvalues$NAXIS4 = naxis4
          hdr$keycomments$NAXIS4 = paste(hdr$keycomments$NAXIS4, 'SUBMOD')
          if(!is.null(hdr$keyvalues$CRPIX4)){
            hdr$keyvalues$CRPIX4 = hdr$keyvalues$CRPIX4 - safet$lo_tar + 1L
            hdr$keycomments$CRPIX4 = paste(hdr$keycomments$CRPIX4, 'SUBMOD')
          }
        }
        hdr$hdr = Rfits_keyvalues_to_hdr(hdr$keyvalues)
        hdr$header = Rfits_keyvalues_to_header(hdr$keyvalues, hdr$keycomments, hdr$comment, hdr$history)
        hdr$raw = Rfits_header_to_raw(hdr$header)
      }else{
        #Dim 1
        hdr$keyvalues$ZNAXIS1 = naxis1
        hdr$keycomments$ZNAXIS1 = paste(hdr$keycomments$ZNAXIS1, 'SUBMOD')
        if(!is.null(hdr$keyvalues$CRPIX1)){
          hdr$keyvalues$CRPIX1 = hdr$keyvalues$CRPIX1 - safex$lo_tar + 1L
          hdr$keycomments$CRPIX1 = paste(hdr$keycomments$CRPIX1, 'SUBMOD')
        }
        #Dim 2
        if(Ndim >= 2){
          hdr$keyvalues$ZNAXIS2 = naxis2
          hdr$keycomments$ZNAXIS2 = paste(hdr$keycomments$NAXIS2, 'SUBMOD')
          if(!is.null(hdr$keyvalues$CRPIX2)){
            hdr$keyvalues$CRPIX2 = hdr$keyvalues$CRPIX2 - safey$lo_tar + 1L
            hdr$keycomments$CRPIX2 = paste(hdr$keycomments$CRPIX2, 'SUBMOD')
          }
        }
        #Dim 3
        if(Ndim >= 3){
          hdr$keyvalues$ZNAXIS3 = naxis3
          hdr$keycomments$ZNAXIS3 = paste(hdr$keycomments$NAXIS3, 'SUBMOD')
          if(!is.null(hdr$keyvalues$CRPIX3)){
            hdr$keyvalues$CRPIX3 = hdr$keyvalues$CRPIX3 - safez$lo_tar + 1L
            hdr$keycomments$CRPIX3 = paste(hdr$keycomments$CRPIX3, 'SUBMOD')
          }
        }
        #Dim 4
        if(Ndim >= 4){
          hdr$keyvalues$ZNAXIS4 = naxis4
          hdr$keycomments$ZNAXIS4 = paste(hdr$keycomments$NAXIS4, 'SUBMOD')
          if(!is.null(hdr$keyvalues$CRPIX4)){
            hdr$keyvalues$CRPIX4 = hdr$keyvalues$CRPIX4 - safet$lo_tar + 1L
            hdr$keycomments$CRPIX4 = paste(hdr$keycomments$CRPIX4, 'SUBMOD')
          }
        }
        hdr$hdr = Rfits_keyvalues_to_hdr(hdr$keyvalues)
        hdr$header = Rfits_keyvalues_to_header(hdr$keyvalues, hdr$keycomments, hdr$comment, hdr$history)
        hdr$raw = Rfits_header_to_raw(hdr$header)
      }
    }
    
    WCSfound = grep('CRVAL1', hdr$keynames, value=TRUE)
    
    if(length(WCSfound) == 0){
      WCSref = 'None'
    }else if(length(WCSfound) == 1){
      WCSref = 'NULL'
    }else{
      WCSref = {}
      for(i in 1:length(WCSfound)){
        WCScurrent = strsplit(WCSfound[i], 'CRVAL1', fixed=TRUE)[[1]]
        if(length(WCScurrent) == 1){
          WCSref = c(WCSref, 'Null')
        }else{
          WCSref = c(WCSref, WCScurrent[2])
        }
      }
    }
    
    output = list(imDat = image,
                  keyvalues = hdr$keyvalues,
                  keycomments = hdr$keycomments,
                  keynames = hdr$keynames,
                  header = hdr$header,
                  hdr = hdr$hdr,
                  raw = hdr$raw,
                  comment = hdr$comment,
                  history = hdr$history,
                  nkey = hdr$nkey,
                  filename = filename,
                  ext = ext,
                  extname = hdr$keyvalues$EXTNAME,
                  WCSref = WCSref
                  )
      
    if(Ndim == 1){
      class(output) = c('Rfits_vector', 'list')
    }else if(Ndim == 2){
      class(output) = c('Rfits_image', 'list')
    }else if(Ndim == 3){
      class(output) = c('Rfits_cube', 'list')
    }else if(Ndim == 4){
      class(output) = c('Rfits_array', 'list')
    }
  }else{
    output = image
  }
  
  if(collapse){
    if(length(dim(output)) == 3){
      if(dim(output)[3] == 1L){
        output = output[,,1, collapse=TRUE]
      }
    }
    
    if(length(dim(output)) == 4){
      if(dim(output)[3] == 1L & dim(output)[4] == 1L){
        output = output[,,1,1, collapse=TRUE]
      }else if(dim(output)[4] == 1L){
        output = output[,,,1, collapse=TRUE]
      }
    }
  }
  
  return(invisible(output))
}

Rfits_read_vector = Rfits_read_image

Rfits_read_cube = Rfits_read_image

Rfits_read_array = Rfits_read_image
  
Rfits_write_image=function(data, filename='temp.fits', ext=1, keyvalues, keycomments,
                           keynames, comment, history, numeric='single',
                           integer='long', create_ext=TRUE, create_file=TRUE,
                           overwrite_file=TRUE, bzero=0, bscale=1, compress=FALSE, bad_compress=0L){
  assertFlag(create_ext)
  assertFlag(create_file)
  assertFlag(overwrite_file)
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  if(grepl('[compress', filename, fixed=TRUE)){
    compress = TRUE
  }
  justfilename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  if(create_file){
    assertPathForOutput(justfilename, overwrite=TRUE)
  }else{
    assertFileExists(justfilename)
    assertAccess(justfilename, access='w')
  }
  if(testFileExists(justfilename) & overwrite_file & create_file){
    file.remove(justfilename)
  }
  if(inherits(data, what=c('Rfits_vector', 'Rfits_image', 'Rfits_cube', 'Rfits_array'))){
    if(missing(keyvalues)){keyvalues=data$keyvalues}
    if(missing(keycomments)){keycomments=data$keycomments}
    if(missing(keynames)){keynames=data$keynames}
    if(missing(comment)){comment=data$comment}
    if(missing(history)){history=data$history}
    data = data$imDat
  }
  if(is.vector(data)){
    assertVector(data)
  }else{
    assertArray(data)
  }
  if(!missing(keyvalues)){keyvalues=as.list(keyvalues)}
  if(!missing(keycomments)){keycomments=as.list(keycomments)}
  if(!missing(keynames)){keynames=as.character(keynames)}
  if(!missing(comment)){comment=as.character(comment)}
  if(!missing(history)){history=as.character(history)}
  if(is.numeric(numeric)){numeric=as.character(numeric)}
  if(is.numeric(integer)){integer=as.character(integer)}
  
  assertIntegerish(ext, len=1)
  assertCharacter(numeric, len=1)
  assertCharacter(integer, len=1)
  assertNumeric(bzero)
  assertNumeric(bscale)
  assertNumeric(bad_compress)
  
  naxes = dim(data)
  if(is.null(naxes)){naxes = length(data)}
  naxis = length(naxes)
  if(naxis == 1){
    naxes = c(naxes,1,1,1)
  }
  if(naxis == 2){
    naxes = c(naxes,1,1)
  }
  if(naxis == 3){
    naxes = c(naxes,1)
  }
  
  if(!isFALSE(compress) & naxis > 1){
    if(isTRUE(compress)){
      filename = paste0(justfilename,'[compress]')
    }else if(is.character(compress)){
      filename = paste0(justfilename,'[compress ',compress,']')
    }
    compress = TRUE
  }else{
    compress = FALSE
  }
  
  if(compress & any(!is.finite(data))){
    data[!is.finite(data)] = bad_compress
  }
  
  bitpix = 0
  
  int_change = FALSE
  
  if(integer=='byte' | integer=='8'){
    if(max(data,na.rm=TRUE) >= 2^7 | min(data,na.rm=TRUE) <= -2^7){
      integer = 'short'
      int_change = TRUE
    }
  }
  
  if(integer=='short' | integer=='16'){
    if(max(data,na.rm=TRUE) >= 2^15 | min(data,na.rm=TRUE) <= -2^15){
      integer = 'long'
      int_change = TRUE
    }
  }
  
  if(integer=='long' | integer=='int' | integer=='32'){
    if(max(data,na.rm=TRUE) >= 2^31 | min(data,na.rm=TRUE) <= -2^31){
      integer = 'longlong'
      int_change = TRUE
    }
  }
  
  if(int_change & (is.integer(data) | is.integer64(data))){
    message('Converted integer type to ',integer,' since data range is too large!')
  }
  
  if(is.logical(data[1])){
    bitpix = 8
    datatype = 11
  }
  
  if(bitpix == 0 & is.integer(data[1])){
    if(integer=='byte' | integer=='8'){
      bitpix = 8
      datatype = 11
    }else if(integer=='short' | integer=='16'){
      bitpix = 16
      datatype = 21
    }else if(integer=='long' | integer=='int' | integer=='32'){
      bitpix = 32
      datatype = 31
      if(!missing(keyvalues)){
        if(!is.null(keyvalues$BZERO)){
          if(keyvalues$BZERO + max(data, na.rm=T) > 2^31){
            keyvalues$BZERO = 0
            message('Changing BZERO to 0 to prevent integer overflow!')
          }
        }
      }
    }else if(integer=='longlong' | integer=='64'){
      bitpix = 64
      datatype = 81
      if(is.integer(data)){
        data = as.integer64(data)
      }
    }else{
      stop('integer type must be short/int/16 (16 bit) or long/32 (32 bit)')
    }
  }else if(is.integer64(data[1])){
    bitpix = 64
    datatype = 81
  }
  
  if(bitpix==0 & is.numeric(data[1])){
    if(numeric=='single' | numeric=='float' | numeric=='32'){
      bitpix = -32
      datatype = 42
    }else if (numeric=='double' | numeric=='64'){
      bitpix = -64
      datatype = 82
    }else{
      stop('numeric type must be single/float/32 or double/64')
    }
  }
  
  if(!missing(keyvalues) & !missing(bzero)){keyvalues$BZERO = bzero}
  if(!missing(keyvalues) & !missing(bscale)){keyvalues$BSCALE = bscale}
  
  Cfits_create_image(filename=filename, naxis=naxis, naxis1=naxes[1], naxis2=naxes[2], naxis3=naxes[3],
                    naxis4=naxes[4], ext=ext, create_ext=create_ext, create_file=create_file, bitpix=bitpix)
  
  ext = Rfits_nhdu(filename=filename)
  
  if(!missing(keyvalues)){
    if(is.null(keyvalues$BITPIX)){
      keynames = c(keynames, 'BITPIX')
      keycomments$BITPIX = 'number of bits per data pixel'
    }
    keyvalues$BITPIX = bitpix
    
    if(!missing(comment)){
      checkAA=grep("FITS \\(Flexible Image Transport System\\) format is defined in 'Astronomy",comment)
      if(length(checkAA)>0){comment = comment[-checkAA]}
      checkAA=grep("and Astrophysics', volume 376, page 359; bibcode: 2001A&A...376..359H",comment)
      if(length(checkAA)>0){comment = comment[-checkAA]}
    }
    Rfits_write_header(filename=justfilename, keyvalues=keyvalues,
                       keycomments=keycomments, keynames=keynames,
                       comment=comment, history=history, ext=ext)
  }
  Cfits_write_pix(filename=filename, data=data, ext=ext, datatype=datatype,
                  naxis=naxis, naxis1=naxes[1], naxis2=naxes[2], naxis3=naxes[3], naxis4=naxes[4])
  return(invisible(list(filename=filename, ext=ext, naxis=naxis, naxes=c(naxes[1], naxes[2], naxes[3], naxes[4])[1:naxis])))
}

Rfits_blank_image = function(filename, ext=1, create_ext=TRUE, create_file=TRUE, overwrite_file=TRUE,
                             bitpix=-32, naxis=2, naxis1=100, naxis2=100, naxis3=1, naxis4=1){
  assertFlag(create_ext)
  assertFlag(create_file)
  assertFlag(overwrite_file)
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  justfilename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  if(create_file){
    assertPathForOutput(justfilename, overwrite=TRUE)
  }else{
    assertFileExists(justfilename)
    assertAccess(justfilename, access='w')
  }
  if(testFileExists(justfilename) & overwrite_file & create_file){
    file.remove(justfilename)
  }
  assertIntegerish(ext, len=1)
  assertIntegerish(naxis, len=1)
  assertIntegerish(naxis1, len=1)
  assertIntegerish(naxis2, len=1)
  assertIntegerish(naxis3, len=1)
  assertIntegerish(naxis4, len=1)
  
  Cfits_create_image(filename=filename, naxis=naxis, naxis1=naxis1, naxis2=naxis2, naxis3=naxis3,
                     naxis4=naxis4, ext=ext, create_ext=create_ext, create_file=create_file, bitpix=bitpix)
  return(invisible(list(filename=filename, ext=ext, naxis=naxis, naxes=c(naxis1, naxis2, naxis3, naxis4)[1:naxis])))
}

Rfits_write_pix = function(data, filename, ext=1, xlo=1L, ylo=1L, zlo=1L, tlo=1L){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  assertFileExists(filename)
  assertAccess(filename, access='w')
  
  if(inherits(data, what=c('Rfits_vector', 'Rfits_image', 'Rfits_cube', 'Rfits_array'))){
    data = data$imDat
  }
  if(is.vector(data)){
    assertVector(data)
  }else{
    assertArray(data)
  }
  
  dim_data = dim(data)
  
  if(is.null(dim_data)){dim_data = length(data)}
  naxis = length(dim_data)
  if(naxis >= 1){
    naxes = c(dim_data[1],1,1,1)
    xhi = xlo + naxes[1] - 1
  }
  if(naxis >= 2){
    naxes = c(dim_data[1:2],1,1)
    yhi = ylo + naxes[2] - 1
  }else{
    yhi = 1
  }
  if(naxis >= 3){
    naxes = c(dim_data[1:3],1)
    zhi = zlo + naxes[3] - 1
  }else{
    zhi = 1
  }
  if(naxis == 4){
    naxes = dim_data
    thi = tlo + naxes[4] - 1
  }else{
    thi = 1
  }
  
  temp_header = Rfits_read_header(filename=filename, ext=ext)
  dim_fits = dim(temp_header)
  
  if(naxis == 1){
    safe_x = .safedim(1L, dim_fits[1], xlo, xhi)
    
    if(safe_x$safe){
      data = data[safe_x$tar]
      xlo = min(safe_x$orig)
      xhi = max(safe_x$orig)
    }else{
      message('Doing nothing: data is entirely outside of target FITS!')
      message('FITS: ',dim_fits)
      return(NULL)
    }
  }
  if(naxis == 2){
    safe_x = .safedim(1L, dim_fits[1], xlo, xhi)
    safe_y = .safedim(1L, dim_fits[2], ylo, yhi)
    
    if(safe_x$safe & safe_y$safe){
      data = data[safe_x$tar, safe_y$tar, drop=FALSE]
      xlo = min(safe_x$orig)
      xhi = max(safe_x$orig)
      ylo = min(safe_y$orig)
      yhi = max(safe_y$orig)
    }else{
      message('Doing nothing: data is entirely outside of target FITS!')
      message('FITS: ', dim_fits[1], ' ',  dim_fits[2])
      return(NULL)
    }
  }
  if(naxis == 3){
    safe_x = .safedim(1L, dim_fits[1], xlo, xhi)
    safe_y = .safedim(1L, dim_fits[2], ylo, yhi)
    safe_z = .safedim(1L, dim_fits[3], zlo, zhi)
    
    if(safe_x$safe & safe_y$safe & safe_z$safe){
      data = data[safe_x$tar, safe_y$tar, safe_z$tar, drop=FALSE]
      xlo = min(safe_x$orig)
      xhi = max(safe_x$orig)
      ylo = min(safe_y$orig)
      yhi = max(safe_y$orig)
      zlo = min(safe_z$orig)
      zhi = max(safe_z$orig)
    }else{
      message('Doing nothing: data is entirely outside of target FITS!')
      message('FITS: ', dim_fits[1], ' ',  dim_fits[2], ' ',  dim_fits[3])
      return(NULL)
    }
  }
  if(naxis == 4){
    safe_x = .safedim(1L, dim_fits[1], xlo, xhi)
    safe_y = .safedim(1L, dim_fits[2], ylo, yhi)
    safe_z = .safedim(1L, dim_fits[3], zlo, zhi)
    safe_t = .safedim(1L, dim_fits[4], tlo, thi)
    
    if(safe_x$safe & safe_y$safe & safe_z$safe & safe_t$safe){
      data = data[safe_x$tar, safe_y$tar, safe_z$tar, safe_t$tar, drop=FALSE]
      xlo = min(safe_x$orig)
      xhi = max(safe_x$orig)
      ylo = min(safe_y$orig)
      yhi = max(safe_y$orig)
      zlo = min(safe_z$orig)
      zhi = max(safe_z$orig)
      tlo = min(safe_t$orig)
      thi = max(safe_t$orig)
    }else{
      message('Doing nothing: data is entirely outside of target FITS!')
      message('FITS: ', dim_fits[1], ' ',  dim_fits[2], ' ',  dim_fits[3], ' ',  dim_fits[4])
      return(NULL)
    }
  }
  
  # int_change = FALSE
  # 
  # if(integer=='byte' | integer=='8'){
  #   if(max(data,na.rm=TRUE) >= 2^7 | min(data,na.rm=TRUE) <= -2^7){
  #     integer = 'short'
  #     int_change = TRUE
  #   }
  # }
  # 
  # if(integer=='short' | integer=='16'){
  #   if(max(data,na.rm=TRUE) >= 2^15 | min(data,na.rm=TRUE) <= -2^15){
  #     integer = 'long'
  #     int_change = TRUE
  #   }
  # }
  # 
  # if(integer=='long' | integer=='int' | integer=='32'){
  #   if(max(data,na.rm=TRUE) >= 2^31 | min(data,na.rm=TRUE) <= -2^31){
  #     integer = 'longlong'
  #     int_change = TRUE
  #   }
  # }
  # 
  # if(int_change & (is.integer(data) | is.integer64(data))){
  #   message('Converted integer type to ',integer,' since data range is too large!')
  # }
  
  datatype = 0
  
  if(is.logical(data[1])){
    datatype = 11
  }
  
  if(datatype == 0 & is.integer(data[1])){
    datatype = 31
  }else if(is.integer64(data[1])){
    datatype = 81
  }
  
  if(datatype==0 & is.numeric(data[1])){
    datatype = 82
  }
  
  Cfits_write_img_subset(filename=filename, data=data, ext=ext, datatype=datatype, naxis=naxis,
                         fpixel0=xlo, fpixel1=ylo, fpixel2=zlo, fpixel3=tlo,
                         lpixel0=xhi, lpixel1=yhi, lpixel2=zhi, lpixel3=thi)
}

Rfits_write_vector = Rfits_write_image

Rfits_write_cube = Rfits_write_image

Rfits_write_array = Rfits_write_image

Rfits_tdigest = function(image, mask=NULL, chunk=100L, compression=1e3, verbose=TRUE){
  if(requireNamespace("tdigest", quietly=TRUE)){
    fd = tdigest::tdigest({}, compression=compression)
    image_ydim = dim(image)[2]
    if(chunk > image_ydim){chunk = image_ydim}
    loop = floor(image_ydim / chunk) - 1
    for(i in 0:loop){
      if(verbose & (i+1)%%10==0){
        message(i+1,' of ',loop+1)
      }
      if(is.null(mask)){
        tdigest::td_merge(tdigest::tdigest(image[,i*chunk+1:chunk],compression=compression),fd)
      }else{
        tdigest::td_merge(tdigest::tdigest(image[,i*chunk+1:chunk][mask[,i*chunk+1:chunk]==0],compression=compression),fd)
      }
    }
    if(loop*chunk + chunk < image_ydim){
      if(is.null(mask)){
        tdigest::td_merge(tdigest::tdigest(image[,(loop*chunk+chunk+1):image_ydim],compression=compression),fd)
      }else{
        tdigest::td_merge(tdigest::tdigest(image[,(loop*chunk+chunk+1):image_ydim][mask[,(loop*chunk+chunk+1):image_ydim]==0],compression=compression),fd)
      }
    }
    return(fd)
  }else{
    stop('The tdigest package is needed for Rfits_tdigest to work. Please install from CRAN.', call.=FALSE)
  }
}

Rfits_create_image = function(data, keyvalues=NULL, keycomments=NULL, comment=NULL, history=NULL,
                              filename='', ext=1, keypass=FALSE, ...){
  assertList(keyvalues)
  class(keyvalues) = 'keylist'
  assertList(keycomments, null.ok=TRUE)
  assertCharacter(comment, null.ok=TRUE)
  assertCharacter(history, null.ok=TRUE)
  assertCharacter(filename, max.len=1)
  assertIntegerish(ext, max.len=1)
  assertFlag(keypass)
  
  output = list(
    imDat = data,
    keyvalues = keyvalues,
    keycomments = keycomments,
    comment = comment,
    history = history,
    filename = filename,
    ext = ext,
    extname = keyvalues$EXTNAME,
    WCSref = keyvalues$WCSref
  )
  
  #Set initial class
  class(output) = c('Rfits_image', 'list')
  
  #Tidy things up
  output = Rfits_check_image(output, keypass=keypass)
  
  return(output)
}

Rfits_check_image = function(data, keypass=FALSE, ...){
  if(!inherits(data, c('Rfits_vector', 'Rfits_image', 'Rfits_cube', 'Rfits_array'))){
    stop('Object class is not of type Rfits_image, Rfits_cube or Rfits_array')
  }
  
  assertFlag(keypass)
  
  #Check imDat
  if(is.null(data$imDat)){
    stop('Missing imDat!')
  }
  
  im_dim = dim(data$imDat)
  Ndim = length(im_dim)
  
  #Check keyvalues
  if(is.null(data$keyvalues)){
    stop('Missing keyvalues!')
  }else{
    if(requireNamespace("Rwcs", quietly=TRUE) & keypass){
      data$keyvalues = Rwcs::Rwcs_keypass(data$keyvalues, ...)
    }
  }
  class(data$keyvalues) = 'Rfits_keylist'
  
  #Enforce key NAXIS keyvalues
  if(isTRUE(data$keyvalues$ZIMAGE)){
    data$keyvalues$ZNAXIS = Ndim
    data$keycomments$ZNAXIS = "number of data axes"
  }else{
    data$keyvalues$NAXIS = Ndim
    data$keycomments$NAXIS = "number of data axes"
  }
  
  
  for(i in 1:Ndim){
    if(isTRUE(data$keyvalues$ZIMAGE)){
      data$keyvalues[[paste0('ZNAXIS',i)]] = im_dim[i]
      data$keycomments[[paste0('ZNAXIS',i)]] = paste("length of data axis", i)
    }else{
      data$keyvalues[[paste0('NAXIS',i)]] = im_dim[i]
      data$keycomments[[paste0('NAXIS',i)]] = paste("length of data axis", i)
    }
  }
  
  #Reset keynames (this is always a good idea)
  data$keynames = names(data$keyvalues)
  
  #Check keycomments
  if(is.null(data$keycomments)){
    message('Missing keycomments! Filling with blank comments.')
    if(is.null(data$keycomments)){
      data$keycomments = as.list(rep('', length(data$keyvalues)))
    }
    names(data$keycomments) = data$keynames
  }else{
    if(length(data$keyvalues) == length(data$keycomments)){
      if(any(data$keynames != names(data$keycomments))){
        keycomments_new = as.list(rep('', length(data$keyvalues)))
        names(keycomments_new) = data$keynames
        match_keys = match(data$keynames, names(data$keycomments), nomatch=0)
        keycomments_new[match_keys] = data$keycomments[match_keys]
        data$keycomments = keycomments_new
      }
    }else{
      keycomments_new = as.list(rep('', length(data$keyvalues)))
      names(keycomments_new) = data$keynames
      match_keys = match(data$keynames, names(data$keycomments), nomatch=0)
      keycomments_new[match_keys] = data$keycomments[match_keys]
      data$keycomments = keycomments_new
    }
  }
  
  data$hdr = Rfits_keyvalues_to_hdr(data$keyvalues)
  data$header = Rfits_keyvalues_to_header(data$keyvalues, keycomments=data$keycomments, comment=data$comment, history=data$history)
  data$raw = Rfits_header_to_raw(data$header)
  
  if(is.null(data$ext)){
    data$ext = 1L
  }
  
  if(is.null(data$extname)){
    data$extname = NULL
  }
  
  if(is.null(data$WCSref)){
    data$WCSref = 'NULL'
  }
  
  if(Ndim == 1){
    class(data) = c('Rfits_vector', 'list')
  }else if(Ndim == 2){
    class(data) = c('Rfits_image', 'list')
  }else if(Ndim == 3){
    class(data) = c('Rfits_cube', 'list')
  }else if(Ndim == 4){
    class(data) = c('Rfits_array', 'list')
  }
  
  return(data)
}

Rfits_crop = function(image, cropNA=TRUE, cropInf=FALSE, cropZero=FALSE){
  if(! inherits(image, c('Rfits_image', 'Rfits_pointer'))){
    stop('image must be either Rfits_image or Rfits_pointer!')
  }
  
  if(inherits(image, 'Rfits_pointer')){
    image = image[,]
  }
  
  if(cropNA == FALSE & cropInf == FALSE & cropZero == FALSE){
    return(image)
  }
  
  xlo = 1L
  xhi = dim(image)[1]
  
  ylo = 1L
  yhi = dim(image)[2]
  
  tempsel = matrix(TRUE, xhi, yhi)
  
  if(cropNA){
    tempsel = !is.na(image$imDat)
  }
  
  if(cropInf){
    tempsel = tempsel & is.finite(image$imDat)
  }
  
  if(cropZero){
    tempsel = tempsel & image$imDat != 0
  }
  
  tempsel = which(tempsel, arr.ind=TRUE)
  
  xlo = min(tempsel[,1], na.rm=TRUE)
  xhi = max(tempsel[,1], na.rm=TRUE)
  ylo = min(tempsel[,2], na.rm=TRUE)
  yhi = max(tempsel[,2], na.rm=TRUE)
  
  if(xlo == 1L & xhi == dim(image)[1] & ylo == 1L & yhi == dim(image)[2]){
    return(image)
  }
  
  return(image[c(xlo,xhi), c(ylo,yhi)])
}
