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
                          force_logical=FALSE, bad=NULL, keypass=FALSE, zap=NULL, sparse=1L){
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
    
    hdr = Rfits_read_header(filename=filename, ext=ext, remove_HIERARCH=remove_HIERARCH, keypass=keypass, zap=zap)
    
    if(isTRUE(hdr$keyvalues$ZIMAGE)){
      naxis1=hdr$keyvalues$ZNAXIS1
      naxis2=hdr$keyvalues$ZNAXIS2
      naxis3=hdr$keyvalues$ZNAXIS3
      naxis4=hdr$keyvalues$ZNAXIS4
      datatype=hdr$keyvalues$ZBITPIX
    }else{
      naxis1=hdr$keyvalues$NAXIS1
      naxis2=hdr$keyvalues$NAXIS2
      naxis3=hdr$keyvalues$NAXIS3
      naxis4=hdr$keyvalues$NAXIS4
      datatype=hdr$keyvalues$BITPIX
    }
    
    if(ext==1 & (is.null(naxis1))){
      message('Missing NAXIS1, usually this means the first image is after ext=1 (e.g. try setting ext=2).')
    }
    
    Ndim = 0
    if(!is.null(naxis1)){Ndim = 1}
    if(!is.null(naxis2)){Ndim = 2}
    if(!is.null(naxis3)){Ndim = 3}
    if(!is.null(naxis4)){Ndim = 4}
    
    if(is.null(naxis1)){
      message('NAXIS1 is missing- this is pretty weird!')
      naxis1 = 1
    }
    if(is.null(naxis2)){
      naxis2 = 1
    }
    if(is.null(naxis3)){
      naxis3 = 1
    }
    if(is.null(naxis4)){
      naxis4 = 1
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
                                           sparse=sparse)
        
        if(sparse > 1){
          temp_image = temp_image*sparse^2
        }
        
        if(naxis2 > 1 & naxis3 == 1 & naxis4 == 1){
          temp_image = matrix(temp_image, floor((xhi - xlo)/sparse) + 1L, floor((yhi - ylo)/sparse) + 1L)
        }
        
        if(naxis3 > 1 & naxis4 == 1){
          temp_image = array(temp_image, dim=c(floor((xhi - xlo)/sparse) + 1L, floor((yhi - ylo)/sparse) + 1L, floor((zhi - zlo)/sparse) + 1L))
        }
        if(naxis4 > 1){
          temp_image = array(temp_image, dim=c(floor((xhi - xlo)/sparse) + 1L, floor((yhi - ylo)/sparse) + 1L, floor((zhi - zlo)/sparse) + 1L, floor((thi - tlo)/sparse) + 1L))
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
          hdr$keyvalues$CRPIX1 = (hdr$keyvalues$CRPIX1 - safex$lo_tar)/sparse + 1
          
          if(Ndim >= 2){
            hdr$keyvalues$NAXIS2 = dim(temp_image)[2]
            hdr$keyvalues$CRPIX2 = (hdr$keyvalues$CRPIX2 - safey$lo_tar)/sparse + 1
          }
          
          if(Ndim >= 3){
            hdr$keyvalues$NAXIS3 = dim(temp_image)[3]
            hdr$keyvalues$CRPIX3 = (hdr$keyvalues$CRPIX3 - safez$lo_tar)/sparse + 1
          }
          
          if(Ndim >= 4){
            hdr$keyvalues$NAXIS4 = dim(temp_image)[4]
            hdr$keyvalues$CRPIX4 = (hdr$keyvalues$CRPIX4 - safet$lo_tar)/sparse + 1
          }
          
          hdr$keyvalues$CD1_1 = hdr$keyvalues$CD1_1 * sparse
          hdr$keyvalues$CD1_2 = hdr$keyvalues$CD1_2 * sparse
          hdr$keyvalues$CD2_1 = hdr$keyvalues$CD2_1 * sparse
          hdr$keyvalues$CD2_2 = hdr$keyvalues$CD2_2 * sparse
          
          output = Rfits_create_image(temp_image, keyvalues = hdr$keyvalues) #scale by sparse^2 to get fluxes roughly right
          
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
    
    naxis1 = safex$len_tar
    naxis2 = safey$len_tar
    naxis3 = safez$len_tar
    naxis4 = safet$len_tar
  }else{
    # Leave the naxis reads- this is the only way to know the naxis if we aren't given a header!
    naxis1 = try(Cfits_read_key(filename=filename, keyname='ZNAXIS1', typecode=82, ext=ext), silent=TRUE)
    if(is.numeric(naxis1)){
      naxis2 = try(Cfits_read_key(filename=filename, keyname='ZNAXIS2', typecode=82, ext=ext), silent=TRUE)
      naxis3 = try(Cfits_read_key(filename=filename, keyname='ZNAXIS3', typecode=82, ext=ext), silent=TRUE)
      naxis4 = try(Cfits_read_key(filename=filename, keyname='ZNAXIS4', typecode=82, ext=ext), silent=TRUE)
      datatype = Cfits_read_key(filename=filename, keyname='ZBITPIX', typecode=82, ext=ext)
    }else{
      naxis1 = try(Cfits_read_key(filename=filename, keyname='NAXIS1', typecode=82, ext=ext), silent=TRUE)
      naxis2 = try(Cfits_read_key(filename=filename, keyname='NAXIS2', typecode=82, ext=ext), silent=TRUE)
      naxis3 = try(Cfits_read_key(filename=filename, keyname='NAXIS3', typecode=82, ext=ext), silent=TRUE)
      naxis4 = try(Cfits_read_key(filename=filename, keyname='NAXIS4', typecode=82, ext=ext), silent=TRUE)
      datatype = Cfits_read_key(filename=filename, keyname='BITPIX', typecode=82, ext=ext)
    }
    if(!is.numeric(naxis1)){
      message('NAXIS1 is missing- this is pretty weird!')
      naxis1 = 1
    }
    if(!is.numeric(naxis2)){
      naxis2 = 1
    }
    if(!is.numeric(naxis3)){
      naxis3 = 1
    }
    if(!is.numeric(naxis4)){
      naxis4 = 1
    }
    try({
      image = Cfits_read_img(filename=filename, ext=ext, datatype=datatype,
                             naxis1=naxis1, naxis2=naxis2, naxis3=naxis3, naxis4=naxis4)
    })
    if(!is.numeric(image)){
      message(paste0('Image read failed for extension '), ext, '. Replacing values with NA!')
      image = NA
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
  
  if(naxis1 == 1 & naxis2 == 1 & naxis3 == 1 & naxis4 == 1){
    image = as.vector(image)
  }
  if(naxis2 == 1 & naxis3 == 1 & naxis4 == 1){
    image = as.vector(image)
  }
  if(naxis2 > 1 & naxis3 == 1 & naxis4 == 1){
    dim(image) =c(naxis1, naxis2)
  }
  if(naxis3 > 1 & naxis4 == 1){
    dim(image) =c(naxis1, naxis2, naxis3)
  }
  if(naxis4 > 1){
    dim(image) =c(naxis1, naxis2, naxis3, naxis4)
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

    if(naxis2 == 1 & naxis3 == 1 & naxis4 == 1){
      class(output) = c('Rfits_vector', class(output))
    }else if(naxis3 == 1 & naxis4 == 1){
      class(output) = c('Rfits_image', class(output))
    }else if(naxis3 > 1 & naxis4 == 1){
      class(output) = c('Rfits_cube', class(output))
    }else if(naxis4 > 1){
      class(output) = c('Rfits_array', class(output))
    }
    return(invisible(output))
  }else{
    return(invisible(image)) 
  }
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
  
  ext = Cfits_read_nhdu(filename=filename)
  
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

Rfits_write_pix = function(data, filename, ext=1, numeric='single', integer='long',
                           xlo=1L, ylo=1L, zlo=1L, tlo=1L){
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
      # if(!missing(keyvalues)){
      #   if(!is.null(keyvalues$BZERO)){
      #     if(keyvalues$BZERO + max(data, na.rm=T) > 2^31){
      #       keyvalues$BZERO = 0
      #       message('Changing BZERO to 0 to prevent integer overflow!')
      #     }
      #   }
      # }
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
  
  Cfits_write_img_subset(filename=filename, data=data, ext=ext, datatype=datatype, naxis=naxis,
                         fpixel0=xlo, fpixel1=ylo, fpixel2=zlo, fpixel3=tlo,
                         lpixel0=xhi, lpixel1=yhi, lpixel2=zhi, lpixel3=thi)
}

# // [[Rcpp::export]]
# SEXP Cfits_write_img_subset(Rcpp::String filename, SEXP data, int ext=1, int datatype = -32, int naxis=2,
#                             long fpixel0=1, long fpixel1=1, long fpixel2=1, long fpixel3=1,
#                             long lpixel0=100, long lpixel1=100, long lpixel2=1, long lpixel3=1
#                             
# ){

Rfits_write_vector = Rfits_write_image

Rfits_write_cube = Rfits_write_image

Rfits_write_array = Rfits_write_image

plot.Rfits_image = function(x, useraw=FALSE, ...){
  if(!inherits(x, 'Rfits_image')){
    stop('Object class is not of type Rfits_image!')
  }
  if(is.null(x$keyvalues$CRVAL1)){
    magimage(x$imDat, ...)
  }else{
    if(requireNamespace("Rwcs", quietly=TRUE)){
      if(useraw){header = x$raw}else{header = NULL}
      Rwcs::Rwcs_image(x$imDat, keyvalues=x$keyvalues, header=header, ...)
    }else{
      message('The Rwcs package is needed to plot a Rfits_image object.')
    }
  }
}

plot.Rfits_cube = function(x, slice=1, useraw=FALSE, ...){
  if(!inherits(x, 'Rfits_cube')){
    stop('Object class is not of type Rfits_image!')
  }
  if(requireNamespace("Rwcs", quietly=TRUE)){
    if(useraw){header = x$raw}else{header = NULL}
    Rwcs::Rwcs_image(x$imDat[,,slice], keyvalues=x$keyvalues, header=header, ...)
  }else{
    message('The Rwcs package is needed to plot a Rfits_cube object.')
  }
}

plot.Rfits_array = function(x, slice=c(1,1), useraw=FALSE, ...){
  if(!inherits(x, 'Rfits_cube')){
    stop('Object class is not of type Rfits_image!')
  }
  if(requireNamespace("Rwcs", quietly=TRUE)){
    if(useraw){header = x$raw}else{header = NULL}
    Rwcs::Rwcs_image(x$imDat[,,slice[1],slice[2]], keyvalues=x$keyvalues, header=header, ...)
  }else{
    message('The Rwcs package is needed to plot a Rfits_cube object.')
  }
}

plot.Rfits_vector = function(x, ...){
  if(!inherits(x, 'Rfits_vector')){
    stop('Object class is not of type Rfits_vector!')
  }
  xref = 1:length(x$imDat)
  if(!is.null(x$keyvalues$CRPIX1)){
    xref = xref - x$keyvalues$CRPIX1
  }
  if(!is.null(x$keyvalues$CDELT1)){
    xref = xref * x$keyvalues$CDELT1
  }
  if(!is.null(x$keyvalues$CRVAL1)){
    xref = xref + x$keyvalues$CRVAL1
  }
  if(requireNamespace("magicaxis", quietly=TRUE)){
    magicaxis::magplot(xref, x$imDat, type='l', ...)
  }else{
    plot(xref, x$imDat, type='l', ...)
  }
}

lines.Rfits_vector = function(x, ...){
  if(!inherits(x, 'Rfits_vector')){
    stop('Object class is not of type Rfits_vector!')
  }
  xref = 1:length(x$imDat)
  if(!is.null(x$keyvalues$CRPIX1)){
    xref = xref - x$keyvalues$CRPIX1
  }
  if(!is.null(x$keyvalues$CDELT1)){
    xref = xref * x$keyvalues$CDELT1
  }
  if(!is.null(x$keyvalues$CRVAL1)){
    xref = xref + x$keyvalues$CRVAL1
  }
  lines(xref, x$imDat, ...)
}

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

centre = function(x, useraw=FALSE, ...){
  UseMethod("centre", x)
}

centre.Rfits_image = function(x, useraw=FALSE, ...){
  if(!inherits(x, c('Rfits_image', 'Rfits_pointer', 'Rfits_header', 'Rfits_keylist'))){
    stop('Object class is not of type Rfits_image / Rfits_pointer / Rfits_header / Rfits_keylist')
  }
  
  if(inherits(x, 'Rfits_keylist')){
    keyvalues = x
  }else{
    keyvalues = x$keyvalues
  }
  
  if(inherits(x, c('Rfits_header', 'Rfits_keylist'))){
    if(is.null(keyvalues$NAXIS) & is.null(keyvalues$ZNAXIS)){
      message('No NAXIS! Probably not an image, returning NA.')
      return(NA)
    }else{
      if(!is.null(keyvalues$ZNAXIS)){
        if(keyvalues$ZNAXIS < 2){
          message('ZNAXIS: ', keyvalues$ZNAXIS,'.  Probably not an image, returning NA.')
          return(NA)
        }
      }else if(keyvalues$NAXIS < 2){
        message('NAXIS: ', keyvalues$NAXIS,'.  Probably not an image, returning NA.')
        return(NA)
      }
    }
  }
  
  im_dim = dim(x)
  if(requireNamespace("Rwcs", quietly=TRUE)){
    if(useraw){header = x$raw}else{header = NULL}
    output = Rwcs::Rwcs_p2s(im_dim[1]/2, im_dim[2]/2, keyvalues = keyvalues, header=header, pixcen='R', ...)
    return(output)
  }else{
    message('The Rwcs package is needed to find the centre of a Rfits_image object.')
  }
}

center = function(x, useraw=FALSE, ...){
  UseMethod("center", x)
}

#other useful methods:
center.Rfits_image = centre.Rfits_image
centre.Rfits_pointer = centre.Rfits_image
center.Rfits_pointer = centre.Rfits_image
centre.Rfits_header = centre.Rfits_image
center.Rfits_header = centre.Rfits_image
centre.Rfits_keylist = centre.Rfits_image
center.Rfits_keylist = centre.Rfits_image

Rfits_centre = function(filename, ext=1, useraw=FALSE, ...){
  temp_header = Rfits_read_header(filename=filename, ext=ext)
  return(centre(temp_header, useraw=useraw, ...))
}

Rfits_center = Rfits_centre

corners = function(x, useraw=FALSE, ...){
  UseMethod("corners", x)
}

corners.Rfits_image = function(x, useraw=FALSE, ...){
  if(!inherits(x, c('Rfits_image', 'Rfits_pointer', 'Rfits_header', 'Rfits_keylist'))){
    stop('Object class is not of type Rfits_image / Rfits_pointer / Rfits_header / Rfits_keylist')
  }
  
  if(inherits(x, 'Rfits_keylist')){
    keyvalues = x
  }else{
    keyvalues = x$keyvalues
  }
  
  if(inherits(x, c('Rfits_header', 'Rfits_keylist'))){
    if(is.null(keyvalues$NAXIS) & is.null(keyvalues$ZNAXIS)){
      message('No NAXIS! Probably not an image, returning NA.')
      return(NA)
    }else{
      if(!is.null(keyvalues$ZNAXIS)){
        if(keyvalues$ZNAXIS < 2){
          message('ZNAXIS: ', keyvalues$ZNAXIS,'.  Probably not an image, returning NA.')
          return(NA)
        }
      }else if(keyvalues$NAXIS < 2){
        message('NAXIS: ', keyvalues$NAXIS,'.  Probably not an image, returning NA.')
        return(NA)
      }
    }
  }
  
  im_dim = dim(x)
  if(requireNamespace("Rwcs", quietly=TRUE)){
    if(useraw){header = x$raw}else{header = NULL}
    BL = Rwcs::Rwcs_p2s(0, 0, keyvalues = keyvalues, header=header, pixcen='R', ...)
    TL = Rwcs::Rwcs_p2s(0, im_dim[2], keyvalues = keyvalues, header=header, pixcen='R', ...)
    TR = Rwcs::Rwcs_p2s(im_dim[1], im_dim[2], keyvalues = keyvalues, header=header, pixcen='R', ...)
    BR = Rwcs::Rwcs_p2s(im_dim[1], 0, keyvalues = keyvalues, header=header, pixcen='R', ...)
    output = rbind(BL, TL, TR, BR)
    row.names(output) = c('BL', 'TL', 'TR', 'BR')
    return(output)
  }else{
    message('The Rwcs package is needed to find the corners of a Rfits_image object.')
  }
}

corners.Rfits_pointer = corners.Rfits_image
corners.Rfits_header = corners.Rfits_image
corners.Rfits_keylist = corners.Rfits_image

Rfits_corners = function(filename, ext=1, useraw=FALSE, ...){
  temp_header = Rfits_read_header(filename=filename, ext=ext)
  return(corners(temp_header, useraw=useraw, ...))
}

pixscale = function(x, useraw=FALSE, unit='asec', ...){
  UseMethod("pixscale", x)
}

pixscale.Rfits_image = function(x, useraw=FALSE, unit='asec', ...){
  if(!inherits(x, c('Rfits_image', 'Rfits_pointer', 'Rfits_header', 'Rfits_keylist'))){
    stop('Object class is not of type Rfits_image / Rfits_pointer / Rfits_header / Rfits_keylist')
  }
  
  if(inherits(x, 'Rfits_keylist')){
    keyvalues = x
  }else{
    keyvalues = x$keyvalues
  }
  
  if(inherits(x, c('Rfits_header', 'Rfits_keylist'))){
    if(is.null(keyvalues$NAXIS) & is.null(keyvalues$ZNAXIS)){
      message('No NAXIS! Probably not an image, returning NA.')
      return(NA)
    }else{
      if(!is.null(keyvalues$ZNAXIS)){
        if(keyvalues$ZNAXIS < 2){
          message('ZNAXIS: ', keyvalues$ZNAXIS,'.  Probably not an image, returning NA.')
          return(NA)
        }
      }else if(keyvalues$NAXIS < 2){
        message('NAXIS: ', keyvalues$NAXIS,'.  Probably not an image, returning NA.')
        return(NA)
      }
    }
  }
  
  im_dim = dim(x) #this works on all classes
  if(requireNamespace("Rwcs", quietly=TRUE)){
    if(useraw){header = x$raw}else{header = NULL}
    output = Rwcs::Rwcs_p2s(im_dim[1]/2 + c(-0.5,0.5,-0.5), im_dim[2]/2 + c(-0.5,-0.5,0.5), keyvalues = keyvalues, header=header, pixcen='R', ...)
    if(max(abs(diff(output[,1]))) > 359){
      output[output[,1] > 359,1] = output[output[,1] > 359,1] - 360
    }
    output[,1] = output[,1] * cos(mean(output[,2])*pi/180)
    scale_deg = 0.7071068*sqrt(diff(output[1:2,1])^2 + diff(output[1:2,2])^2 + diff(output[c(1,3),1])^2 + diff(output[c(1,3),2])^2) # 0.7071068 = 1/sqrt(2)
    
    if(unit=='deg'){
      return(scale_deg)
    }else if(unit == 'asec'){
      return(scale_deg*3600)
    }else if(unit == 'amin'){
      return(scale_deg*60)
    }else if(unit=='rad'){
      return(scale_deg * (pi/180))
    }else{
      message('Not a valid unit, must be one of asec / amin / deg / rad')
    }
  }else{
    message('The Rwcs package is needed to find the centre of a Rfits_image object.')
  }
}

pixscale.Rfits_pointer = pixscale.Rfits_image
pixscale.Rfits_header = pixscale.Rfits_image
pixscale.Rfits_keylist = pixscale.Rfits_image

Rfits_pixscale = function(filename, ext=1, useraw=FALSE, unit='asec', ...){
  temp_header = Rfits_read_header(filename=filename, ext=ext)
  return(pixscale(temp_header, useraw=useraw, unit=unit, ...))
}

pixarea = function(x, useraw=FALSE, unit='asec2', ...){
  UseMethod("pixarea", x)
}

pixarea.Rfits_image = function(x, useraw=FALSE, unit='asec2', ...){
  if(!inherits(x, c('Rfits_image', 'Rfits_pointer', 'Rfits_header', 'Rfits_keylist'))){
    stop('Object class is not of type Rfits_image / Rfits_pointer / Rfits_header / Rfits_keylist')
  }
  
  if(inherits(x, 'Rfits_keylist')){
    keyvalues = x
  }else{
    keyvalues = x$keyvalues
  }
  
  if(inherits(x, 'Rfits_header')){
    if(is.null(keyvalues$NAXIS) & is.null(keyvalues$ZNAXIS)){
      message('No NAXIS! Probably not an image, returning NA.')
      return(NA)
    }else{
      if(!is.null(keyvalues$ZNAXIS)){
        if(keyvalues$ZNAXIS < 2){
          message('ZNAXIS: ', keyvalues$ZNAXIS,'.  Probably not an image, returning NA.')
          return(NA)
        }
      }else if(keyvalues$NAXIS < 2){
        message('NAXIS: ', keyvalues$NAXIS,'.  Probably not an image, returning NA.')
        return(NA)
      }
    }
  }
  
  im_dim = dim(x) #this works on all classes
  if(requireNamespace("Rwcs", quietly=TRUE)){
    if(useraw){header = x$raw}else{header = NULL}
    output = Rwcs::Rwcs_p2s(im_dim[1]/2 + c(-0.5,0.5,-0.5), im_dim[2]/2 + c(-0.5,-0.5,0.5), keyvalues = keyvalues, header=header, pixcen='R', ...)
    if(max(abs(diff(output[,1]))) > 359){
      output[output[,1] > 359,1] = output[output[,1] > 359,1] - 360
    }
    output[,1] = output[,1] * cos(mean(output[,2])*pi/180)
    area_deg = sqrt(diff(output[1:2,1])^2 + diff(output[1:2,2])^2)*sqrt(diff(output[c(1,3),1])^2 + diff(output[c(1,3),2])^2)
    
    if(unit=='deg2'){
      return(area_deg)
    }else if(unit == 'asec2'){
      return(area_deg*3600^2)
    }else if(unit == 'amin2'){
      return(area_deg*60^2)
    }else if(unit=='rad2' | unit=='str'){
      return(area_deg * (pi/180)^2)
    }else{
      message('Not a valid unit, must be one of asec2 / amin2 / deg2 / rad2 / str')
    }
  }else{
    message('The Rwcs package is needed to find the centre of a Rfits_image object.')
  }
}

pixarea.Rfits_pointer = pixarea.Rfits_image
pixarea.Rfits_header = pixarea.Rfits_image
pixarea.Rfits_keylist = pixarea.Rfits_image

Rfits_pixarea = function(filename, ext=1, useraw=FALSE, unit='asec2', ...){
  temp_header = Rfits_read_header(filename=filename, ext=ext)
  return(pixarea(temp_header, useraw=useraw, unit=unit, ...))
}

Rfits_create_image = function(image, keyvalues=NULL, keycomments=NULL, comment=NULL, history=NULL,
                              filename='', ext=1, keypass=TRUE){
  assertList(keyvalues)
  class(keyvalues) = 'keylist'
  
  if(requireNamespace("Rwcs", quietly=TRUE) & keypass){
    keyvalues = Rwcs::Rwcs_keypass(keyvalues)
  }
  
  if(is.null(keyvalues$NAXIS1)){
    keyvalues$NAXIS1 = dim(image)[1]
  }else{
    if(keyvalues$NAXIS1 != dim(image)[1]){
      keyvalues$NAXIS1 = dim(image)[1]
    }
  }
  
  if(is.null(keyvalues$NAXIS2)){
    keyvalues$NAXIS2 = dim(image)[2]
  }else{
    if(keyvalues$NAXIS2 != dim(image)[2]){
      keyvalues$NAXIS2 = dim(image)[2]
    }
  }
  
  hdr = Rfits_keyvalues_to_hdr(keyvalues)
  header = Rfits_keyvalues_to_header(keyvalues, keycomments=keycomments, comment=comment, history=history)
  raw = Rfits_header_to_raw(header)
  keynames = names(keyvalues)
  if(is.null(keycomments)){
    keycomments = as.list(rep('', length(keyvalues)))
  }
  names(keycomments) = keynames
  
  output = list(
    imDat = image,
    keyvalues = keyvalues,
    keycomments = keycomments,
    keynames = keynames,
    header = header,
    hdr = hdr,
    raw = raw,
    comment = comment,
    history = history,
    filename = filename,
    ext = ext,
    extname = keyvalues$EXTNAME
  )
  
  class(output) = c('Rfits_image', class(output))
  
  return(output)
}
