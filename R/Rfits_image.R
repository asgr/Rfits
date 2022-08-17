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
                          force_logical=FALSE, bad=NULL, keypass=FALSE, zap=NULL){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  filename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  assertAccess(filename, access='r')
  filename = Rfits_gunzip(filename)
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
  
  subset=FALSE
  
  if(!is.null(xlo) | !is.null(xhi) | !is.null(ylo) | !is.null(yhi) | !is.null(zlo) | !is.null(zhi) | !is.null(tlo) | !is.null(thi) | header){
    
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
      
      if(xhi<xlo){stop('xhi must be larger than xlo')}
      if(yhi<ylo){stop('yhi must be larger than ylo')}
      if(zhi<zlo){stop('zhi must be larger than zlo')}
      if(thi<tlo){stop('thi must be larger than tlo')}
    }
  }
  
  if(subset){
    if(safex$safe & safey$safe & safez$safe & safet$safe){
      try({
        temp_image = Cfits_read_img_subset(filename=filename, fpixel0=xlo, fpixel1=ylo, fpixel2=zlo, fpixel3=tlo,
                                  lpixel0=xhi, lpixel1=yhi, lpixel2=zhi, lpixel3=thi, ext=ext, datatype=datatype)
        
        if(naxis2 > 1 & naxis3 == 1 & naxis4 == 1){
          temp_image = matrix(temp_image, xhi-xlo+1, yhi-ylo+1)
        }
        
        if(naxis3 > 1 & naxis4 == 1){
          temp_image = array(temp_image, dim=c(xhi-xlo+1, yhi-ylo+1, zhi-zlo+1))
        }
        if(naxis4 > 1){
          temp_image = array(temp_image, dim=c(xhi-xlo+1, yhi-ylo+1, zhi-zlo+1, thi-tlo+1))
        }
      })
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
      image = Cfits_read_img(filename=filename, naxis1=naxis1, naxis2=naxis2, naxis3=naxis3,
                           naxis4=naxis4, ext=ext, datatype=datatype)
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
  
  if(subset){
    naxis1 = safex$len_tar
    naxis2 = safey$len_tar
    naxis3 = safez$len_tar
    naxis4 = safet$len_tar
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
  Cfits_write_pix(filename=filename, data=data, datatype=datatype, naxis=naxis, naxis1=naxes[1],
                  naxis2=naxes[2], naxis3=naxes[3], naxis4=naxes[4], ext=ext)
}

Rfits_write_pix = function(data, filename, ext=1, numeric='single', integer='long'){
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
  
  Cfits_write_pix(filename=filename, data=data, datatype=datatype, naxis=naxis, naxis1=naxes[1],
                  naxis2=naxes[2], naxis3=naxes[3], naxis4=naxes[4], ext=as.integer(ext))
}

Rfits_write_vector = Rfits_write_image

Rfits_write_cube = Rfits_write_image

Rfits_write_array = Rfits_write_image

plot.Rfits_image=function(x, useraw=FALSE, ...){
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

plot.Rfits_cube=function(x, slice=1, useraw=FALSE, ...){
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

plot.Rfits_array=function(x, slice=c(1,1), useraw=FALSE, ...){
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

plot.Rfits_vector=function(x, ...){
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

lines.Rfits_vector=function(x, ...){
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

Rfits_tdigest=function(image, mask=NULL, chunk=100L, compression=1e3, verbose=TRUE){
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

centre = function(x, useraw=FALSE){
  UseMethod("centre", x)
}

centre.Rfits_image = function(x, useraw=FALSE, ...){
  if(!inherits(x, c('Rfits_image', 'Rfits_pointer', 'Rfits_header'))){
    stop('Object class is not of type Rfits_image / Rfits_pointer / Rfits_header')
  }
  if(inherits(x, 'Rfits_header')){
    if(is.null(x$keyvalues$NAXIS)){
      stop('No NAXIS!')
    }else{
      if(x$keyvalues$NAXIS < 2){
        stop('NAXIS: ', x$keyvalues$NAXIS)
      }
    }
  }
  dims = dim(x)
  if(requireNamespace("Rwcs", quietly=TRUE)){
    if(useraw){header = x$raw}else{header = NULL}
    output = Rwcs::Rwcs_p2s(dims[1]/2, dims[2]/2, keyvalues = x$keyvalues, header=header, pixcen='R', ...)
    return(output)
  }else{
    message('The Rwcs package is needed to find the centre of a Rfits_image object.')
  }
}

center = function(x, useraw=FALSE){
  UseMethod("center", x)
}

#other useful methods:
center.Rfits_image = centre.Rfits_image
centre.Rfits_pointer = centre.Rfits_image
center.Rfits_pointer = centre.Rfits_image
centre.Rfits_header = centre.Rfits_image
center.Rfits_header = centre.Rfits_image

corners = function(x, useraw=FALSE){
  UseMethod("corners", x)
}

corners.Rfits_image = function(x, useraw=FALSE, ...){
  if(!inherits(x, c('Rfits_image', 'Rfits_pointer', 'Rfits_header'))){
    stop('Object class is not of type Rfits_image / Rfits_pointer / Rfits_header')
  }
  if(inherits(x, 'Rfits_header')){
    if(is.null(x$keyvalues$NAXIS)){
      stop('No NAXIS!')
    }else{
      if(x$keyvalues$NAXIS < 2){
        stop('NAXIS: ', x$keyvalues$NAXIS)
      }
    }
  }
  dims = dim(x)
  if(requireNamespace("Rwcs", quietly=TRUE)){
    if(useraw){header = x$raw}else{header = NULL}
    BL = Rwcs::Rwcs_p2s(0, 0, keyvalues = x$keyvalues, header=header, pixcen='R', ...)
    TL = Rwcs::Rwcs_p2s(0, dims[2], keyvalues = x$keyvalues, header=header, pixcen='R', ...)
    TR = Rwcs::Rwcs_p2s(dims[1], dims[2], keyvalues = x$keyvalues, header=header, pixcen='R', ...)
    BR = Rwcs::Rwcs_p2s(dims[1], 0, keyvalues = x$keyvalues, header=header, pixcen='R', ...)
    output = rbind(BL, TL, TR, BR)
    row.names(output) = c('BL', 'TL', 'TR', 'BR')
    return(output)
  }else{
    message('The Rwcs package is needed to find the corners of a Rfits_image object.')
  }
}

corners.Rfits_pointer = corners.Rfits_image
corners.Rfits_header = corners.Rfits_image

pixscale = function(x, useraw=FALSE){
  UseMethod("pixscale", x)
}

pixscale.Rfits_image = function(x, useraw=FALSE, ...){
  if(!inherits(x, c('Rfits_image', 'Rfits_pointer', 'Rfits_header'))){
    stop('Object class is not of type Rfits_image / Rfits_pointer / Rfits_header')
  }
  if(inherits(x, 'Rfits_header')){
    if(is.null(x$keyvalues$NAXIS)){
      stop('No NAXIS!')
    }else{
      if(x$keyvalues$NAXIS < 2){
        stop('NAXIS: ', x$keyvalues$NAXIS)
      }
    }
  }
  dims = dim(x)
  if(requireNamespace("Rwcs", quietly=TRUE)){
    if(useraw){header = x$raw}else{header = NULL}
    output = Rwcs::Rwcs_p2s(dims[1]/2 + c(-0.5,0.5), dims[2]/2 + c(-0.5,0.5), keyvalues = x$keyvalues, header=header, pixcen='R', ...)
    output[,1] = output[,1] * cos(mean(output[,2])*pi/180)
    return(2545.584412*sqrt(diff(output[,1])^2 + diff(output[,2])^2)) # 2545.584412 = 3600/sqrt(2)
  }else{
    message('The Rwcs package is needed to find the centre of a Rfits_image object.')
  }
}

pixscale.Rfits_pointer = pixscale.Rfits_image
pixscale.Rfits_header = pixscale.Rfits_image

Rfits_create_image = function(image, keyvalues, keycomments=NULL, comment = NULL, history = NULL,
                              filename='', ext=1, keypass=TRUE){
  
  if(requireNamespace("Rwcs", quietly=TRUE) & keypass){
    keyvalues = Rwcs::Rwcs_keypass(keyvalues)
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
