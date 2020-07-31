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
                          yhi=NULL, zlo=NULL, zhi=NULL, remove_HIERARCH=FALSE){
  assertCharacter(filename, max.len=1)
  filename=path.expand(filename)
  assertAccess(filename, access='r')
  assertIntegerish(ext, len=1)
  assertFlag(header)
  assertIntegerish(xlo, null.ok=TRUE)
  assertIntegerish(xhi, null.ok=TRUE)
  assertIntegerish(ylo, null.ok=TRUE)
  assertIntegerish(yhi, null.ok=TRUE)
  assertIntegerish(zlo, null.ok=TRUE)
  assertIntegerish(zhi, null.ok=TRUE)
  assertFlag(remove_HIERARCH)
  
  subset=FALSE
  
  if(!is.null(xlo) | !is.null(xhi) | !is.null(ylo) | !is.null(yhi) | !is.null(zlo) | !is.null(zhi) | header){
    
    hdr=Rfits_read_header(filename=filename, ext=ext, remove_HIERARCH=remove_HIERARCH)
    
    if(isTRUE(hdr$keyvalues$ZIMAGE)){
      naxis1=hdr$keyvalues$ZNAXIS1
      naxis2=hdr$keyvalues$ZNAXIS2
      naxis3=hdr$keyvalues$ZNAXIS3
      datatype=hdr$keyvalues$ZBITPIX
    }else{
      naxis1=hdr$keyvalues$NAXIS1
      naxis2=hdr$keyvalues$NAXIS2
      naxis3=hdr$keyvalues$NAXIS3
      datatype=hdr$keyvalues$BITPIX
    }
    
    if(ext==1 & (is.null(naxis1) | is.null(naxis2))){
      stop('Missing naxis1 or naxis2, usually this means the first image is after ext=1 (e.g. try setting ext=2).')
    }
    
    if(is.null(naxis1)){
      naxis1 = 1
    }
    if(is.null(naxis2)){
      naxis2 = 1
    }
    if(is.null(naxis3)){
      naxis3 = 1
    }
    
    if(is.null(xlo)){xlo=1}else{subset=TRUE}
    if(is.null(ylo)){ylo=1}else{subset=TRUE}
    if(is.null(zlo)){zlo=1}else{subset=TRUE}
    if(is.null(xhi)){xhi=naxis1}else{subset=TRUE}
    if(is.null(yhi)){yhi=naxis2}else{subset=TRUE}
    if(is.null(zhi)){zhi=naxis3}else{subset=TRUE}
    if(xlo < 1){message('xlo out of data range, truncating!'); xlo=1}
    if(xhi > naxis1){message('xhi out of data range, truncating!'); xhi=naxis1}
    if(ylo < 1){message('ylo out of data range, truncating!'); ylo=1}
    if(yhi > naxis2){message('yhi out of data range, truncating!'); yhi=naxis2}
    if(zlo < 1){message('zlo out of data range, truncating!'); zlo=1}
    if(zhi > naxis3){message('zhi out of data range, truncating!'); zhi=naxis3}
    assertIntegerish(xlo, lower=1, upper=naxis1, len=1)
    assertIntegerish(xhi, lower=1, upper=naxis1, len=1)
    assertIntegerish(ylo, lower=1, upper=naxis2, len=1)
    assertIntegerish(yhi, lower=1, upper=naxis2, len=1)
    assertIntegerish(zlo, lower=1, upper=naxis3, len=1)
    assertIntegerish(zhi, lower=1, upper=naxis3, len=1)
    
    if(xhi<xlo){stop('xhi must be larger than xlo')}
    if(yhi<ylo){stop('yhi must be larger than ylo')}
    if(zhi<zlo){stop('zhi must be larger than zlo')}
  }
  
  if(subset){
    image=Cfits_read_img_subset(filename=filename, fpixel0=xlo, fpixel1=ylo, fpixel2=zlo,
                                lpixel0=xhi, lpixel1=yhi, lpixel2=zhi, ext=ext, datatype=datatype)
    if(naxis3 > 1){
      image = array(image, dim=c(xhi-xlo+1, yhi-ylo+1, zhi-zlo+1))
    }
  }else{
    naxis1 = try(Cfits_read_key(filename=filename, keyname='ZNAXIS1', typecode=82, ext=ext), silent=TRUE)
    if(is.numeric(naxis1)){
      naxis2 = try(Cfits_read_key(filename=filename, keyname='ZNAXIS2', typecode=82, ext=ext), silent=TRUE)
      naxis3 = try(Cfits_read_key(filename=filename, keyname='ZNAXIS3', typecode=82, ext=ext), silent=TRUE)
      datatype = Cfits_read_key(filename=filename, keyname='ZBITPIX', typecode=82, ext=ext)
    }else{
      naxis1 = try(Cfits_read_key(filename=filename, keyname='NAXIS1', typecode=82, ext=ext), silent=TRUE)
      naxis2 = try(Cfits_read_key(filename=filename, keyname='NAXIS2', typecode=82, ext=ext), silent=TRUE)
      naxis3 = try(Cfits_read_key(filename=filename, keyname='NAXIS3', typecode=82, ext=ext), silent=TRUE)
      datatype = Cfits_read_key(filename=filename, keyname='BITPIX', typecode=82, ext=ext)
    }
    if(!is.numeric(naxis1)){
      stop('NAXIS1 is missing- at least this NAXIS required!')
    }
    if(!is.numeric(naxis2)){
      naxis2 = 1
    }
    if(!is.numeric(naxis3)){
      naxis3 = 1
    }
    image=Cfits_read_img(filename=filename, naxis1=naxis1, naxis2=naxis2, naxis3=naxis3,
                         ext=ext, datatype=datatype)
    if(naxis3 > 1){
      image = array(image, dim=c(naxis1, naxis2, naxis3))
    }
  }
  
  if(header){
    if(subset){
      hdr$hdr[which(hdr$hdr=='NAXIS1')+1] = xhi - xlo +1
      hdr$hdr[which(hdr$hdr=='NAXIS2')+1] = yhi - ylo +1
      hdr$hdr[which(hdr$hdr=='CRPIX1')+1] = as.character(hdr$keyvalues$CRPIX1 - xlo + 1)
      hdr$hdr[which(hdr$hdr=='CRPIX2')+1] = as.character(hdr$keyvalues$CRPIX2 - ylo + 1)
      hdr$keyvalues$NAXIS1 = xhi - xlo +1
      hdr$keyvalues$NAXIS2 = yhi - ylo +1
      hdr$keyvalues$CRPIX1 = hdr$keyvalues$CRPIX1 - xlo + 1
      hdr$keyvalues$CRPIX2 = hdr$keyvalues$CRPIX2 - ylo +1
      hdr$keycomments$NAXIS1 = paste(hdr$keycomments$NAXIS1, 'SUBMOD')
      hdr$keycomments$NAXIS2 = paste(hdr$keycomments$NAXIS2, 'SUBMOD')
      hdr$keycomments$CRPIX1 = paste(hdr$keycomments$CRPIX1, 'SUBMOD')
      hdr$keycomments$CRPIX2 = paste(hdr$keycomments$CRPIX2, 'SUBMOD')
      hdr$header[grep('NAXIS1', hdr$header)] = paste(formatC('NAXIS1', width=8,flag="-"),'=',formatC(hdr$keyvalues$NAXIS1, width=21),' / ',hdr$keycomments$NAXIS1,sep='')
      hdr$header[grep('NAXIS2', hdr$header)] = paste(formatC('NAXIS2', width=8,flag="-"),'=',formatC(hdr$keyvalues$NAXIS2, width=21),' / ',hdr$keycomments$NAXIS2,sep='')
      hdr$header[grep('CRPIX1', hdr$header)] = paste(formatC('CRPIX1', width=8,flag="-"),'=',formatC(hdr$keyvalues$CRPIX1, width=21),' / ',hdr$keycomments$CRPIX1,sep='')
      hdr$header[grep('CRPIX2', hdr$header)] = paste(formatC('CRPIX2', width=8,flag="-"),'=',formatC(hdr$keyvalues$CRPIX2, width=21),' / ',hdr$keycomments$CRPIX2,sep='')
    }
    output=list(imDat=image, hdr=hdr$hdr, header=hdr$header, keyvalues=hdr$keyvalues,
                keycomments=hdr$keycomments, keynames=hdr$keynames, comment=hdr$comment, history=hdr$history, filename=filename, ext=ext)
    if(naxis3==1){
      class(output) = c('Rfits_image', class(output))
    }else if(naxis3>1){
      class(output) = c('Rfits_cube', class(output))
    }
    return(invisible(output))
  }else{
    return(invisible(image)) 
  }
}

Rfits_read_cube = Rfits_read_image

Rfits_write_image=function(data, filename='temp.fits', ext=1, keyvalues, keycomments,
                           keynames, comment, history, numeric='single',
                           integer='long', create_ext=TRUE, create_file=TRUE,
                           overwrite_file=TRUE, bzero=0, bscale=1){
  assertFlag(create_ext)
  assertFlag(create_file)
  assertFlag(overwrite_file)
  assertCharacter(filename, max.len=1)
  filename=path.expand(filename)
  if(create_file){
    assertPathForOutput(filename, overwrite=overwrite_file)
  }else{
    assertFileExists(filename)
    assertAccess(filename, access='w')
  }
  if(testFileExists(filename) & overwrite_file & create_file){
    file.remove(filename)
  }
  if(inherits(data, what=c('Rfits_image', 'Rfits_cube'))){
    if(missing(keyvalues)){keyvalues=data$keyvalues}
    if(missing(keycomments)){keycomments=data$keycomments}
    if(missing(keynames)){keynames=data$keynames}
    if(missing(comment)){comment=data$comment}
    if(missing(history)){history=data$history}
    data=data$imDat
  }
  assertArray(data)
  if(!missing(keyvalues)){keyvalues=as.list(keyvalues)}
  if(!missing(keycomments)){keycomments=as.list(keycomments)}
  if(!missing(keynames)){keynames=as.character(keynames)}
  if(!missing(comment)){comment=as.character(comment)}
  if(!missing(history)){history=as.character(history)}
  if(is.numeric(numeric)){numeric=as.character(numeric)}
  if(is.numeric(integer)){integer=as.character(integer)}
  assertCharacter(numeric, len=1)
  assertCharacter(integer, len=1)
  assertNumeric(bzero)
  assertNumeric(bscale)
  
  naxes = dim(data)
  naxis = length(naxes)
  if(naxis == 2){
    naxes = c(naxes,1)
  }
  
  bitpix=0
  
  if(max(data,na.rm=TRUE)>2^30){
    integer='long'
  }

  if(is.integer(data[1])){
    if(integer=='short' | integer=='int' | integer=='16'){
      bitpix=16
      datatype=21
    }else if(integer=='long' | integer=='32'){
      bitpix=32
      datatype=31
    }else{
      stop('integer type must be short/int/16 (16 bit) or long/32 (32 bit)')
    }
  }else if(is.integer64(data[1])){
    bitpix=64
    datatype=81
  }
  if(bitpix==0){
    if(numeric=='single' | numeric=='float' | numeric=='32'){
      bitpix=-32
      datatype=42
    }else if (numeric=='double' | numeric=='64'){
      bitpix=-64
      datatype=82
    }else{
      stop('numeric type must be single/float/32 or double/64')
    }
  }
  
  # Below is old deprecated code for reference, safer to create image (Cfits_create_image),
  # write header (Rfits_write_header), then write pixels (Cfits_write_pix)
  
  # if(!missing(keyvalues)){
  #   if(!is.null(keyvalues$BZERO)){bzero = keyvalues$BZERO}
  #   if(!is.null(keyvalues$BSCALE)){bscale = keyvalues$BSCALE}
  # }
  
  #Cfits_write_image(filename, data=data, datatype=datatype, naxis=naxis, naxis1=naxes[1],
  #                  naxis2=naxes[2], naxis3=naxes[3], ext=ext, create_ext=create_ext,
  #                  create_file=create_file, bitpix=bitpix, bzero=bzero, bscale=bscale)
  
  if(!missing(keyvalues) & !missing(bzero)){keyvalues$BZERO = bzero}
  if(!missing(keyvalues) & !missing(bscale)){keyvalues$BSCALE = bscale}
  
  Cfits_create_image(filename=filename, naxis=naxis, naxis1=naxes[1], naxis2=naxes[2], naxis3=naxes[3],
                     ext=ext, create_ext=create_ext, create_file=create_file, bitpix=bitpix)
  
  ext = Cfits_read_nhdu(filename=filename)
  
  if(!missing(keyvalues)){
    keyvalues$BITPIX = bitpix
    
    if(!missing(comment)){
      checkAA=grep("FITS \\(Flexible Image Transport System\\) format is defined in 'Astronomy",comment)
      if(length(checkAA)>0){comment = comment[-checkAA]}
      checkAA=grep("and Astrophysics', volume 376, page 359; bibcode: 2001A&A...376..359H",comment)
      if(length(checkAA)>0){comment = comment[-checkAA]}
    }
    filename=strsplit(filename,split = "[",fixed=TRUE)[[1]][1]
    Rfits_write_header(filename=filename, keyvalues=keyvalues,
                       keycomments=keycomments, keynames=keynames,
                       comment=comment, history=history, ext=ext)
  }
  Cfits_write_pix(filename=filename, data=data, datatype=datatype, naxis=naxis, naxis1=naxes[1],
                  naxis2=naxes[2], naxis3=naxes[3], ext=ext)
}

Rfits_write_cube = Rfits_write_image

plot.Rfits_image=function(x, ...){
  if(!inherits(x, 'Rfits_image')){
    stop('Object class is not of type Rfits_image!')
  }
  if(requireNamespace("Rwcs", quietly=TRUE)){
    Rwcs::Rwcs_image(x, ...)
  }else{
    message('The Rwcs package is needed to plot a Rfits_image object.')
  }
}

plot.Rfits_cube=function(x, slice=1, ...){
  if(!inherits(x, 'Rfits_cube')){
    stop('Object class is not of type Rfits_image!')
  }
  if(requireNamespace("Rwcs", quietly=TRUE)){
    Rwcs::Rwcs_image(x$imDat[,,slice], keyvalues=x$keyvalues, ...)
  }else{
    message('The Rwcs package is needed to plot a Rfits_cube object.')
  }
}

plot.Rfits_image_pointer=function(x, ...){
  if(!inherits(x, 'Rfits_image_pointer')){
    stop('Object class is not of type Rfits_image!')
  }
  if(requireNamespace("Rwcs", quietly=TRUE)){
    Rwcs::Rwcs_image(x[,], keyvalues=x$keyvalues, ...)
  }else{
    message('The Rwcs package is needed to plot a Rfits_image object.')
  }
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
