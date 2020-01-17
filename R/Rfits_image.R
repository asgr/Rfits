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

Rfits_read_image=function(filename, ext=1, header=TRUE, xlo=NULL, xhi=NULL, ylo=NULL, yhi=NULL){
  assertCharacter(filename, max.len=1)
  filename=path.expand(filename)
  assertAccess(filename, access='r')
  assertIntegerish(ext, len = 1)
  
  subset=FALSE
  
  if(!is.null(xlo) | !is.null(xhi) | !is.null(ylo) | !is.null(yhi) | header){
    
    hdr=Rfits_read_header(filename = filename, ext = ext)
    
    if(isTRUE(hdr$keyvalues$ZIMAGE)){
      naxis1=hdr$keyvalues$ZNAXIS1
      naxis2=hdr$keyvalues$ZNAXIS2
      datatype=hdr$keyvalues$ZBITPIX
    }else{
      naxis1=hdr$keyvalues$NAXIS1
      naxis2=hdr$keyvalues$NAXIS2
      datatype=hdr$keyvalues$BITPIX
    }
    naxis3 = 1
    
    if(ext==1 & (is.null(naxis1) | is.null(naxis2))){
      stop('Missing naxis1 or naxis2, usually this means the first image is after ext=1 (e.g. try setting ext=2).')
    }
    
    if(is.null(xlo)){xlo=1}else{subset=TRUE}
    if(is.null(ylo)){ylo=1}else{subset=TRUE}
    if(is.null(xhi)){xhi=naxis1}else{subset=TRUE}
    if(is.null(yhi)){yhi=naxis2}else{subset=TRUE}
    if(xlo<1){xlo=1}
    if(xhi>naxis1){xhi=naxis1}
    if(ylo<1){ylo=1}
    if(yhi>naxis2){yhi=naxis2}
    assertIntegerish(xlo, lower = 1, upper = naxis1, len = 1)
    assertIntegerish(xhi, lower = 1, upper = naxis1, len = 1)
    assertIntegerish(ylo, lower = 1, upper = naxis2, len = 1)
    assertIntegerish(ylo, lower = 1, upper = naxis2, len = 1)
    
    if(xhi<xlo){stop('xhi must be larger than xlo')}
    if(yhi<ylo){stop('yhi must be larger than ylo')}
  }
  
  if(subset){
    image=Cfits_read_img_subset(filename=filename, fpixel0=xlo, fpixel1=ylo,
                                lpixel0=xhi, lpixel1=yhi, ext=ext, datatype=datatype) 
  }else{
    naxis1 = try(Cfits_read_key(filename=filename, keyname='ZNAXIS1', typecode=82, ext=ext), silent = TRUE)
    if(is.numeric(naxis1)){
      naxis2 = Cfits_read_key(filename=filename, keyname='ZNAXIS2', typecode=82, ext=ext)
      naxis3 = try(Cfits_read_key(filename=filename, keyname='ZNAXIS3', typecode=82, ext=ext), silent = TRUE)
      datatype = Cfits_read_key(filename=filename, keyname='ZBITPIX', typecode=82, ext=ext)
    }else{
      naxis1 = Cfits_read_key(filename=filename, keyname='NAXIS1', typecode=82, ext=ext)
      naxis2 = Cfits_read_key(filename=filename, keyname='NAXIS2', typecode=82, ext=ext)
      naxis3 = try(Cfits_read_key(filename=filename, keyname='NAXIS3', typecode=82, ext=ext), silent = TRUE)
      datatype = Cfits_read_key(filename=filename, keyname='BITPIX', typecode=82, ext=ext)
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
                keycomments=hdr$keycomments, keynames=hdr$keynames, comment=hdr$comment, history=hdr$history)
    if(naxis3==1){
      class(output)='Rfits_image'
    }else if(naxis3>1){
      class(output)='Rfits_cube'
    }
    return(invisible(output))
  }else{
    return(invisible(image)) 
  }
}

Rfits_write_image=function(image, filename, ext=1, keyvalues, keycomments,
                           keynames, comment, history, numeric='single',
                           integer='long', create_ext=TRUE, create_file=TRUE,
                           overwrite_file=TRUE){
  assertFlag(create_ext)
  assertFlag(create_file)
  assertFlag(overwrite_file)
  assertCharacter(filename, max.len = 1)
  filename=path.expand(filename)
  if(create_file){
    assertPathForOutput(filename, overwrite=overwrite_file)
  }else{
    assertFileExists(filename)
  }
  if(testFileExists(filename) & overwrite_file & create_file){
    file.remove(filename)
  }
  if(class(image)=='Rfits_image' | class(image)=='Rfits_cube'){
    if(missing(keyvalues)){keyvalues=image$keyvalues}
    if(missing(keycomments)){keycomments=image$keycomments}
    if(missing(keynames)){keynames=image$keynames}
    if(missing(comment)){comment=image$comment}
    if(missing(history)){history=image$history}
    image=image$imDat
  }
  assertArray(image)
  if(!missing(keyvalues)){keyvalues=as.list(keyvalues)}
  if(!missing(keycomments)){keycomments=as.list(keycomments)}
  if(!missing(keynames)){keynames=as.character(keynames)}
  if(!missing(comment)){comment=as.character(comment)}
  if(!missing(history)){history=as.character(history)}
  if(is.numeric(numeric)){numeric=as.character(numeric)}
  if(is.numeric(integer)){integer=as.character(integer)}
  assertCharacter(numeric, len = 1)
  assertCharacter(integer, len = 1)
  
  naxes = dim(image)
  naxis = length(naxes)
  if(naxis == 2){
    naxes = c(naxes,1)
  }
  
  bitpix=0
  
  if(max(image,na.rm=TRUE)>2^30){
    integer='long'
  }

  if(is.integer(image[1])){
    if(integer=='short' | integer=='int' | integer=='16'){
      bitpix=16
      datatype=21
    }else if(integer=='long' | integer=='32'){
      bitpix=32
      datatype=31
    }else{
      stop('integer type must be short/int/16 (16 bit) or long/32 (32 bit)')
    }
  }
  if(bitpix==0){
    # if(all(image %% 1 == 0)){
    #   image=as.integer(image)
    #   if(integer=='short' | integer=='int' | integer=='16'){
    #     bitpix=16
    #     datatype=21
    #   }else if(integer=='long' | integer=='32'){
    #     bitpix=32
    #     datatype=31
    #   }else{
    #     stop('integer type must be short/int/16 (16 bit) or long/32 (32 bit)')
    #   }
    # }else{
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
  #Cfits_create_image(filename, bitpix=bitpix, naxis1=naxis[1], naxis2=naxis[2])
  Cfits_write_image(filename, data=image, datatype=datatype, naxis=naxis, naxis1=naxes[1],
                    naxis2=naxes[2], naxis3=naxes[3], ext=ext, create_ext=create_ext,
                    create_file=create_file, bitpix=bitpix)
  ext = Cfits_read_nhdu(filename)
  if(!missing(keyvalues)){
    keyvalues$BITPIX = bitpix
    
    if(!missing(comment)){
      checkAA=grep("FITS \\(Flexible Image Transport System\\) format is defined in 'Astronomy",comment)
      if(length(checkAA)>0){comment = comment[-checkAA]}
      checkAA=grep("and Astrophysics', volume 376, page 359; bibcode: 2001A&A...376..359H",comment)
      if(length(checkAA)>0){comment = comment[-checkAA]}
    }
    filename=strsplit(filename,split = "[",fixed=TRUE)[[1]][1]
    Rfits_write_header(filename = filename, keyvalues = keyvalues,
                       keycomments = keycomments, keynames = keynames,
                       comment=comment, history=history, ext=ext)
  }
}

plot.Rfits_image=function(x, ...){
  if(class(x)!='Rfits_image'){
    stop('Object class is not of type Rfits_image!')
  }
  if(requireNamespace("Rwcs", quietly = TRUE)){
    Rwcs::Rwcs_image(x, ...)
  }else{
    message('The Rwcs package is needed to plot a Rfits_image object.')
  }
}

plot.Rfits_cube=function(x, slice=1, ...){
  if(class(x)!='Rfits_cube'){
    stop('Object class is not of type Rfits_image!')
  }
  if(requireNamespace("Rwcs", quietly = TRUE)){
    Rwcs::Rwcs_image(x$imDat[,,slice], keyvalues=x$keyvalues, ...)
  }else{
    message('The Rwcs package is needed to plot a Rfits_cube object.')
  }
}
