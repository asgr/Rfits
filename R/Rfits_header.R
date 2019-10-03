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

Rfits_read_key=function(filename, keyname, keytype='numeric', ext = 1){
  assertCharacter(filename, max.len=1)
  filename=path.expand(filename)
  assertAccess(filename, access='r')
  assertCharacter(keyname, len = 1)
  assertCharacter(keytype, max.len=1)
  assertIntegerish(ext, len = 1)
  if(keytype=='numeric'){
    typecode=82
  }else if(keytype=='string'){
    typecode=16
  }else{
    stop('Unrecognised keytype')
  }
  return(Cfits_read_key(filename=filename, keyname=keyname, typecode=typecode, ext=ext))
}

Rfits_write_key=function(filename, keyname, keyvalue, comment="", ext=1){
  assertCharacter(filename, max.len=1)
  filename=path.expand(filename)
  assertAccess(filename, access='w')
  assertCharacter(keyname, len = 1)
  if(length(keyvalue)!=1){stop('keyvalue must be length 1')}
  assertCharacter(comment, len = 1)
  assertIntegerish(ext, len = 1)
  
  typecode=0
  if(is.integer(keyvalue)){typecode=31}
  if(is.integer64(keyvalue)){typecode=81}
  if(typecode==0 & is.numeric(keyvalue)){
    if(keyvalue %% 1 == 0){
      if(keyvalue < 2^31){
        keyvalue=as.integer(keyvalue)
        typecode=31
      }else{
        keyvalue=as.integer64(keyvalue)
        typecode=81
      }
    }else{
      typecode=82
    }
  }
  if(is.character(keyvalue)){typecode=16}
  Cfits_update_key(filename=filename, keyvalue=keyvalue, keyname=keyname, comment=comment, ext=ext, typecode=typecode)
}

Rfits_delete_key=function(filename, keyname, ext=1){
  assertCharacter(filename, max.len=1)
  filename=path.expand(filename)
  assertAccess(filename, access='w')
  assertCharacter(keyname, len = 1)
  assertIntegerish(ext, len = 1)
  Cfits_delete_key(filename=filename, keyname=keyname, ext=ext)
}

Rfits_read_header=function(filename, ext=1){
  assertCharacter(filename, max.len=1)
  filename=path.expand(filename)
  assertAccess(filename, access='r')
  assertIntegerish(ext, len = 1)
  
  #raw header
  header=Cfits_read_header(filename=filename, ext=ext)
  
  #remove comments for parsing
  remove=grep(pattern = 'COMMENT', header)
  if(length(remove)>0){
    headertemp = header[-remove]
  }else{
    headertemp = header
  }
  
  #hdr vector
  hdr = .parse_header(headertemp)
  
  #keyword list
  keynames = hdr[c(T,F)]
  suppressWarnings({keyvalues = as.list(as.numeric(hdr[c(F,T)]))})
  isint = unlist(keyvalues[!is.na(keyvalues)]) %% 1 == 0
  keyvalues[!is.na(keyvalues)][isint] = as.integer(keyvalues[!is.na(keyvalues)][isint])
  keyvalues[is.na(keyvalues)] = hdr[c(F,T)][is.na(keyvalues)]
  keyvalues[hdr[c(F,T)] == 'T']=TRUE
  keyvalues[hdr[c(F,T)] == 'F']=FALSE
  names(keyvalues)=keynames
  
  #comments list
  comments = lapply(strsplit(headertemp,'/ '),function(x) x[2])
  names(comments) = keynames
  
  return(list(header=header, hdr=hdr, keyvalues=keyvalues, comments=comments, keynames=keynames))
}

Rfits_write_header=function(filename, keyvalues, comments, keynames, ext=1){
  assertCharacter(filename, max.len=1)
  filename=path.expand(filename)
  assertAccess(filename, access='w')
  assertIntegerish(ext, len = 1)
  assertList(keyvalues, min.len = 1)
  if(! missing(comments)){
    if(is.list(comments)){
      assertList(comments, len = length(keyvalues))
      comments=unlist(comments)
    }
    assertCharacter(comments, len = length(keyvalues))
  }
  if(missing(keynames)){
    keynames=names(keyvalues)
  }
  assertCharacter(keynames, max.len=length(keyvalues))
  
  for(i in 1:length(keyvalues)){
    if(missing(comments)){
      Rfits_write_key(filename=filename, keyname = keynames[i], keyvalue = keyvalues[[i]], comment="", ext=ext)
    }else{
      Rfits_write_key(filename=filename, keyname = keynames[i], keyvalue = keyvalues[[i]], comment = comments[[i]], ext=ext)
    }
  }
}

Rfits_info=function(filename){
  ext = Cfits_read_nhdu(filename)
  headers=list()
  info={}
  for(i in 1:ext){
    temp = Rfits_read_header(filename, i)
    info = c(info, temp$header[1])
    headers=c(headers, list(temp))
  }
  return(invisible(list(summary = info, headers=headers)))
}

.parse_header=function(header){
  #Based on parseHdr in FITSio
  good = which(substr(header, 9, 10) == "= ")
  header=header[good]
  Nhead=length(header)
  for (i in 1:Nhead) {
    header[i] = strsplit(header[i], "/")[[1]][1]
  }
  hdr = unlist(strsplit(header, "="))
  smark = grep("'", hdr)
  for (i in smark) {
    hdr[i] = gsub("''", "aAlJ2fZ47xx", hdr[i])
    hdr[i] = strsplit(hdr[i], "'")[[1]][2]
    hdr[i] = gsub("aAlJ2fZ47xx", "''", hdr[i])
  }
  for (i in 1:length(hdr)) {
    hdr[i] = sub("^ *", "", hdr[i])
    hdr[i] = sub(" *$", "", hdr[i])
  }
  return(invisible(hdr))
}
