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

Rfits_read_key=function(filename='temp.fits', keyname, keytype='numeric', ext=1){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  filename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  assertAccess(filename, access='r')
  assertCharacter(keyname, len=1)
  assertCharacter(keytype, max.len=1)
  assertIntegerish(ext, len=1)
  
  if(keytype=='numeric'){
    typecode=82
  }else if(keytype=='string'){
    typecode=16
  }else{
    stop('Unrecognised keytype')
  }
  return(Cfits_read_key(filename=filename, keyname=keyname, typecode=typecode, ext=ext))
}

Rfits_write_key=function(filename='temp.fits', keyname, keyvalue, keycomment="", ext=1){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  filename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  assertAccess(filename, access='w')
  assertCharacter(keyname, len=1)
  if(length(keyvalue)!=1){stop('keyvalue must be length 1')}
  assertCharacter(keycomment, len=1)
  assertIntegerish(ext, len=1)
  
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
  if(is.logical(keyvalue)){
    typecode=14
    keyvalue=as.integer(keyvalue)
  }
  
  if(nchar(keyname) > 8){
    if(substr(keyname, 1, 8) != 'HIERARCH'){
      keyname = paste0('HIERARCH  ', keyname)
    }
  }
    
  if(is.character(keyvalue)){typecode=16}
  Cfits_update_key(filename=filename, keyvalue=keyvalue, keyname=keyname, keycomment=keycomment, ext=ext, typecode=typecode)
}

Rfits_write_comment=function(filename='temp.fits', comment="", ext=1){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  filename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  assertAccess(filename, access='w')
  assertCharacter(comment, len=1)
  assertIntegerish(ext, len=1)
  
  Cfits_write_comment(filename=filename, comment=paste('  ',comment,sep=''), ext=ext)
}

Rfits_write_history=function(filename='temp.fits', history="", ext=1){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  filename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  assertAccess(filename, access='w')
  assertCharacter(history, len=1)
  assertIntegerish(ext, len=1)
  
  Cfits_write_history(filename=filename, history=paste('  ',history,sep=''), ext=ext)
}

Rfits_write_date=function(filename='temp.fits', ext=1){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  filename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  assertAccess(filename, access='w')
  assertIntegerish(ext, len=1)
  
  Cfits_write_date(filename=filename, ext=ext)
}

Rfits_delete_key=function(filename='temp.fits', keyname, ext=1){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  filename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  assertAccess(filename, access='w')
  assertCharacter(keyname, len=1)
  assertIntegerish(ext, len=1)
  
  Cfits_delete_key(filename=filename, keyname=keyname, ext=ext)
}

Rfits_read_header=function(filename='temp.fits', ext=1, remove_HIERARCH=FALSE, keypass=FALSE){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  filename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  assertAccess(filename, access='r')
  assertIntegerish(ext, len=1)
  assertFlag(remove_HIERARCH)
  
  #raw header
  header = Cfits_read_header(filename=filename, ext=ext)
  
  #read nkey
  nkey = Cfits_read_nkey(filename=filename, ext=ext)
  
  #remove comments for parsing
  loc_comment = grep('COMMENT', header)
  loc_history = grep('HISTORY', header)
  
  if(length(loc_comment)>0){
    comment = gsub('COMMENT ', '', header[loc_comment])
    #comment = gsub('  ','',comment) #not sure this works generically, probably better to keep it raw-er.
  }else{
    comment = NULL
  }
  
  if(length(loc_history)>0){
    history = gsub('HISTORY ', '', header[loc_history])
    #history = gsub('  ','',history) #not sure this works generically, probably better to keep it raw-er.
  }else{
    history = NULL
  }
  
  if(length(loc_comment)>0 | length(loc_history)>0){
    headertemp = header[-c(loc_comment, loc_history)]
  }else{
    headertemp = header
  }
  
  #hdr vector
  hdr = Rfits_header_to_hdr(headertemp, remove_HIERARCH=remove_HIERARCH)
  
  #keyword list
  keyvalues = Rfits_hdr_to_keyvalues(hdr)
  keynames = names(keyvalues)
  
  loc_HIERARCH = grep('HIERARCH', keynames)
  if(length(loc_HIERARCH)>0){
    keynames_goodhead = keynames[-loc_HIERARCH] 
    pattern_goodhead = paste(c(paste0(format(keynames_goodhead, width=8), '='), 'HIERARCH'), collapse = '|')
  }else{
    pattern_goodhead = paste(paste0(format(keynames, width=8), '='), collapse = '|')
  }
  loc_goodhead = grep(pattern_goodhead, headertemp)
  
  headertemp = headertemp[loc_goodhead]
  #comments list
  keycomments = lapply(strsplit(headertemp,' / '),function(x) x[2])
  names(keycomments) = keynames
  
  if(requireNamespace("Rwcs", quietly=TRUE) & keypass){
    keyvalues = Rwcs::Rwcs_keypass(keyvalues)
    keynames = names(keyvalues)
    header = Rfits_keyvalues_to_header(keyvalues=keyvalues, keycomments=keycomments, comment=comment, history=history)
    hdr = Rfits_header_to_hdr(headertemp, remove_HIERARCH=remove_HIERARCH)
  }
  
  #weirdly, I found some issues using wcslib when CDX_Y wasn't in front of the distortion terms. This is a hard fix
  
  #CDloc = grep('CD[1-2]_[1-2]   =', header)
  #raw = Rfits_header_to_raw(c(header[CDloc], header[-CDloc]))
  
  #The above was never really the issue- all came back to mangled headers.
  
  output = list(keyvalues=keyvalues,
                keycomments=keycomments,
                keynames=keynames,
                header=header,
                hdr=hdr,
                raw=raw,
                comment=comment,
                history=history,
                nkey=nkey)
  class(output) = c('Rfits_header', class(output))
  return(output)
}

Rfits_read_header_raw=function(filename='temp.fits', ext=1){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  filename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  assertAccess(filename, access='r')
  assertIntegerish(ext, len=1)
  return(Cfits_read_header_raw(filename=filename, ext=ext))
}

Rfits_write_header=function(filename='temp.fits', keyvalues, keycomments, keynames,
                            comment, history, ext=1, create_ext=FALSE, create_file=FALSE,
                            overwrite_file=FALSE){
  assertFlag(create_ext)
  assertFlag(create_file)
  assertFlag(overwrite_file)
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  filename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  if(create_file){
    assertPathForOutput(filename, overwrite=overwrite_file)
    keynames_old = NULL
  }else{
    assertFileExists(filename)
    assertAccess(filename, access='w')
    keynames_old = Rfits_read_header(filename, ext=ext)$keynames
  }
  if(testFileExists(filename) & overwrite_file & create_file){
    file.remove(filename)
  }
  assertList(keyvalues, min.len=1)
  if(inherits(keyvalues, what='Rfits_header')){
    if(missing(keycomments)){keycomments=keyvalues$keycomments}
    if(missing(keynames)){keynames=keyvalues$keynames}
    if(missing(comment)){comment=keyvalues$comment}
    if(missing(history)){history=keyvalues$history}
    keyvalues = keyvalues$keyvalues
  }
  if(! missing(keycomments)){
    if(is.list(keycomments)){
      assertList(keycomments, len=length(keyvalues))
      keycomments = unlist(keycomments)
    }
    assertCharacter(keycomments, len=length(keyvalues))
  }
  if(missing(keynames)){
    keynames = names(keyvalues)
  }
  assertCharacter(keynames, max.len=length(keyvalues))
  if(! missing(comment)){
    assertCharacter(comment, null.ok = TRUE)
  }
  if(! missing(history)){
    assertCharacter(history, null.ok = TRUE)
  }
  assertIntegerish(ext, len=1)
  if(create_file){
    assertPathForOutput(filename, overwrite=overwrite_file)
  }else{
    assertFileExists(filename)
  }
  if(testFileExists(filename) & overwrite_file & create_file){
    file.remove(filename)
  }
  
  if(create_ext | create_file){
    Cfits_create_header(filename, create_ext=create_ext, create_file=create_file)
    ext = Cfits_read_nhdu(filename=filename)
  }
  
  for(i in 1:length(keyvalues)){
    if(ext==1){
      if(!keynames[i] %in% c('XTENSION', 'PCOUNT', ' GCOUNT')){
        if((! keynames[i] %in% keynames_old) & (! paste0('Z',keynames[i]) %in% keynames_old)){
          if(missing(keycomments)){
            Rfits_write_key(filename=filename, keyname = keynames[i], keyvalue = keyvalues[[i]], keycomment="", ext=ext)
          }else{
            Rfits_write_key(filename=filename, keyname = keynames[i], keyvalue = keyvalues[[i]], keycomment = keycomments[[i]], ext=ext)
          }
        }
      }
    }
  }
  
  if(!missing(comment)){
    if(length(comment) > 0){
      for(i in comment){
        Rfits_write_comment(filename, comment=i)
      }
    }
  }
  
  if(!missing(history)){
    if(length(history) > 0){
      for(i in history){
        Rfits_write_history(filename, history=i)
      }
    }
  }
}

Rfits_info = function(filename='temp.fits', remove_HIERARCH=FALSE){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  filename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  assertAccess(filename, access='r')
  assertFlag(remove_HIERARCH)
  
  ext = Cfits_read_nhdu(filename=filename)
  headers = list()
  info = {}
  for(i in 1:ext){
    temp = Rfits_read_header(filename, i, remove_HIERARCH = remove_HIERARCH)
    info = c(info, temp$header[1])
    headers = c(headers, list(temp))
  }
  return(invisible(list(summary = info, headers=headers)))
}

Rfits_write_chksum=function(filename='temp.fits'){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  filename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  assertAccess(filename, access='w')
  
  Cfits_write_chksum(filename=filename)
}

Rfits_verify_chksum=function(filename='temp.fits', verbose=TRUE){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  filename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  assertAccess(filename, access='r')
  assertFlag(verbose)
  
  out = Cfits_verify_chksum(filename=filename, verbose=verbose)
  out = as.character(out)
  names(out) = c('DATASUM', 'CHECKSUM')
  out[out=='1'] = 'correct'
  out[out=='0'] = 'missing'
  out[out=='-1'] = 'incorrect'
  return(invisible(out))
}

Rfits_get_chksum = function(filename='temp.fits'){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  filename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  assertAccess(filename, access='r')
  
  out = Cfits_get_chksum(filename=filename)
  names(out) = c('DATASUM', 'CHECKSUM')
  return(out)
}

Rfits_nhdu = function(filename='temp.fits'){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  filename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  assertAccess(filename, access='r')
  
  return(Cfits_read_nhdu(filename=filename))
}

Rfits_nkey = function(filename='temp.fits', ext=1){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  filename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  assertAccess(filename, access='r')
  assertIntegerish(ext, len=1)
  return(Cfits_read_nkey(filename=filename, ext=ext))
}

Rfits_header_to_hdr = function(header, remove_HIERARCH=FALSE){
  assertCharacter(header)
  assertFlag(remove_HIERARCH)
  
  #Based on parseHdr in FITSio
  sel_HIERARCH = grep('HIERARCH', header)
  good = sort(unique(c(which(substr(header, 9, 9) == "="),sel_HIERARCH)))
  if(remove_HIERARCH){
    header = sub('HIERARCH  ', '', header)
  }
  header_good = header[good]
  Nhead = length(header_good)
  headlist = list()
  for (i in 1:Nhead){
    headtemp = strsplit(header_good[i], " /")[[1]][1]
    find_eq = gregexpr('=', headtemp)[[1]][1] #more generic, even though it should be at character 9 in FITS standard
    headlist[[i]] = c(substr(headtemp, 1, find_eq-1), substr(headtemp, find_eq+1, nchar(headtemp)))
  }
  
  hdr = unlist(headlist)
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
  return(hdr)
}

Rfits_hdr_to_keyvalues = function(hdr){
  assertCharacter(hdr)
  
  keynames = hdr[c(T,F)]
  suppressWarnings({keyvalues = as.list(as.numeric(hdr[c(F,T)]))})
  numerickey = !is.na(keyvalues)
  if(any(numerickey)){
    suppressWarnings({isint = unlist(keyvalues[numerickey]) %% 1 == 0 & abs(unlist(keyvalues[numerickey])) <= .Machine$integer.max})
    if(any(isint)){
      keyvalues[numerickey][isint] = as.integer(keyvalues[numerickey][isint])
    }
  }
  if(any(is.na(keyvalues))){
    keyvalues[is.na(keyvalues)] = hdr[c(F,T)][is.na(keyvalues)]
  }
  keyvalues[hdr[c(F,T)] == 'T'] = TRUE
  keyvalues[hdr[c(F,T)] == 'F'] = FALSE
  names(keyvalues) = keynames
  return(keyvalues)
}

Rfits_header_to_keyvalues=function(header, remove_HIERARCH=FALSE){
  return(Rfits_hdr_to_keyvalues(Rfits_header_to_hdr(header=header, remove_HIERARCH=remove_HIERARCH)))
}

Rfits_keyvalues_to_hdr = function(keyvalues){
  assertList(keyvalues)
  temp_out = rep('', 2*length(keyvalues))
  temp_out[seq(1,2*length(keyvalues)-1,by=2)] = names(keyvalues)
  temp_keyvalues = as.character(keyvalues)
  temp_keyvalues[temp_keyvalues=='TRUE'] = 'T'
  temp_keyvalues[temp_keyvalues=='FALSE'] = 'F'
  temp_out[seq(2,2*length(keyvalues),by=2)] = temp_keyvalues
  return(temp_out)
}

Rfits_keyvalues_to_header = function(keyvalues, keycomments=NULL, comment=NULL, history=NULL){
  assertList(keyvalues, null.ok=FALSE)
  assertList(keycomments, null.ok=TRUE)
  assertCharacter(comment, null.ok=TRUE)
  assertCharacter(history, null.ok=TRUE)
  if(inherits(keyvalues, c('Rfits_header', 'Rfits_vector', 'Rfits_image', 'Rfits_cube', 'Rfits_array'))){
    keycomments = keyvalues$keycomments
    comment = keyvalues$comment
    history = keyvalues$history
    keyvalues = keyvalues$keyvalues
  }
  temp_out = {}
  for(i in 1:length(keyvalues)){
    if(!is.na(keyvalues[[i]])){
      if(is.character(keyvalues[[i]])){
        temp_keyvalue = paste0('\'',keyvalues[[i]],'\'')
      }else{
        if(is.numeric(keyvalues[[i]])){
          if(keyvalues[[i]] <= 1e-4 | keyvalues[[i]] >= 1e4){
            temp_keyvalue = formatC(keyvalues[[i]],format='E', digits=10)
          }else{
            temp_keyvalue = as.character(keyvalues[[i]])
          }
        }else{
          temp_keyvalue = as.character(keyvalues[[i]])
        }
      }
      if(temp_keyvalue == 'TRUE'){temp_keyvalue = 'T'}
      if(temp_keyvalue == 'FALSE'){temp_keyvalue = 'F'}
      temp_key = paste0(formatC(names(keyvalues[i]),width = 8, flag='-'), '= ', formatC(temp_keyvalue,width=20),' / ')
      if(!is.null(keycomments)){
        temp_key = paste0(temp_key,keycomments[[i]])
      }
      temp_out = c(temp_out, temp_key)
    }
  }
  if(!is.null(comment)){
    temp_out = c(temp_out, paste0('COMMENT ',comment))
  }
  if(!is.null(history)){
    temp_out = c(temp_out, paste0('HISTORY ',history))
  }
  return(temp_out)
}

Rfits_header_to_raw = function(header){
  # if(fix){
  #   CDloc = grep('CD[1-2]_[1-2]   =', header)
  #   header = c(header[CDloc], header[-CDloc])
  # }
  return(paste(formatC(substr(header,1,79), width=80, flag='-'),sep='',collapse = ''))
}

Rfits_raw_to_header = function(header){
  nkey = nchar(header)/80
  return(substring(header, 1+(80*((1:nkey)-1)), 80*(1:nkey)))
}

Rfits_encode_chksum = function(checksum, complement=FALSE){
  assertNumeric(checksum, max.len=1)
  assertFlag(complement)
  
  return(Cfits_encode_chksum(sum=checksum, complement=complement))
}

Rfits_decode_chksum = function(checksum, complement=FALSE){
  assertCharacter(checksum, max.len=1)
  assertFlag(complement)
  
  return(Cfits_decode_chksum(ascii=checksum, complement=complement))
}
