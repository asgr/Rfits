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

Rfits_read_key=function(filename='temp.fits', keyname, keytype='auto', ext=1){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  filename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  assertAccess(filename, access='r')
  filename = Rfits_gunzip(filename)
  assertCharacter(keyname, len=1)
  assertCharacter(keytype, max.len=1)
  if(is.character(ext)){ext = Rfits_extname_to_ext(filename, ext)}
  assertIntegerish(ext, len=1)
  
  keytype = tolower(keytype)
  
  if(keytype=='numeric'){
    typecode = 82
  }else if(keytype=='integer'){
    typecode = 41
  }else if(keytype=='string' | keytype=='character' | keytype=='char' | keytype=='auto'){
    typecode = 16
  }else{
    stop('Unrecognised keytype')
  }
  
  temp_key = try(Cfits_read_key(filename=filename, keyname=keyname, typecode=typecode, ext=ext), silent=TRUE)
  
  if(keytype=='auto'){
    if(inherits(temp_key, "try-error")){
      return(NA)
    }
    suppressWarnings({temp_key_num = as.numeric(temp_key)})
    if(!is.na(temp_key_num)){
      suppressWarnings({isint = temp_key_num %% 1 == 0 & abs(temp_key_num) <= .Machine$integer.max})
      if(isint){
        return(as.integer(temp_key_num))
      }else{
        return(as.numeric(temp_key_num))
      }
    }else{
      if(temp_key == 'T'){
        return(TRUE)
      }else if(temp_key == 'F'){
        return(FALSE)
      }else{
        if(temp_key == 'NA'){
          return(NA)
        }else{
          return(temp_key) #should be a character by now
        }
      }
    }
  }else{
    return(temp_key)
  }
}

Rfits_write_key=function(filename='temp.fits', keyname, keyvalue, keycomment="", ext=1){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  filename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  assertAccess(filename, access='w')
  assertCharacter(keyname, len=1)
  if(is.null(keyvalue)){
    if(identical(parent.frame(n=1), globalenv())){
      message('keyvalue for', keyname, ' is NULL. Nothing written.')
    }
    return(invisible(FALSE))
  }
  if(length(keyvalue)!=1){stop('keyvalue must be length 1')}
  assertCharacter(keycomment, len=1)
  if(is.character(ext)){ext = Rfits_extname_to_ext(filename, ext)}
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
  
  if(is.na(keyvalue)){
    keyvalue = 'NA'
  }
  
  if(is.character(keyvalue)){
    typecode=16
  }
  try(Cfits_update_key(filename=filename, keyvalue=keyvalue, keyname=keyname, keycomment=keycomment, ext=ext, typecode=typecode))
  return(invisible(TRUE))
}

Rfits_write_comment=function(filename='temp.fits', comment="", ext=1){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  filename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  assertAccess(filename, access='w')
  assertCharacter(comment, len=1)
  if(is.character(ext)){ext = Rfits_extname_to_ext(filename, ext)}
  assertIntegerish(ext, len=1)
  
  try(Cfits_write_comment(filename=filename, comment=paste('  ',comment,sep=''), ext=ext))
}

Rfits_write_history=function(filename='temp.fits', history="", ext=1){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  filename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  assertAccess(filename, access='w')
  assertCharacter(history, len=1)
  if(is.character(ext)){ext = Rfits_extname_to_ext(filename, ext)[1]}
  assertIntegerish(ext, len=1)
  
  try(Cfits_write_history(filename=filename, history=paste('  ',history,sep=''), ext=ext))
}

Rfits_write_date=function(filename='temp.fits', ext=1){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  filename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  assertAccess(filename, access='w')
  if(is.character(ext)){ext = Rfits_extname_to_ext(filename, ext)}
  assertIntegerish(ext, len=1)
  
  try(Cfits_write_date(filename=filename, ext=ext))
}

Rfits_delete_key=function(filename='temp.fits', keyname, ext=1){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  filename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  assertAccess(filename, access='w')
  assertCharacter(keyname, len=1)
  if(is.character(ext)){ext = Rfits_extname_to_ext(filename, ext)}
  assertIntegerish(ext, len=1)
  
  try(Cfits_delete_key(filename=filename, keyname=keyname, ext=ext))
}

Rfits_read_header=function(filename='temp.fits', ext=1, remove_HIERARCH=FALSE, keypass=FALSE, zap=NULL){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  filename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  assertAccess(filename, access='r')
  filename = Rfits_gunzip(filename)
  if(is.character(ext)){ext = Rfits_extname_to_ext(filename, ext)}
  assertIntegerish(ext, len=1)
  assertFlag(remove_HIERARCH)
  
  #raw header
  header = Cfits_read_header(filename=filename, ext=ext)
  
  if(!is.null(zap)){
    header = Rfits_header_zap(header, zap=zap)
  }
  
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
  class(keyvalues) = 'Rfits_keylist'
  
  nloop = ceiling(length(keynames)/1e3)
  loc_goodhead={}
  
  for(i in 1:nloop){
    temp_keynames = keynames[((i - 1)*1e3 + 1):min(i*1e3, length(keynames))]
    loc_HIERARCH = grep('HIERARCH', temp_keynames)
    if(length(loc_HIERARCH)>0){
      keynames_remHIERARCH = temp_keynames[-loc_HIERARCH] 
      pattern_goodhead = paste(c(paste0(format(keynames_remHIERARCH, width=8), '='), 'HIERARCH'), collapse = '|')
    }else{
      pattern_goodhead = paste(paste0(format(temp_keynames, width=8), '='), collapse = '|')
    }
    loc_goodhead = c(loc_goodhead, grep(pattern_goodhead, headertemp))
  }
  
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
  raw = Rfits_header_to_raw(header)
  
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
  filename = Rfits_gunzip(filename)
  if(is.character(ext)){ext = Rfits_extname_to_ext(filename, ext)}
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
  assertCharacter(keynames, len=length(keyvalues))
  if(! missing(comment)){
    assertCharacter(comment, null.ok = TRUE)
  }
  if(! missing(history)){
    assertCharacter(history, null.ok = TRUE)
  }
  if(is.character(ext)){ext = Rfits_extname_to_ext(filename, ext)}
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
    }else{
      if((! keynames[i] %in% keynames_old) & (! paste0('Z',keynames[i]) %in% keynames_old)){
        if(missing(keycomments)){
          Rfits_write_key(filename=filename, keyname = keynames[i], keyvalue = keyvalues[[i]], keycomment="", ext=ext)
        }else{
          Rfits_write_key(filename=filename, keyname = keynames[i], keyvalue = keyvalues[[i]], keycomment = keycomments[[i]], ext=ext)
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
  filename = Rfits_gunzip(filename)
  assertFlag(remove_HIERARCH)
  
  ext = Cfits_read_nhdu(filename=filename)
  headers = list()
  info = {}
  for(i in 1:ext){
    temp = Rfits_read_header(filename, i, remove_HIERARCH = remove_HIERARCH)
    info = c(info, temp$header[1])
    headers = c(headers, list(temp))
  }
  return(invisible(list(summary=info, headers=headers)))
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
  filename = Rfits_gunzip(filename)
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
  filename = Rfits_gunzip(filename)
  
  out = Cfits_get_chksum(filename=filename)
  names(out) = c('DATASUM', 'CHECKSUM')
  return(out)
}

Rfits_nhdu = function(filename='temp.fits'){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  filename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  assertAccess(filename, access='r')
  filename = Rfits_gunzip(filename)
  
  return(Cfits_read_nhdu(filename=filename))
}

Rfits_nkey = function(filename='temp.fits', ext=1){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  filename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  assertAccess(filename, access='r')
  filename = Rfits_gunzip(filename)
  if(is.character(ext)){ext = Rfits_extname_to_ext(filename, ext)}
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
  keyvalues[hdr[c(F,T)] == 'NA'] = NA
  names(keyvalues) = keynames
  class(keyvalues) = 'Rfits_keylist'
  
  return(keyvalues)
}

Rfits_keyvalues_to_hdr = function(keyvalues){
  assertList(keyvalues)
  keyvalues = keyvalues[!is.na(keyvalues)]
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

Rfits_header_to_keyvalues=function(header, remove_HIERARCH=FALSE){
  return(Rfits_hdr_to_keyvalues(Rfits_header_to_hdr(header=header, remove_HIERARCH=remove_HIERARCH)))
}

Rfits_header_to_raw = function(header, zap=NULL, ...){
  header = Rfits_header_zap(header=header, zap=zap, ...)
  return(paste(formatC(substr(header,1,79), width=80, flag='-'),sep='',collapse = ''))
}

Rfits_raw_to_header = function(raw){
  nkey = nchar(raw)/80
  return(substring(raw, 1+(80*((1:nkey)-1)), 80*(1:nkey)))
}

Rfits_raw_to_keyvalues = function(raw, remove_HIERARCH=FALSE){
  return(Rfits_header_to_keyvalues(Rfits_raw_to_header(raw), remove_HIERARCH=FALSE))
}

Rfits_keyvalues_to_raw = function(keyvalues, keycomments=NULL, comment=NULL, history=NULL, zap=NULL, ...){
  return(Rfits_header_to_raw(
    Rfits_keyvalues_to_header(
      keyvalues = keyvalues,
      keycomments = keycomments,
      comment = comment,
      history = history
    ),
    zap = zap,
    ...
  )
  )
}

Rfits_header_zap=function(header, zap=NULL, ...){
  if(!is.null(zap)){
    for(i in 1:length(zap)){
      zaplocs = grep(zap[i], header, ...)
      if(length(zaplocs) > 0){
        header = header[-zaplocs]
      }
    }
  }
  return(header)
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

Rfits_key_scan = function(filelist=NULL, dirlist=NULL, image_list=NULL, keylist=NULL, extlist=1, pattern=NULL,
                          recursive=TRUE, fileinfo='All', keep_ext=TRUE, cores=1, get_length=FALSE,
                          get_dim=FALSE, get_centre=FALSE, get_rotation=FALSE, get_corners=FALSE, get_extremes=FALSE,
                          get_pixscale=FALSE, get_pixarea=FALSE, get_all=FALSE, remove_HIERARCH=FALSE, 
                          keypass=FALSE, zap=NULL, data.table=TRUE, ...){
  
  registerDoParallel(cores=cores)
  
  if(is.null(image_list)){
    if(is.null(filelist)){
      if(is.null(dirlist)){
        stop('Missing filelist and dirlist')
      }
      for(i in 1:length(dirlist)){
        filelist = c(filelist,
                     list.files(dirlist[i], full.names=TRUE, recursive=recursive))
      }
    }
    
    filelist = normalizePath(filelist)
    if(!is.null(pattern)){
      for(i in pattern){
        filelist = grep(pattern=i, filelist, value=TRUE)
      }
    }
    filelist = grep(pattern='.fits$', filelist, value=TRUE)
    filelist = unique(filelist)
    
    Nscan = length(filelist)
  }else{
    Nscan = length(image_list)
    filelist = foreach(i = 1:Nscan, .combine='c')%dopar%{
      image_list[[i]]$filename
    }
  }
  
  if(length(extlist) == 1){
    extlist = rep(extlist, Nscan)
  }
  
  if(length(keylist) > 0){
    obs_info = data.frame()
    
    obs_info = foreach(i = 1:Nscan, .combine='rbind')%dopar%{
      current_info = list()
      for(key in keylist){
          if(is.null(image_list)){
            keyval = Rfits_read_key(filename=filelist[i], keyname=key, keytype='auto', ext=extlist[i])
          }else{
            keyval = image_list[[i]]$keyvalues[[key]]
          }
        current_info = c(current_info, key=keyval)
      }
      return(as.data.frame(current_info))
    }
    
    colnames(obs_info) = keylist
  }else{
    obs_info = NULL
  }
  
  if(get_all){
    get_length = TRUE
    get_dim = TRUE
    get_centre = TRUE
    get_rotation = TRUE
    get_corners = TRUE
    get_extremes = TRUE
    get_pixscale = TRUE
    get_pixarea = TRUE
  }
  
  if(any(get_length, get_dim, get_centre, get_corners, get_pixscale, get_pixarea)){
    method_info = foreach(i = 1:Nscan, .combine='rbind')%dopar%{
      
      suppressMessages({
        if(is.null(image_list)){
          temp_keyvalues = Rfits_read_header(filename=filelist[i], ext=extlist[i], remove_HIERARCH=remove_HIERARCH, keypass=keypass, zap=zap)$keyvalues
        }else{
          temp_keyvalues = image_list[[i]]$keyvalues
        }
        
        current_info = list()
        
        if(get_length){
          temp_length = length(temp_keyvalues)
          if(is.null(temp_length)){
            temp_length = NA
          }
          current_info = c(current_info, length = temp_length)
        }
        
        if(get_dim){
          temp_dim = dim(temp_keyvalues)
          if(is.null(temp_dim[1])){
            temp_dim = rep(NA, 2)
          }
          current_info = c(current_info,
                           dim_1 = temp_dim[1],
                           dim_2 = temp_dim[2],
                           dim_3 = temp_dim[3],
                           dim_4 = temp_dim[4]
                           )
        }
        
        if(get_centre){
          temp_cen = centre(temp_keyvalues, ...)
          if(is.na(temp_cen[1])){
            temp_cen = rep(NA, 2)
          }
          current_info = c(current_info,
                           centre_RA = temp_cen[1], centre_Dec = temp_cen[2]
                           )
        }
        
        if(get_rotation){
          temp_rot = rotation(temp_keyvalues)
          if(is.na(temp_rot[1])){
            temp_rot = rep(NA, 2)
          }
          current_info = c(current_info,
                           rotation_North = temp_rot[1], rotation_East = temp_rot[2]
          )
        }
        
        if(get_corners){
          temp_cor = corners(temp_keyvalues, ...)
          if(is.na(temp_cor[1])){
            temp_cor = matrix(NA, 4, 2)
          }
          current_info = c(current_info,
                           corner_BL_RA = temp_cor[1,1], corner_BL_Dec = temp_cor[1,2],
                           corner_TL_RA = temp_cor[2,1], corner_TL_Dec = temp_cor[2,2],
                           corner_TR_RA = temp_cor[3,1], corner_TR_Dec = temp_cor[3,2],
                           corner_BR_RA = temp_cor[4,1], corner_BR_Dec = temp_cor[4,2]
                           )
        }
        
        if(get_extremes){
          temp_ext = extremes(temp_keyvalues, ...)
          if(is.na(temp_ext[1])){
            temp_ext = matrix(NA, 3, 2)
          }
          current_info = c(current_info,
                           min_RA = temp_ext[1,1], min_Dec = temp_ext[1,2],
                           max_RA = temp_ext[2,1], max_Dec = temp_ext[2,2],
                           range_RA = temp_ext[3,1], range_Dec = temp_ext[3,2]
          )
        }
        
        if(get_pixscale){
          temp_pixscale = pixscale(temp_keyvalues, ...)
          current_info = c(current_info, pixscale = temp_pixscale)
        }
        
        if(get_pixarea){
          temp_pixarea = pixarea(temp_keyvalues, ...)
          current_info = c(current_info, pixarea = temp_pixarea)
        }
        return(as.data.frame(current_info))
      })
    }
    
    method_info = as.data.frame(method_info)
  }else{
    method_info = NULL
  }
  
  output_info = NULL
  
  if(!is.null(obs_info)){
    output_info = obs_info
  }
  
  if(!is.null(method_info)){
    if(is.null(output_info)){
      output_info = method_info
    }else{
      output_info = cbind(output_info, method_info)
    }
  }
  
  file = basename(filelist)
  stub = gsub('.fits$','',file)
  path = dirname(filelist)
  
  fileinfo = tolower(fileinfo)
  colname_fileinfo = NULL
  
  if(keep_ext){
    colname_fileinfo = c('ext', colname_fileinfo)
    if(is.null(output_info)){
      output_info = as.data.frame(extlist)
    }else{
      output_info = cbind(extlist, output_info)
    }
  }
  if('path' %in% fileinfo | 'all' %in% fileinfo){
    colname_fileinfo = c('path', colname_fileinfo)
    if(is.null(output_info)){
      output_info = as.data.frame(path)
    }else{
      output_info = cbind(path, output_info)
    }
  }
  if('stub' %in% fileinfo | 'all' %in% fileinfo){
    colname_fileinfo = c('stub', colname_fileinfo)
    if(is.null(output_info)){
      output_info = as.data.frame(stub)
    }else{
      output_info = cbind(stub, output_info)
    }
  }
  if('file' %in% fileinfo | 'all' %in% fileinfo){
    colname_fileinfo = c('file', colname_fileinfo)
    if(is.null(output_info)){
      output_info = as.data.frame(file)
    }else{
      output_info = cbind(file, output_info)
    }
  }
  if('full' %in% fileinfo | 'all' %in% fileinfo){
    colname_fileinfo = c('full', colname_fileinfo)
    if(is.null(output_info)){
      output_info = as.data.frame(filelist)
    }else{
      output_info = cbind(filelist, output_info)
    }
  }
  
  if(!is.null(colname_fileinfo)){
    colnames(output_info)[1:length(colname_fileinfo)] = colname_fileinfo
  }
  
  if(data.table){
    data.table::setDT(output_info)
  }
  
  return(invisible(output_info))
}

Rfits_extnames = function(filename='temp.fits'){
  Nhdu = Rfits_nhdu(filename)
  
  extnames = {}
  
  for(i in 1:Nhdu){
    temp_extname = Rfits_read_key(filename, keyname = 'EXTNAME', keytype='string', ext=i)
    if(length(temp_extname) == 0 | inherits(temp_extname, 'try-error')){
      temp_extname = NA_character_
    }
    extnames = c(extnames, temp_extname)
  }
  return(extnames)
}

Rfits_extname_to_ext = function(filename='temp.fits', extname=''){
  extnames = Rfits_extnames(filename)
  loc = which(extnames == extname)
  if(length(loc) == 0){
    loc = NA
  }
  return(loc)
}

Rfits_key_match = function(keyvalues_test, keyvalues_ref, check = 'both', ignore = 'NULL',
                          verbose = FALSE){
  keynames_test = names(keyvalues_test)
  keynames_ref = names(keyvalues_ref)
  
  if(check[1] == 'both'){
    keynames_check = keynames_ref[keynames_ref %in% keynames_test]
  }else if(check[1] == 'ref'){
    keynames_check = keynames_ref
  }else if(check[1] == 'test'){
    keynames_check = keynames_test
  }else if(check[1] == 'all'){
    keynames_check = unique(keynames_test, keyvalues_ref)
  }else{
    keynames_check = check
  }
  
  if(!is.null(ignore)){
    keynames_check = keynames_check[!keynames_check %in% ignore]
  }
  
  i = NULL
  
  test_out = foreach(i = 1:length(keynames_check), .combine='c')%do%{
    isTRUE(keyvalues_test[[keynames_check[i]]] == keyvalues_ref[[keynames_check[i]]])
  }
  
  if(verbose){
    if(any(test_out)){
      message('Equal: ', paste(keynames_check[test_out], collapse = ' '))
    }
    
    if(any(!test_out)){
      message('Diff: ', paste(keynames_check[!test_out], collapse = ' '))
    }
    
    if(!all(keynames_check %in% keynames_test)){
      message('Missing in test: ', paste(keynames_check[!(keynames_check %in% keynames_test)], collapse = ' '))
    }
    
    if(!all(keynames_check %in% keynames_ref)){
      message('Missing in ref: ', paste(keynames_check[!(keynames_check %in% keynames_ref)], collapse = ' '))
    }
  }
  
  return(all(test_out))
}
