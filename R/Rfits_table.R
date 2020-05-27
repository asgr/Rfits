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

Rfits_read_table=function(filename, ext=2, data.table=TRUE, cols=NULL, verbose=FALSE,
                          header=FALSE, remove_HIERARCH=FALSE){
  assertCharacter(filename, max.len=1)
  filename=path.expand(filename)
  assertAccess(filename, access='r')
  assertIntegerish(ext, len=1)
  assertFlag(data.table)
  
  ncol = Cfits_read_ncol(filename, ext=ext)
  colnames = Cfits_read_colname(filename, ext=ext)
  
  if(! is.null(cols)){
    if(is.character(cols)){
      assertCharacter(cols)
      cols = match(cols, colnames)
    }
    assertIntegerish(cols, any.missing = FALSE)
    colnames = colnames[cols]
  }else{
    cols = 1:ncol
  }
  
  assertFlag(verbose)
  assertFlag(header)
  assertFlag(remove_HIERARCH)

  output=list()
  count=1
  
  for(i in cols){
    if(verbose){
      message("Reading column: ",colnames[count],", which is ",count," of ", length(cols))
    }
    output[[count]]=Cfits_read_col(filename,colref=i,ext=ext)
    count = count + 1
  }
  
  if(data.table){
    output=data.table::as.data.table(output)
    
  }else{
    output=as.data.frame(output)
  }
  
  colnames(output) = colnames
  
  if(header){
    attributes(output)$info = Rfits_info(filename, remove_HIERARCH = remove_HIERARCH)
  }
  
  return(invisible(output))
}

Rfits_read_colnames=function(filename, ext=2){
  assertCharacter(filename, max.len=1)
  filename=path.expand(filename)
  assertAccess(filename, access='r')
  assertIntegerish(ext, len=1)
  
  colnames=Cfits_read_colname(filename, ext=ext)
  return(colnames)
}

Rfits_write_table=function(table, filename, ext=2, extname='Main', tunits=rep('\01', dim(table)[2]), 
                           tforms='auto', tadd=NULL, create_ext=TRUE, create_file=TRUE, overwrite_file=TRUE, table_type='binary', 
                           NA_replace=-999, NaN_replace=-9999, Inf_replace=-99999, verbose = FALSE){
  assertDataFrame(table, min.rows = 1, min.cols = 1)
  
  nrow=dim(table)[1]
  ncol=dim(table)[2]
  
  assertFlag(create_ext)
  assertFlag(create_file)
  assertFlag(overwrite_file)
  assertCharacter(filename, max.len=1)
  filename=path.expand(filename)
  if(create_file){
    assertPathForOutput(filename, overwrite=overwrite_file)
  }else{
    assertFileExists(filename)
  }
  if(testFileExists(filename) & overwrite_file & create_file){
    file.remove(filename)
  }
  assertCharacter(extname, max.len=1)
  assertCharacter(tunits, len=ncol)
  assert(testCharacter(tforms, len=1) | testCharacter(tforms, len=ncol))
  assertCharacter(table_type, len=1)
  table_type = tolower(table_type)
  assertSubset(table_type, c('binary','ascii'))
  assertNumeric(NA_replace, len=1)
  assertNumeric(NaN_replace, len=1)
  assertNumeric(Inf_replace, len=1)
  assertFlag(verbose)
  
  NA_replace = as.integer(NA_replace)
  NaN_replace = as.integer(NaN_replace)
  Inf_replace = as.integer(Inf_replace)
  
  ttypes=colnames(table)
  
  check.logical=sapply(table,is.logical)
  check.int=sapply(table,is.integer)
  check.integer64=sapply(table,is.integer64)
  check.double=sapply(table,is.numeric) & (! check.int) & (! check.integer64)
  check.char=sapply(table,is.character)
  
  if(any(check.logical)){
    for(i in which(check.logical)){
      table[,i] = as.integer(table[,i])
    }
  }
  
  if(table_type == 'ascii'){
    table_type = 1
    
    if(tforms[1] == 'auto'){
      tforms=character(ncol)
      tforms[check.logical]="I9"
      tforms[check.int]="I9"
      tforms[check.integer64]='I20'
      tforms[check.double]="D18.10"
      tforms[check.char]=paste('A', sapply(table[,check.char,drop=FALSE],function(x) max(nchar(x))+1), sep='')
    }
    if(length(grep('I|D|A',tforms)) != ncol){
      stop(cat('Unrecognised column data type in column', paste(which(!1:ncol %in% grep('I|D|A',tforms))),sep='\n'))
    }
  }else if(table_type == 'binary'){
    table_type = 2
    
    if(tforms[1] == 'auto'){
      tforms=character(ncol)
      tforms[check.logical]="1J"
      tforms[check.int]="1J" # will become typecode = TINT = 31
      tforms[check.integer64]='1K' # will become typecode = TLONGLONG = 81
      tforms[check.double]="1D" # will become typecode = TDOUBLE = 82
      tforms[check.char]=paste(sapply(table[,check.char,drop=FALSE],function(x) max(nchar(x))+1), 'A', sep='') # will become typecode = TSTRING = 16
    }
    
    if(length(grep('1B|1K|1J|1D|A',tforms)) != ncol){
      stop(cat('Unrecognised column data type in column', paste(which(!1:ncol %in% grep('1B|1K|1J|1D|A',tforms))),sep='\n'))
    }
  }
  
  typecode=rep(0, ncol)
  typecode[check.logical]=31
  typecode[check.int]=31
  typecode[check.integer64]=81
  typecode[check.double]=82
  typecode[check.char]=16
    
  assertCharacter(ttypes, len=ncol)
  assertCharacter(tforms, len=ncol)
  assertCharacter(tunits, len=ncol)
  
  Cfits_create_bintable(filename, tfields=ncol, ttypes=ttypes, tforms=tforms,
                        tunits=tunits, extname=extname, ext=ext, create_ext=create_ext,
                        create_file=create_file, table_type=table_type)
  ext = Cfits_read_nhdu(filename)
  if(!is.null(tadd)){
    keynames=names(tadd)
    for(i in 1:length(tadd)){
      Rfits_write_key(filename=filename, keyname=keynames[i], keyvalue=tadd[[i]], keycomment='', ext=ext)
    }
  }
  for(i in 1:ncol){
    if(verbose){
      message("Writing column: ",ttypes[i],", which is ",i," of ", ncol)
    }
    table[[i]][is.na(table[[i]])] = NA_replace
    table[[i]][is.nan(table[[i]])] = NaN_replace
    table[[i]][is.infinite(table[[i]])] = Inf_replace
    Cfits_write_col(filename = filename, data = table[[i]], nrow = nrow, colref = i, ext = ext, typecode = typecode[i])
  }
}
