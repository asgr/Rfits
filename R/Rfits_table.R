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

Rfits_read_table=function(filename, ext=2, data.table=TRUE){
  assertCharacter(filename, max.len=1)
  filename=path.expand(filename)
  assertAccess(filename, access='r')
  assertIntegerish(ext, len = 1)
  assertFlag(data.table)
  
  ncol=Cfits_read_ncol(filename)
  output=list()
  
  for(i in 1:ncol){
    output[[i]]=Cfits_read_col(filename,colref=i)
  }
  
  if(data.table){
    output=data.table::as.data.table(output)
    colnames(output)=Cfits_read_colname(filename, ext=ext)
  }else{
    output=as.data.frame(output)
    colnames(output)=Cfits_read_colname(filename, ext=ext)
  }
  
  return(invisible(output))
}

Rfits_write_table=function(table, filename, extname='Main', tunits=rep('\01', dim(table)[2]), overwrite_file=TRUE){
  assertDataFrame(table, min.rows = 1, min.cols = 1)
  assertCharacter(filename, max.len = 1)
  filename=path.expand(filename)
  assertPathForOutput(filename, overwrite=TRUE)
  if(testFileExists(filename) & overwrite_file){
    file.remove(filename)
  }
  assertCharacter(extname, max.len = 1)
  
  nrow=dim(table)[1]
  ncol=dim(table)[2]
  
  ttypes=colnames(table)
  
  check.int=sapply(table,is.integer)
  check.integer64=sapply(table,is.integer64)
  check.double=sapply(table,is.numeric) & (! check.int) & (! check.integer64)
  check.char=sapply(table,is.character)
  
  tforms=character(ncol)
  tforms[check.int]="1J" # will become typecode = TINT = 31
  tforms[check.integer64]='1K' # will become typecode = TLONGLONG = 81
  tforms[check.double]="1D" # will become typecode = TDOUBLE = 82
  tforms[check.char]=paste(sapply(table[,check.char],function(x) max(nchar(x))+1), 'A', sep='') # will become typecode = TSTRING = 16
  
  if(length(grep('1K|1J|1D|A',tforms)) != ncol){
    stop(paste('Unrecognised column data type in column',which(!1:ncol %in% grep('1K|1J|1D|A',tforms))))
  }
  
  typecode=rep(0, ncol)
  typecode[check.int]=31
  typecode[check.integer64]=81
  typecode[check.double]=82
  typecode[check.char]=16
    
  assertCharacter(ttypes, len = ncol)
  assertCharacter(tforms, len = ncol)
  assertCharacter(tunits, len = ncol)
  
  Cfits_create_bintable(filename, tfields=ncol, ttypes=ttypes, tforms=tforms, tunits=tunits, extname=extname)
  for(i in 1:ncol){
    Cfits_write_col(filename = filename, data = table[[i]], nrow = nrow, colref = i, typecode = typecode[i])
  }
}
