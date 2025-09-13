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

Rfits_read_table=function(filename='temp.fits', ext=2, data.table=TRUE, cols=NULL, verbose=FALSE,
                          header=FALSE, remove_HIERARCH=FALSE, startrow=1L, nrow=0L, zap=NULL, zaptype='full', cores=1){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  filename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  assertAccess(filename, access='r')
  filename = Rfits_gunzip(filename)
  if(is.character(ext)){ext = Rfits_extname_to_ext(filename, ext)}
  assertIntegerish(ext, len=1)
  assertFlag(data.table)
  
  ncol = Cfits_read_ncol(filename=filename, ext=ext)
  colnames = Cfits_read_colname(filename=filename, ext=ext)
  
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
  
  assertInt(startrow, lower = 1L)

  if (!is.null(cores)) {
    if(length(cols) < cores){
      cores = length(cols)
    }
    registerDoParallel(cores=cores)
  }
  
  if(!getDoParRegistered()){
    output = list()
    count = 1
    for(i in cols){
      if(verbose){
        message("Reading column: ",colnames[count],", which is ",count," of ", length(cols))
      }
      try({
        output[[count]] = Cfits_read_col(filename=filename, colref=i, ext=ext, startrow=startrow, nrow=nrow)
      })
      if(is.null(output[count][[1]])){
        output[[count]] = NA
      }
      count = count + 1
    }
  }else{
    output = foreach(i = cols)%dopar%{
      temp = Cfits_read_col(filename=filename, colref=i, ext=ext, startrow=startrow, nrow=nrow)
      if(is.null(temp)){
        return(NA)
      }else{
        return(temp)
      }
    }
  }
  
  if(data.table){
    data.table::setDT(output)
  }else{
    output = as.data.frame(output)
  }
  
  colnames(output) = colnames
  
  if(header){
    hdr = Rfits_read_header(filename=filename, ext=ext, remove_HIERARCH=remove_HIERARCH, zap=zap, zaptype=zaptype)
    
    meta_col = try(.header_to_meta_col(output, hdr$keyvalues))
    if(inherits(meta_col, "try-error")){
      meta_col = NULL
    }
    
    attributes(output) = c(attributes(output), 
                           hdr,
                           meta_col = list(meta_col),
                           filename = filename,
                           ext = ext,
                           extname = hdr$keyvalues$EXTNAME
                           )
    
    class(output) = c('Rfits_table', class(output))
  }
  
  return(invisible(output))
}

Rfits_read_colnames=function(filename='temp.fits', ext=2){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  filename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  assertAccess(filename, access='r')
  filename = Rfits_gunzip(filename)
  if(is.character(ext)){ext = Rfits_extname_to_ext(filename, ext)}
  assertIntegerish(ext, len=1)
  
  colnames=Cfits_read_colname(filename=filename, ext=ext)
  return(colnames)
}

Rfits_write_table=function(table, filename='temp.fits', ext=2, extname='Main', tforms='auto', tunits=rep('\01', dim(table)[2]), 
                           tadd=NULL, create_ext=TRUE, create_file=TRUE, overwrite_file=TRUE, table_type='binary', 
                           NA_replace=-999, NaN_replace=-9999, Inf_replace=-99999, verbose = FALSE){
  assertDataFrame(table, min.rows = 1, min.cols = 1)
  
  nrow=dim(table)[1]
  ncol=dim(table)[2]
  
  assertFlag(create_ext)
  assertFlag(create_file)
  assertFlag(overwrite_file)
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  filename = strsplit(filename, '[compress', fixed=TRUE)[[1]][1]
  if(create_file){
    assertPathForOutput(filename, overwrite=overwrite_file)
  }else{
    assertFileExists(filename)
    assertAccess(filename, access='w')
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
  
  ttypes = colnames(table)
  
  check.logical = sapply(table,is.logical)
  check.int = sapply(table,is.integer)
  check.integer64 = sapply(table,is.integer64)
  check.double = sapply(table,is.numeric) & (! check.int) & (! check.integer64)
  check.char = sapply(table,is.character)
  
  if(any(check.logical)){
    for(i in which(check.logical)){
      table[,i] = as.integer(table[,i])
    }
  }
  
  if(inherits(table, 'Rfits_table') & tforms[1]=='get'){
    TFORMsel = grep('TFORM', attributes(table)$keynames)
    if(length(TFORMsel)>0){
      tforms = attributes(table)$keyvalues[TFORMsel]
    }
  }
  
  if(inherits(table, 'Rfits_table') & tunits[1]=='get'){
    TUNITsel = grep('TUNIT', attributes(table)$keynames)
    if(length(TUNITsel)>0){
      tunits = attributes(table)$keyvalues[TUNITsel]
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
    
    if(length(grep('1B|1K|1J|1D|1E|1I|A',tforms)) != ncol){
      stop(cat('Unrecognised column data type in column', paste(which(!1:ncol %in% grep('1B|1K|1J|1D|1E|1I|A',tforms))),sep='\n'))
    }
  }
  
  typecode = rep(0, ncol)
  typecode[check.logical]=31
  typecode[check.int]=31
  typecode[check.integer64]=81
  typecode[check.double]=82
  typecode[check.char]=16
    
  assertCharacter(ttypes, len=ncol)
  assertCharacter(tforms, len=ncol)
  assertCharacter(tunits, len=ncol)
  
  Cfits_create_bintable(filename=filename, tfields=ncol, ttypes=ttypes, tforms=tforms,
                        tunits=tunits, extname=extname, ext=ext, create_ext=create_ext,
                        create_file=create_file, table_type=table_type)
  ext = Cfits_read_nhdu(filename=filename)
  if(!is.null(tadd)){
    keynames=names(tadd)
    for(i in 1:length(tadd)){
      Rfits_write_key(filename=filename, keyname=keynames[i], keyvalue=tadd[[i]], keycomment='', ext=ext)
    }
  }else{
    if(inherits(table, 'Rfits_table')){
      TSCALsel = grep('TSCAL', attributes(table)$keynames)
      TZEROsel = grep('TZERO', attributes(table)$keynames)
      if(length(TSCALsel)>0){
        for(i in TSCALsel){
          Rfits_write_key(filename=filename, keyname=attributes(table)$keynames[i], keyvalue=attributes(table)$keyvalues[i], keycomment='', ext=ext)
        }
      }
      if(length(TZEROsel)>0){
        for(i in TZEROsel){
          Rfits_write_key(filename=filename, keyname=attributes(table)$keynames[i], keyvalue=attributes(table)$keyvalues[i], keycomment='', ext=ext)
        }
      }
    }
  }
  
  for(i in 1:ncol){
    if(verbose){
      message("Writing column: ",ttypes[i],", which is ",i," of ", ncol)
    }
    if(anyNA(table[[i]])){
      table[[i]][is.na(table[[i]])] = NA_replace
    }
    if(anyNaN(table[[i]])){
      table[[i]][is.nan(table[[i]])] = NaN_replace
    }
    if(anyInfinite(table[[i]])){
      table[[i]][is.infinite(table[[i]])] = Inf_replace
    }
    Cfits_write_col(filename=filename, data=table[[i]], nrow=nrow, colref=i, ext=ext, typecode=typecode[i])
  }
}

.header_to_meta_col = function(table, keyvalues){
  
  keynames = names(keyvalues)
  field_name = unlist(keyvalues[grep('TTYPE', keynames)])
  Nfield = length(field_name)
  
  #field_unit = unlist(keyvalues[grep('TUNIT', keynames)])
  field_unit = rep(NA_character_, Nfield)
  field_sel = grep('TUNIT', keynames)
  subset = as.integer(unlist(strsplit(keynames[field_sel], 'TUNIT'))[c(F,T)])
  field_unit[subset] = unlist(keyvalues[field_sel])
  
  #field_descrip = unlist(keyvalues[grep('TCOMM', keynames)])
  field_descrip = rep(NA_character_, Nfield)
  field_sel = grep('TCOMM', keynames)
  subset = as.integer(unlist(strsplit(keynames[field_sel], 'TCOMM'))[c(F,T)])
  field_descrip[subset] = unlist(keyvalues[field_sel])
  
  #field_ucd = unlist(keyvalues[grep('TUCD', keynames)])
  field_ucd = rep(NA_character_, Nfield)
  field_sel = grep('TUCD', keynames)
  subset = as.integer(unlist(strsplit(keynames[field_sel], 'TUCD'))[c(F,T)])
  field_ucd[subset] = unlist(keyvalues[field_sel])
  
  field_datatype = foreach(i = 1:ncol(table), .combine='c')%do%{
    switch(class(table[[i]])[1],
    integer = "int",
    integer64 = "long",
    numeric = "double",
    character = "char",
    "char")
  }
  
  #field_form = unlist(keyvalues[grep('TFORM', keynames)])
  field_form = rep(NA_character_, Nfield)
  field_sel = grep('TFORM', keynames)
  subset = as.integer(unlist(strsplit(keynames[field_sel], 'TFORM'))[c(F,T)])
  field_form[subset] = unlist(keyvalues[field_sel])
  
  field_arraysize = rep(NA_integer_, Nfield)
  for(i in which(field_datatype == 'char')){
    field_arraysize[i] = as.integer(strsplit(field_form[[i]], 'A')[[1]])
  }
  
  meta_col = data.frame(Name = field_name,
                        Units = field_unit,
                        Description = field_descrip,
                        UCD = field_ucd,
                        Datatype = field_datatype,
                        Arraysize = field_arraysize)
  
  return(meta_col)
}
