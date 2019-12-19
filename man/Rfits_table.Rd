\name{Rfits_table}
\alias{Rfits_table}
\alias{Rfits_read_table}
\alias{Rfits_read_colnames}
\alias{Rfits_write_table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FITS Table Readers and Writers
}
\description{
Generic table readers and writers. Works with a wide range of data types including 64 bit integers (long long).
}
\usage{
Rfits_read_table(filename, ext = 2, data.table = TRUE, cols = NULL, verbose = FALSE)

Rfits_read_colnames(filename, ext = 2)

Rfits_write_table(table, filename, ext = 2, extname = 'Main',
  tunits = rep('\01', dim(table)[2]), create_ext = TRUE, create_file = TRUE,
  overwrite_file = TRUE, table_type = 'binary', NA_replace = -999, NaN_replace = -9999,
  Inf_replace = -99999, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
Character scalar; path to FITS file, either to be written to or read.
}
  \item{ext}{
Integer scalar; the extension to read or write (usually starts at 2 for tables). For writing, what really happens is the specified extension is deleted (if \option{create_ext} = FALSE), all current extensions shuffle forward one place, and a new extension is appended at the end of the file. This means unless the target extension is already at the end of the FITS file, the extension order will change. This is how cfitsio works with extensions (not a \code{Rfits} design decision). If the order is really important then you will need to construct a new FITS file from scratch and build it in the order desired.
}
  \item{table}{
Data.frame or data.table; data to be written out as a binary FITS table.
}
  \item{cols}{
Vector; either a vector of integer column numbers to be extracted from the target FITS table, or a vector of column names (all must be present in the target FITS table).  
}
  \item{verbose}{
Logical; should the progress of table reading and writing be displayed? This is probably only useful for very large (many GB) that might take a long time to read or write.
}
  \item{extname}{
Character scalar; name of the table extension. 
}
  \item{tunits}{
Character vector; data units to be written out. Default is NULL. 
}
  \item{create_ext}{
Logical; should a new extension be created (TRUE) or not (FALSE). This will always be appended at the end of the FITS file (as per the FITS standard). If \option{create_ext} = TRUE then no matter what the \option{ext} argument is, the extension will be created at the end of the FITS file. If \option{create_ext} = FALSE then the extension specified by \option{ext} will be first deleted, and then the extension will be created at the end of the FITS file.
}
  \item{create_file}{
Logical; should a new file be created (TRUE) or not (FALSE). If not then the file at \option{filename} must exist for extensions to be written to it. This is really here to ensure safe operation, i.e. if \option{create_file}=TRUE and \option{overwrite_file}=FALSE and there is a file already at the path given by \option{filename} then you will get an error.
}
  \item{overwrite_file}{
Logical; if file exists at location \option{filename}, should it be overwritten (i.e. deleted and a fresh FITS file created)?
}
  \item{data.table}{
Logical; should a \code{data.table} be returned?
}
  \item{table_type}{
Character scalar; type of table to be written. Must be one of 'binary' (default) or 'ascii'.  
}
  \item{NA_replace}{
Integer scalar; value to replace NA (since FITS does not support NA). If numeric, it will be converted to integer internally.  
}
  \item{NaN_replace}{
Integer scalar; value to replace NaN (since FITS does not support NaN). If numeric, it will be converted to integer internally.  
}
  \item{Inf_replace}{
Integer scalar; value to replace Inf (since FITS does not support Inf). If numeric, it will be converted to integer internally.  
}
}
\details{
R FITS table readers and writers that uses CFITSIO to do the interfacing with files. Requires \code{bit64} package for integer64 (i.e. long long) to be work.

\code{Rfits_read_table} reads in tabular data from a FITS file.

\code{Rfits_write_table_} writes tabular data (data.frames and data.tables) to a FITS file. Note that when writing extensions are always added onto the end of the current FITS file. If doing this you should use settings \option{overwrite_file} = FALSE (we do not want to delete the current file), \option{create_file} = FALSE (we do not want to create a new file), \option{create_ext} = FALSE (we do not want to simply append an extension to the end of the current file), \option{ext} = N (we want to delete extension N, and add a new one to the end of the file).
}
\references{
\url{https://heasarc.gsfc.nasa.gov/docs/software/fitsio/c/c_user/cfitsio.html}
}
\author{
Aaron Robotham
Rodrigo Tobar
}

\seealso{
\code{\link{Rfits_header}}, \code{\link{Rfits_image}}, \code{FITSio}, \code{astro}
}
\examples{
#Read table
file_table = system.file('extdata', 'table.fits', package = "Rfits")
temp_table = Rfits_read_table(file_table)
temp_table[1:5,]

#Check write and read consistency
file_table_temp = tempfile()
Rfits_write_table(temp_table, file_table_temp)
temp_table2 = Rfits_read_table(file_table_temp)
cols_check = which(sapply(temp_table[1,], is.numeric))
sum(temp_table[,..cols_check] - temp_table2[,..cols_check])

#64 bit integer
library(bit64)
#The OBJID column requires integer64:
is.integer64(temp_table$OBJID)
#This cannot even be stored accurately in a numeric double:
suppressWarnings(print(as.numeric(temp_table$OBJID[1:5]), digits = 20))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\concept{ FITS }% use one of  RShowDoc("KEYWORDS")
