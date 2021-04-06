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
Rfits_read_table(filename = 'temp.fits', ext = 2, data.table = TRUE,
    cols = NULL, verbose = FALSE, header = FALSE, remove_HIERARCH = FALSE)

Rfits_read_colnames(filename = 'temp.fits', ext = 2)

Rfits_write_table(table, filename = 'temp.fits', ext = 2, extname = 'Main',
    tforms = 'auto', tunits = rep('\01', dim(table)[2]), tadd = NULL,
    create_ext = TRUE, create_file = TRUE, overwrite_file = TRUE,
    table_type = 'binary', NA_replace = -999, NaN_replace = -9999,
    Inf_replace = -99999, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
Character scalar; path to FITS file, either to be written to or read.
}
  \item{table}{
Data.frame or data.table; data to be written out as a binary FITS table.
}
  \item{ext}{
Integer scalar; the extension to read or write (usually starts at 2 for tables). For writing, what really happens is the specified extension is deleted (if \option{create_ext} = FALSE), all current extensions shuffle forward one place, and a new extension is appended at the end of the file. This means unless the target extension is already at the end of the FITS file, the extension order will change. This is how cfitsio works with extensions (not a \code{Rfits} design decision). If the order is really important then you will need to construct a new FITS file from scratch and build it in the order desired.
}
  \item{data.table}{
Logical; should a \code{data.table} be returned?
}
  \item{cols}{
Vector; either a vector of integer column numbers to be extracted from the target FITS table, or a vector of column names (all must be present in the target FITS table).  
}
  \item{verbose}{
Logical; should the progress of table reading and writing be displayed? This is probably only useful for very large (many GB) that might take a long time to read or write.
}
  \item{header}{
Logical; if TRUE then output gains attributes containing the header information from a call to \code{\link{Rfits_read_header}} (see Examples). Default is FALSE, so no table header information is explictly copied over. If \option{header}=TRUE is set then the output inherits an extra class of 'Rfits_table' (it keeps its 'data.frame' and 'data.table' classes as relevant though).
}
  \item{extname}{
Character scalar; name of the table extension. 
}
  \item{tforms}{
Character vector; this should usually be left at the default of 'auto', where the table writer will choose sensible values for the table formatting. If you are an advanced user who needs more control, then this must be the same length as the number of columns being written, and specify FITS standard formatting types (do some Googling to find those, but they look like I9, I20, D18.10 for ASCII \option{table_type}, and 1J, 1K and 1D for binary \option{table_type}). If set to 'get' and the table is of class 'Rfits_table' (so has header information) then the TFORM header information stored in the attributes will be used instead.
}
  \item{tunits}{
Character vector; data units to be written out. Default is NULL. If set to 'get' and the table is of class 'Rfits_table' (so has header information) then the TUNIT header information stored in the attributes will be used instead.
}
  \item{tadd}{
List; additional column specific key values that you want to add before writing out the FITS table. This is particularly designed for \option{TSCALn} and \option{TZEROn} (where n is the column number) key values that scale the column values on writing (see Examples).
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
  \item{remove_HIERARCH}{
Logical, should the leading 'HIERARCH' be removed for extended keyword names (longer than 8 characters)?  
}
}
\details{
R FITS table readers and writers that uses CFITSIO to do the interfacing with files. Requires \code{bit64} package for integer64 (i.e. long long) to be work. If \option{header}=TRUE is set then the output inherits an extra class of 'Rfits_table' (it keeps its 'data.frame' and 'data.table' classes as relevant though).
}
\note{
\code{Rfits_read_table} will apply TSCALn and TZEROn scaling, since this is applied at the CFITSIO level, so the column values will be VALn = (RAWn x TSCALn) + TZEROn, where VALn is what is seen in the R \code{data.frame} or \code{data.table} and RAWn is what you would find if inspecting the binary values stored on disk. The \code{Rfits_write_table} will only scale if you pass the TSCALn and TZEROn key values into the \option{tadd} argument (see Examples).
}
\value{
\code{Rfits_read_table} reads in tabular data from a FITS file, either as a data.frame or data.table. Also gains an attribute of type \option{info}, which is the header information from a call to \code{\link{Rfits_info}} (see examples). It is done this way so carrying the header info does not affect the data.frame or data.table class of the table (which is still just a normal table in every other respect). This meets the principle of least surprise since people nearly always just want to work with the table and rarely care about table header info (which is really there to help read in the table in CFITSIO).

\code{Rfits_write_table_} writes tabular data (data.frames and data.tables) to a FITS file. Note that when writing extensions are always added onto the end of the current FITS file. If doing this you should use settings \option{overwrite_file} = FALSE (we do not want to delete the current file), \option{create_file} = FALSE (we do not want to create a new file), \option{create_ext} = FALSE (we do not want to simply append an extension to the end of the current file), \option{ext} = N (we want to delete extension N, and add a new one to the end of the file).
}
\references{
\url{https://heasarc.gsfc.nasa.gov/docs/software/fitsio/c/c_user/cfitsio.html}
}
\author{
Aaron Robotham,
Rodrigo Tobar
}

\seealso{
\code{\link{Rfits_header}}, \code{\link{Rfits_image}}, \code{FITSio}, \code{astro}
}
\examples{
#Read table
file_table = system.file('extdata', 'table.fits', package = "Rfits")
temp_table = Rfits_read_table(file_table, header=TRUE)
temp_table[1:5,]

#Extra header attributes (does not affect the data.frame or data.table class)
attributes(temp_table)$header[1:5]

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

#Use TSCALn and TZEROn
file_table_temp = tempfile()
Rfits_write_table(temp_table, file_table_temp, tadd=list(TSCAL6=2, TZERO6=10))
temp_table2 = Rfits_read_table(file_table_temp)
sum(temp_table[,..cols_check] - temp_table2[,..cols_check])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\concept{ FITS }% use one of  RShowDoc("KEYWORDS")
