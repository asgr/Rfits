\name{Rfits}
\alias{Rfits}
\alias{Rfits_table}
\alias{Rfits_read_table}
\alias{Rfits_write_table}
\alias{Rfits_read_image}
\alias{Rfits_write_image}
\alias{Rfits_read_key}
\alias{Rfits_write_key}
\alias{Rfits_delete_key}
\alias{Rfits_read_header}
\alias{Rfits_write_header}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FITS Readers and Writers
}
\description{
Generic header key, image and table readers and writers. Works with a wide range of data types including 64 bit integers (long long).
}
\usage{
Rfits_read_table(filename, ext = 2, data.table = TRUE)

Rfits_write_table(table, filename, extname = 'Main', tunits = rep('\01', dim(table)[2]),
overwrite = TRUE)

Rfits_read_image(filename, ext = 1, header = TRUE)

Rfits_write_image(filename, image, overwrite = TRUE)

Rfits_read_key(filename, keyname, keytype = 'numeric', ext = 1)

Rfits_write_key(filename, keyname, keyvalue, comment = "", ext = 1)

Rfits_delete_key(filename, keyname, ext = 1)

Rfits_read_header(filename, ext = 1)

Rfits_write_header(filename, keyvalues, comments, keynames, ext=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
Character scalar; path to FITS file.
}
  \item{table}{
Data.frame or data.table; data to be written out as a binary FITS table.
}
  \item{image}{
Numeric or integer matrix; data to be written out as a FITS image.
}
  \item{header}{
Logical; should the header be read along with the image matrix. If TRUE (default) then the output of \option{Rfits_write_image} is a list with components \option{imDat}, and the header list outputs created by \option{Rfits_read_header} (all at the same level), if FALSE then it is just the raw image matrix.
}
  \item{keyname}{
Character scalar; keyname in FITS header to be updated, added (if not present), or deleted.  
}
  \item{keynames}{
Character list; (not required) keynames in FITS header to be updated or added (if not present). If not supplied then the keynames will be taken from the names of \option{keyvalues}.
}
  \item{keytype}{
Character scalar; type of key to be read in, either "numeric" or "string". 
}
  \item{keyvalue}{
Scalar; value to be written (updated or added) into header (can be any type, e.g. numeric, integer, string).
}
  \item{keyvalues}{
List; values to be written (updated or added) into header (can be any type, e.g. numeric, integer, string).
}
  \item{comment}{
Character scalar; (not required) comment associated with updates or added \option{keyname}.  
}
  \item{comments}{
Character list; (not required) comments associated with updates or added \option{keynames}.  
}
  \item{ext}{
Integer scalar; the extension to use (usually starts at 1 for images and 2 for tables).
}
  \item{extname}{
Character scalar; name of the table extension. 
}
  \item{tunits}{
Character vector; data units to be written out. Default is NULL. 
}
  \item{overwrite}{
Logical; if file exists at location \option{filename}, should it be overwritten?
}
  \item{data.table}{
Logical; should a \code{data.table} be returned?
}
}
\details{
R FITS readers and writers that uses CFITSIO to do the interfacing with files. Requires \code{bit64} package for integer64 (i.e. long long) to be work.

\code{Rfits_read_} reads in data from a FITS file.

\code{Rfits_write_} writes data to a FITS file.

\code{table} functions work with binary FITS tables and R data.frames and data.tables. Currently when writing out the only option is to create a new file (cannot append to existing FITS). This may change in the future.

\code{image} functions work with binary FITS images and R numeric or integer matrices. When reading, if \option{header}=TRUE (default) then the output of \option{Rfits_write_image} is a list with components \option{imDat}, and the header list outputs created by \option{Rfits_read_header} (all at the same level), if \option{header}=FALSE then it is just the raw image matrix. Currently when writing out the only option is to create a new file (cannot append to existing FITS). This may change in the future.

\code{key} functions work with any FITS header and single keyname inputs and outputs (there is not a full header read yet). Note when writing, if key is already present it will be replaced, if not present the new key will be added at the end of the header.

\code{header} functions work with any FITS header, reading and writing them in their entirety.
}
\value{
\code{Rfits_write_} functions just have the side effect of writing data out.

\code{Rfits_delete_key} deletes the specified \option{keyname}.

\code{Rfits_read_table}: data.frame or data.table.

\code{Rfits_read_image}: numeric or integer matrix.

\code{Rfits_read_key}: scalar numeric or string.

\code{Rfits_read_header}: list containing the unformatted header string vector (\option{header}), the keyname / keyvalue stripped version (\option{hdr}), the named \option{keyvalues} list, the named \option{comments} list, and the vector of non-comment keynames (\option{keynames}).
}
\references{
\url{https://heasarc.gsfc.nasa.gov/docs/software/fitsio/c/c_user/cfitsio.html}
}
\author{
Aaron Robotham
Rodrigo Tobar
}

\seealso{
\code{FITSio}, ~~~
}
\examples{
file_table = system.file('extdata', 'table.fits', package = "Rfits")
temp_table = Rfits_read_table(file_table)
temp_table[1:5,]

file_image=system.file('extdata', 'image.fits', package = "Rfits")
temp_image=Rfits_read_image(file_image)
Rfits_read_key(file_image, 'NAXIS1')
Rfits_read_key(file_image, 'CTYPE1', keytype = 'string')


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\concept{ FITS }% use one of  RShowDoc("KEYWORDS")
