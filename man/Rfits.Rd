\name{Rfits}
\alias{Rfits}
\alias{Rfits_table}
\alias{Rfits_read_table}
\alias{Rfits_write_table}
\alias{Rfits_read_image}
\alias{Rfits_write_image}
\alias{Rfits_read_header}
\alias{Rfits_write_header}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FITS Readers and Writers
}
\description{
Generic header, image and table readers and writers. Works with a wide range of data types including 64 bit integers (long long).
}
\usage{
Rfits_read_table(filename, ext = 2, data.table = TRUE)

Rfits_write_table(table, filename, extname = 'Main', tunits = rep('\01', dim(table)[2]),
overwrite = TRUE)

Rfits_read_image(filename, ext=1)

Rfits_write_image(filename, image, overwrite=TRUE)

Rfits_read_header(filename, keyname, keytype = 'numeric', ext = 1)

Rfits_write_header(filename, keyname, keyvalue, comment = "", ext = 1)
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
  \item{keyname}{
Character scalar; keyname in FITS header to be updated or added (if not present).  
}
  \item{keytype}{
Character scalar; type of key to be read in, either "numeric" or "string". 
}
  \item{keyvalue}{
Scalar; value to be written into header (can be any type).
}
  \item{comment}{
Character scalar; comment associated with update or added \option{keyname}.  
}
  \item{ext}{
Integer scalar; the extension to read in (usually starts at 2 for tables).
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

\code{table} functions work with binary FITS tables and R data.frames and data.tables.

\code{image} functions work with binary FITS images and R numeric or integer matrices.

\code{header} functions work with any FITS header and single keyword inputs and outputs (there is not a full header read yet).
}
\value{
\code{Rfits_write_} functions just have the side effect of writing data out.

\code{Rfits_read_table}: data.frame or data.table.

\code{Rfits_read_image}: numeric or integer matrix.

\code{Rfits_read_hader}: scalar numeric or string keyword.
}
\references{
\url{https://heasarc.gsfc.nasa.gov/docs/software/fitsio/c/c_user/cfitsio.html}
}
\author{
Aaron Robotham
Rodrgio Tobar
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
Rfits_read_header(file_image, 'NAXIS1')
Rfits_read_header(file_image, 'CTYPE1', keytype = 'string')


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\concept{ FITS }% use one of  RShowDoc("KEYWORDS")
