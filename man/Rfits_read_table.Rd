\name{Rfits_read_table}
\alias{Rfits_read_table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FITS Table Reader
}
\description{
Generic table reader. Works with a wide range of data types including 64 bit integers (long long).
}
\usage{
Rfits_read_table(file, ext = 2, data.table = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
Character scalar; path to FITS file.
}
  \item{ext}{
Integer scalar; the extension to read in (usually starts at 2 for tables).
}
  \item{data.table}{
Logical; should a \code{data.table} be returned?
}
}
\details{
R FITS reader that uses CFITSIO to do the interfacing with files. Requires \code{bit64} package for integer64 (i.e. long long) to be work.
}
\value{
Data.frame or data.table.
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
file=system.file('extdata','table.fits',package="Rfits")
temp[1:5,]
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\concept{ FITS }% use one of  RShowDoc("KEYWORDS")
