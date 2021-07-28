\name{Rfits_point}
\alias{Rfits_point}
\alias{Rfits_pointer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FITS File Pointer
}
\description{
Creates a pointer to an on-disk FITS file, which can then be used like a Matrix.
}
\usage{
Rfits_point(filename, ext = 1, header = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
Character scalar; path to FITS file, either to be written to or read.
}
  \item{ext}{
Integer scalar; the extension to read the image from (usually starts at 1 for images).
}
  \item{header}{
Logical; should a header be extracted for each subset operation? Setting to TRUE adds computational overhead.
}
}
\details{
This function creates a pointer to a FITS file. This contains the bare essentials regarding the file path and extension. From here, there are various methods to create on the fly cutouts of an on-disk FITS file, so the whole file does not need to be loaded into memory to e.g. cutout a small subset and calculate properties. In principle the object created by \code{Rfits_point} can be used exactly like a Matrix if \option{header}=FALSE is set (the sensible default).
}
\value{
A pointer to a FITS file of class Rfits_image. There are numerous methods for this class (see \code{\link{Rfits_methods}})

\item{filename}{Character scalar; path to FITS file}
\item{ext}{Integer scalar; position of the FITS extension of interest}
\item{keyvalues}{List; named \option{keyvalues} list header, as per \code{\link{Rfits_read_header}}}
\item{header}{Logical; should a bespoke header be extracted for each subset operation? Setting to TRUE adds computational overhead.}
\item{dim}{Integer vector; the dimension of the pointer object. The length of this is the number of dimensions (1 a vector, 2 an image/matrix, 3 a cube, 4 an array), where the value at each position is the length in that dimension. Hence c(100,200) would be a [100,200] sized image/matrix.}
\item{type}{Character scalar; the type of object being pointer to: (1D is a 'vector', 2D an 'image', 3 a 'cube', 4 an 'array')}
}
\author{
Aaron Robotham
}
\seealso{
\code{\link{Rfits_image}}, \code{\link{Rfits_methods}}
}
\examples{
file_image = system.file('extdata', 'image.fits', package = "Rfits")
temp_image = Rfits_point(file_image)

temp_image[1:5,1:5]

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\concept{ FITS }% use one of  RShowDoc("KEYWORDS")
