\name{Rfits_point}
\alias{Rfits_point}
\alias{Rfits_pointer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FITS File Pointer
}
\description{
Creates a pointer to an on-disk FITS image file, which can then be used like a Matrix.
}
\usage{
Rfits_point(filename, ext = 1, header = TRUE, zap = NULL, zaptype = 'full',
  allow_write = FALSE, sparse = 1L, scale_sparse = FALSE)
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
Logical; should a header be extracted for each subset operation? Setting to TRUE (default) adds computational overhead.
}
  \item{zap}{
Character vector; optional unique strings to zap out of the header. These elements are passed through code{\link{grep}} one at a time and all matches are removed. This is useful if there a problematic keywords you want to remove when passing raw headers into \code{Rwcs} functions. Be careful that the strings specified are quite unique and do not remove more of the header than intended, e.g. 'CRVAL' would remove all mentions of CRVAL1/2/3 etc. Useful tricks- for inclusive ranges you can use '[]', e.g. 'CD[1-2]_[1-2]' will match to CD1_1, CD1_2, CD2_1, CD2_2. See \code{\link{grep}} for more information on how you pattern match within strings. For HST data we have often need to use \option{zap} = c('LOOKUP', 'DP[1-2]') because of the presence of non-compliant WCS keywords. By default the full 80 character header per key is scanned for matches, but this can be changed with \option{zaptype}.
}
  \item{zaptype}{
Character scalar; the type of zap to carry out. Either the grep is done on the full 80 character string ('full'; the default) or just the keynames ('name'). The latter might be useful when you are certain a simpler grep can be made if just using the keynames. The former is useful if some other flag in the key entry (a comment etc) can help flag keys to be zapped.  
}
  \item{allow_write}{
Logical; flag to indicate whether we should allow writing back into an on-disk pointer object. The default is FALSE (for safety) since you can potentially do some extreme/permanent data-altering operations with this set to TRUE. SO BE CAREFUL!
}
  \item{sparse}{
Integer scalar; default reads in a dense matrix (so all pixels in the rectangular region requested). Setting sparse to higher values means few pixels are read in, i.e. it skips every \option{sparse} pixels in each dimension.  
}
  \item{scale_sparse}{
Logical; should the image be scaled to compensate for sparse sampling (so roghly conserve flux). In an 2D image this would mean pixels are scaled by \option{sparse}^2, and more generally a N dimensional array is scaled by \option{sparse}^N. The default is to return just the native values unscaled.
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
\item{header}{Value of input \option{header}.}
\item{zap}{Value of input \option{zap}.}
\item{allow_write}{Value of input \option{allow_write}.}
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
library(magicaxis)

file_image = system.file('extdata', 'image.fits', package = "Rfits")
temp_image = Rfits_point(file_image, header=FALSE) # just the matrix part

temp_image[1:5,1:5]

temp_image[cbind(1:5,1:5)] #notice this just strips out the diagonal elements

magimage(temp_image[sparse=10])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\concept{ FITS }% use one of  RShowDoc("KEYWORDS")
