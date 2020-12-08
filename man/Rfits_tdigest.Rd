\name{Rfits_tdigest}
\alias{Rfits_tdigest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a t-Digest of an Image
}
\description{
Creates a \code{tdigest} object of an image, which can be a matrix or a pointer. This \code{tdigest} can then be accessed using the usual methods to obtain rapid quantiles of the original image. This is useful for very large images (think more than 10k x 10k) where it is quite expensive to load the whole image into memory and compute quantiles.
}
\usage{
Rfits_tdigest(image, mask = NULL, chunk = 100L, compression = 1e3,
  verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{image}{
Numeric matrix or pointer; required, the image we want to digest. Can be a true numeric matrix held in memory locally, or an \code{\link{Rfits_point}} pointer to a FITS file containing the target image. NA pixels are ignored (effectively treated as masked).
}
  \item{mask}{
Boolean matrix; optional, parts of the image to mask out (i.e. ignore). If a matrix is provided, this matrix *must* be the same dimensions as \option{image} where 1 means mask out and 0 means use for analysis.
}
  \item{chunk}{
Integer scalar; the number of columns read in at a time. Larger is faster to a point, but uses more memory (which can then slow things down). Given the number of pixels in a column, you probably do not want Ncol x \option{chunk} to be much larger than 1e7.
}
  \item{compression}{
Numeric scalar; the input compression value; should be >= 1.0; this will control how aggressively the t-Digest compresses data together (larger is more accurate, where accuracy goes as 1/\option{compression} and memory use is linearly proportional with \option{compression}). The original t-Digest paper suggests using a value of 100 for a good balance between precision and efficiency. It will land at very small (think like 1e-6 percentile points) errors at extreme points in the distribution, and compression ratios of around 500 for large data sets (~1 million datapoints). Defaults to 100.
}
  \item{verbose}{
Logical; should digesting progress be printed to the screen?
}
}
\details{
See ?\code{tdigest} for more details.
}
\value{
A \option{tdigest} object.
}
\author{
Aaron Robotham
}
\seealso{
\code{\link{Rfits_point}}
}
\examples{
library(tdigest)
file_image=system.file('extdata', 'image.fits', package = "Rfits")
temp_image=Rfits_read_image(file_image)
median(temp_image$imDat)

#This is a silly example since the target image is small and we can digest
#it directly in memory. The advantage will be very large images where we
#want to use a pointer instead.

td=tdigest(temp_image$imDat, compression=1e3) #larger compression is better!
td[0.5]

temp_point=Rfits_point(file_image)
td2=Rfits_tdigest(temp_point)
td2[0.5]

#Now with a mask (blanking out the central part of the \option{image})

mask=matrix(0L,dim(temp_image)[1],dim(temp_image)[2])
mask[151:250,151:250]=1L
median(temp_image$imDat[mask==0])

td3=Rfits_tdigest(temp_point, mask=mask)
td3[0.5]
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\concept{ quantile }% use one of  RShowDoc("KEYWORDS")
