\name{Rfits_crop}
\alias{Rfits_crop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Image Cropper
}
\description{
Simple cropping of a target image, shifting the WCS as required.
}
\usage{
Rfits_crop(image, cropNA = TRUE, cropInf = FALSE, cropZero = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{image}{
Rfits_image / Rfits_pointer; image we wish to crop.
}
  \item{cropNA}{
Logical; should NA values be treated as masked pixels and cropped? Default it TRUE.
}
  \item{cropInf}{
Logical; should non-finite values be treated as masked pixels and cropped? Default it FALSE.
}
  \item{cropZero}{
Logical; should exactly zero (0) values be treated as masked pixels and cropped? Default it FALSE.
}
}
\details{
Just returns the cropped image.
}
\value{
Rfits_image, cropped as desired.
}
\author{
Aaron Robotham
}
\seealso{
\code{\link{Rfits_image}}
}
\examples{
image = Rfits_read_image(system.file('extdata/VIKING/mystery_VIKING_Z.fits',
  package="ProFound"))

plot(image)

image$imDat[1:20,] = NA
image$imDat[337:356,] = NA
image$imDat[,1:20] = NA
image$imDat[,337:356] = NA

plot(image)

plot(Rfits_crop(image))

image$imDat[1:20,] = NA
image$imDat[337:356,] = Inf
image$imDat[,1:20] = NA
image$imDat[,337:356] = NA

plot(image)
plot(Rfits_crop(image))
plot(Rfits_crop(image, cropInf = TRUE))

image$imDat[1:20,] = 0
image$imDat[337:356,] = Inf
image$imDat[,1:20] = NA
image$imDat[,337:356] = NA

plot(image)
plot(Rfits_crop(image))
plot(Rfits_crop(image, cropInf = TRUE))
plot(Rfits_crop(image, cropInf = TRUE, cropZero=TRUE))
}

\concept{ crop }
