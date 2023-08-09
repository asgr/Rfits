\name{plot.Rfits}
\alias{plot.Rfits_image}
\alias{plot.Rfits_cube}
\alias{plot.Rfits_array}
\alias{plot.Rfits_vector}
\alias{lines.Rfits_vector}
\alias{plot.Rfits_pointer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
WCS Plot Methods for Images and Cubes
}
\description{
Makes a nice WCS image of a sensibly described vector / image / cube / array read in with \code{\link{Rfits_read_image}}.
}
\usage{
\method{plot}{Rfits_image}(x, useraw = FALSE, ...)
\method{plot}{Rfits_cube}(x, slice = 1, useraw = FALSE, ...)
\method{plot}{Rfits_array}(x, slice = c(1,1), useraw = FALSE, ...)
\method{plot}{Rfits_vector}(x, ...)
\method{lines}{Rfits_vector}(x, ...)
\method{plot}{Rfits_pointer}(x, useraw = FALSE, sparse = 'auto', ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class \code{Rfits_image} / \code{Rfits_cube} / \code{Rfits_array} / \code{Rfits_vector}, e.g. as read in by \code{\link{Rfits_read_image}}.
}
  \item{useraw}{
Logical; should the full raw header be used and passed to CFITSIO? This is often good (uses all the distortion terms etc), but there are failing cases with long messy headers so sometimes this cannot be used.  
}
  \item{slice}{
Integer scalar or vector; the slice/s of the FITS cube/array to display.  
}
  \item{sparse}{
Integer scalar; determines whether the image pixels are sparse sampled to speed up plotting. If set to 2 it will only determine every 2nd pixel, and if 3 every 3rd etc. The default 'auto' means it will scale to produce a maximum number of 1,000 pixels on any side (on most monitors this is a fairly useful maximum, and ensures quick displaying of even very large images).
}
  \item{\dots}{
Extra options to pass to \code{Rwcs_image}.
}
}
\value{
Outputs the final image list containing x,y and z. Generally run for the side effect of producing rapid and well-scaled image plots.
}
\author{
Aaron Robotham
}
\seealso{
\code{\link{Rfits_image}}, ~~~
}
\examples{
\dontrun{
file_image = system.file('extdata', 'image.fits', package = "Rfits")
temp_image = Rfits_read_image(file_image)
plot(temp_image)

temp_point = Rfits_point(file_image)
plot(temp_point)
plot(temp_point, sparse=4) #This will also update the WCS on the fly
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\concept{ FITS }% use one of  RShowDoc("KEYWORDS")
\concept{ WCS }% __ONLY ONE__ keyword per line
