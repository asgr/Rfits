\name{plot.Rfits}
\alias{plot.Rfits_image}
\alias{plot.Rfits_cube}
\alias{plot.Rfits_array}
\alias{plot.Rfits_image_pointer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
WCS Plot Methods for Images and Cubes
}
\description{
Makes a nice WCS image of a sensibly described image read in with \code{\link{Rfits_read_image}}.
}
\usage{
\method{plot}{Rfits_image}(x, ...)
\method{plot}{Rfits_cube}(x, slice = 1, ...)
\method{plot}{Rfits_array}(x, slice = c(1,1), ...)
\method{plot}{Rfits_image_pointer}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class \code{Rfits_image}, e.g. as read in by \code{\link{Rfits_read_image}}.
}
  \item{slice}{
Integer scalar or vector; the slice/s of the FITS cube/array to display.  
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
file_image=system.file('extdata', 'image.fits', package = "Rfits")
temp_image=Rfits_read_image(file_image)
plot(temp_image)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\concept{ FITS }% use one of  RShowDoc("KEYWORDS")
\concept{ WCS }% __ONLY ONE__ keyword per line
