\name{Rfits_methods}
\alias{Rfits_methods}
\alias{print.Rfits_image}
\alias{dim.Rfits_image}
\alias{print.Rfits_image_pointer}
\alias{dim.Rfits_image_pointer}
\alias{[.Rfits_image_pointer}
\alias{&.Rfits_image_pointer}
\alias{|.Rfits_image_pointer}
\alias{xor.Rfits_image_pointer}
\alias{!=.Rfits_image_pointer}
\alias{==.Rfits_image_pointer}
\alias{<.Rfits_image_pointer}
\alias{<=.Rfits_image_pointer}
\alias{>.Rfits_image_pointer}
\alias{>=.Rfits_image_pointer}
\alias{+.Rfits_image_pointer}
\alias{-.Rfits_image_pointer}
\alias{*.Rfits_image_pointer}
\alias{^.Rfits_image_pointer}
\alias{/.Rfits_image_pointer}
\alias{\%/\%.Rfits_image_pointer}
\alias{\%\%.Rfits_image_pointer}
\alias{\%*\%.Rfits_image_pointer}
\alias{binattr}
\title{
  Binary Operators for Rfits_image_pointer Objects
}
\description{
  Binary operators for Rfits_image_pointer objects.
}
\usage{
\method{print}{Rfits_image}(x,  ...)
\method{dim}{Rfits_image}(x)
\method{print}{Rfits_image_pointer}(x,  ...)
\method{dim}{Rfits_image_pointer}(x)
\method{[}{Rfits_image_pointer}(x,i,j)
\method{&}{Rfits_image_pointer}(e1,e2)
\method{|}{Rfits_image_pointer}(e1,e2)
\method{!=}{Rfits_image_pointer}(e1,e2)
\method{==}{Rfits_image_pointer}(e1,e2)
\method{<}{Rfits_image_pointer}(e1,e2)
\method{<=}{Rfits_image_pointer}(e1,e2)
\method{>}{Rfits_image_pointer}(e1,e2)
\method{>=}{Rfits_image_pointer}(e1,e2)
\method{+}{Rfits_image_pointer}(e1,e2)
\method{-}{Rfits_image_pointer}(e1,e2)
\method{*}{Rfits_image_pointer}(e1,e2)
\method{^}{Rfits_image_pointer}(e1,e2)
\method{/}{Rfits_image_pointer}(e1,e2)
\method{\%/\%}{Rfits_image_pointer}(e1,e2)
\method{\%\%}{Rfits_image_pointer}(e1,e2)
\method{\%*\%}{Rfits_image_pointer}(e1,e2)
}
\arguments{
  \item{i}{Integer vector, a contiguous vector os desired i indices to extract}
  \item{j}{Integer vector, a contiguous vector os desired j indices to extract}
  \item{x}{ an object of class 'Rfits_image_pointer'}
  \item{e1}{ an object of class 'Rfits_image_pointer'}
  \item{e2}{ anything else}
  \item{\dots}{Unused, but required}
}
\value{
  \code{\link{&}}, \code{\link{|}}, \code{\link{xor}}, \code{\link{!=}}, \code{\link{==}}, 
  \code{\link{<}}, \code{\link{<=}}, \code{\link{>}}, \code{\link{>=}} return a logical vector \cr
  \code{\link{^}} and \code{\link{/}} return a double vector\cr
  \code{\link{+}}, \code{\link{-}}, \code{\link{*}}, \code{\link{\%/\%}}, \code{\link{\%*\%}}, \code{\link{\%\%}}
   return a vector of class Matrix
}
\author{
Aaron Robotham
}
\seealso{\code{\link{Rfits_image}}}
\examples{
#Read full image
file_image=system.file('extdata', 'image.fits', package = "Rfits")
temp_image=Rfits_point(file_image)

#temp_image[1:5,1:5]
}
\keyword{ classes }
\keyword{ manip }