\name{Rfits_methods}
\alias{Rfits_methods}
\alias{&.Rfits_image}
\alias{|.Rfits_image}
\alias{xor.Rfits_image}
\alias{!=.Rfits_image}
\alias{==.Rfits_image}
\alias{<.Rfits_image}
\alias{<=.Rfits_image}
\alias{>.Rfits_image}
\alias{>=.Rfits_image}
\alias{+.Rfits_image}
\alias{-.Rfits_image}
\alias{*.Rfits_image}
\alias{^.Rfits_image}
\alias{/.Rfits_image}
\alias{\%/\%.Rfits_image}
\alias{\%\%.Rfits_image}
\alias{\%*\%.Rfits_image}
\alias{binattr}
\title{
  Binary Operators for Rfits_image Objects
}
\description{
  Binary operators for Rfits_image objects.
}
\usage{
\method{&}{Rfits_image}(e1,e2)
\method{|}{Rfits_image}(e1,e2)
\method{!=}{Rfits_image}(e1,e2)
\method{==}{Rfits_image}(e1,e2)
\method{<}{Rfits_image}(e1,e2)
\method{<=}{Rfits_image}(e1,e2)
\method{>}{Rfits_image}(e1,e2)
\method{>=}{Rfits_image}(e1,e2)
\method{+}{Rfits_image}(e1,e2)
\method{-}{Rfits_image}(e1,e2)
\method{*}{Rfits_image}(e1,e2)
\method{^}{Rfits_image}(e1,e2)
\method{/}{Rfits_image}(e1,e2)
\method{\%/\%}{Rfits_image}(e1,e2)
\method{\%\%}{Rfits_image}(e1,e2)
\method{\%*\%}{Rfits_image}(e1,e2)
}
\arguments{
  \item{e1}{ an object of class 'Rfits_image'}
  \item{e2}{ anything else}
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