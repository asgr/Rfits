\name{Rfits_methods}
\alias{Rfits_methods}
\alias{print.Rfits_vector}
\alias{print.Rfits_image}
\alias{print.Rfits_cube}
\alias{print.Rfits_array}
\alias{print.Rfits_pointer}
\alias{print.Rfits_header}
\alias{print.Rfits_list}
\alias{length.Rfits_vector}
\alias{dim.Rfits_image}
\alias{dim.Rfits_cube}
\alias{dim.Rfits_array}
\alias{dim.Rfits_pointer}
\alias{[.Rfits_vector}
\alias{[.Rfits_image}
\alias{[.Rfits_cube}
\alias{[.Rfits_array}
\alias{[.Rfits_pointer}
\alias{&.Rfits_pointer}
\alias{|.Rfits_pointer}
\alias{xor.Rfits_pointer}
\alias{!=.Rfits_pointer}
\alias{==.Rfits_pointer}
\alias{<.Rfits_pointer}
\alias{<=.Rfits_pointer}
\alias{>.Rfits_pointer}
\alias{>=.Rfits_pointer}
\alias{+.Rfits_pointer}
\alias{-.Rfits_pointer}
\alias{*.Rfits_pointer}
\alias{^.Rfits_pointer}
\alias{/.Rfits_pointer}
\alias{\%/\%.Rfits_pointer}
\alias{\%\%.Rfits_pointer}
\alias{\%*\%.Rfits_pointer}
\alias{binattr}
\title{
  Operators for Rfits Objects
}
\description{
  Operators for Rfits objects.
}
\usage{
\method{print}{Rfits_vector}(x,  ...)
\method{print}{Rfits_image}(x,  ...)
\method{print}{Rfits_cube}(x,  ...)
\method{print}{Rfits_array}(x,  ...)
\method{print}{Rfits_pointer}(x,  ...)
\method{print}{Rfits_header}(x, ...)
\method{print}{Rfits_list}(x, ...)
\method{length}{Rfits_vector}(x)
\method{dim}{Rfits_image}(x)
\method{dim}{Rfits_cube}(x)
\method{dim}{Rfits_array}(x)
\method{dim}{Rfits_pointer}(x)
\method{[}{Rfits_vector}(x,i,keepWCS=FALSE)
\method{[}{Rfits_image}(x,i,j,keepWCS=FALSE)
\method{[}{Rfits_cube}(x,i,j,k,keepWCS=FALSE)
\method{[}{Rfits_array}(x,i,j,k,m,keepWCS=FALSE)
\method{[}{Rfits_pointer}(x,i,j,k,m,header=x$header)
\method{&}{Rfits_pointer}(e1,e2)
\method{|}{Rfits_pointer}(e1,e2)
\method{!=}{Rfits_pointer}(e1,e2)
\method{==}{Rfits_pointer}(e1,e2)
\method{<}{Rfits_pointer}(e1,e2)
\method{<=}{Rfits_pointer}(e1,e2)
\method{>}{Rfits_pointer}(e1,e2)
\method{>=}{Rfits_pointer}(e1,e2)
\method{+}{Rfits_pointer}(e1,e2)
\method{-}{Rfits_pointer}(e1,e2)
\method{*}{Rfits_pointer}(e1,e2)
\method{^}{Rfits_pointer}(e1,e2)
\method{/}{Rfits_pointer}(e1,e2)
\method{\%/\%}{Rfits_pointer}(e1,e2)
\method{\%\%}{Rfits_pointer}(e1,e2)
\method{\%*\%}{Rfits_pointer}(e1,e2)
}
\arguments{
  \item{i}{Integer vector, a contiguous vector os desired i indices to extract}
  \item{j}{Integer vector, a contiguous vector os desired j indices to extract}
  \item{k}{Integer vector, a contiguous vector os desired k indices to extract}
  \item{m}{Integer vector, a contiguous vector os desired m indices to extract}
  \item{keepWCS}{Logical, should a modified \code{Rfits} object be output with a fixed header, making the coordinate system consistent with the cutout? If FALSE (default) then just the raw object type (e.g. vector, image, cube, array) is output.}
  \item{x}{An object of class 'Rfits_pointer'}
  \item{e1}{An object of class 'Rfits_pointer'}
  \item{e2}{Anything else}
  \item{header}{Logical; should the header also be extracted when subsetting the pointer? Default inherits from the original call to \code{\link{Rfits_pointer}}.}
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
temp_image=Rfits_read_image(file_image)

#For convenience we can access the matrix part directly
temp_image$imDat[1:5,1:5] #explicit
temp_image[1:5,1:5] #convenient

#Cutout with fixed header
\dontrun{
plot(temp_image[1:50,1:50,keepWCS=TRUE])
}

#And we can print and check dim easily
print(temp_image)
dim(temp_image)

#And now a cube
temp_cube = Rfits_read_image(system.file('extdata', 'cube.fits',
  package = "Rfits"))
  
#For convenience we can access the array part directly
temp_cube$imDat[26:30,26:30,1:2] #explicit
temp_cube[26:30,26:30,1:2] #convenient

#And we can print and check dim easily
print(temp_cube)
dim(temp_cube)
}
\keyword{ classes }
\keyword{ manip }