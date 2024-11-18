\name{Rfits_methods}
\alias{Rfits_methods}
\alias{print.Rfits_vector}
\alias{print.Rfits_image}
\alias{print.Rfits_cube}
\alias{print.Rfits_array}
\alias{print.Rfits_pointer}
\alias{print.Rfits_header}
%\alias{print.Rfits_keylist}
\alias{print.Rfits_list}

\alias{length.Rfits_vector}
\alias{length.Rfits_image}
\alias{length.Rfits_cube}
\alias{length.Rfits_array}
\alias{length.Rfits_pointer}
\alias{length.Rfits_header}
%\alias{length.Rfits_keylist}

\alias{dim.Rfits_vector}
\alias{dim.Rfits_image}
\alias{dim.Rfits_cube}
\alias{dim.Rfits_array}
\alias{dim.Rfits_pointer}
\alias{dim.Rfits_header}
\alias{dim.Rfits_keylist}
\alias{dim.Rfits_pointer_hdf5}

\alias{centre}
\alias{center}
\alias{corners}
\alias{extremes}
\alias{pixscale}
\alias{pixarea}
\alias{rotation}

\alias{centre.Rfits_image}
\alias{center.Rfits_image}
\alias{centre.Rfits_pointer}
\alias{center.Rfits_pointer}
\alias{centre.Rfits_header}
\alias{center.Rfits_header}
\alias{centre.Rfits_keylist}
\alias{center.Rfits_keylist}

\alias{corners.Rfits_image}
\alias{corners.Rfits_pointer}
\alias{corners.Rfits_header}
\alias{corners.Rfits_keylist}

\alias{extremes.Rfits_image}
\alias{extremes.Rfits_pointer}
\alias{extremes.Rfits_header}
\alias{extremes.Rfits_keylist}

\alias{pixscale.Rfits_image}
\alias{pixscale.Rfits_pointer}
\alias{pixscale.Rfits_header}
\alias{pixscale.Rfits_keylist}

\alias{pixarea.Rfits_image}
\alias{pixarea.Rfits_pointer}
\alias{pixarea.Rfits_header}
\alias{pixarea.Rfits_keylist}

\alias{rotation.Rfits_image}
\alias{rotation.Rfits_pointer}
\alias{rotation.Rfits_header}
\alias{rotation.Rfits_keylist}

\alias{Rfits_length}
\alias{Rfits_dim}
\alias{Rfits_centre}
\alias{Rfits_center}
\alias{Rfits_corners}
\alias{Rfits_extremes}
\alias{Rfits_pixscale}
\alias{Rfits_pixarea}
\alias{Rfits_rotation}

\alias{[.Rfits_vector}
\alias{[.Rfits_image}
\alias{[.Rfits_cube}
\alias{[.Rfits_array}
\alias{[.Rfits_pointer}
\alias{[.Rfits_pointer_hdf5}

\alias{[<-.Rfits_pointer}

\alias{&.Rfits_image}
\alias{|.Rfits_image}
\alias{!=.Rfits_image}
\alias{==.Rfits_image}
\alias{<.Rfits_image}
\alias{<=.Rfits_image}
\alias{>.Rfits_image}
\alias{>=.Rfits_image}
\alias{+.Rfits_image}
\alias{-.Rfits_image}
\alias{*.Rfits_image}
\alias{/.Rfits_image}
\alias{^.Rfits_image}
\alias{\%\%.Rfits_image}
\alias{\%*\%.Rfits_image}
\alias{\%/\%.Rfits_image}

\alias{&.Rfits_pointer}
\alias{|.Rfits_pointer}
\alias{!=.Rfits_pointer}
\alias{==.Rfits_pointer}
\alias{<.Rfits_pointer}
\alias{<=.Rfits_pointer}
\alias{>.Rfits_pointer}
\alias{>=.Rfits_pointer}
\alias{+.Rfits_pointer}
\alias{-.Rfits_pointer}
\alias{*.Rfits_pointer}
\alias{/.Rfits_pointer}
\alias{^.Rfits_pointer}
\alias{\%\%.Rfits_pointer}
\alias{\%*\%.Rfits_pointer}
\alias{\%/\%.Rfits_pointer}
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
%\method{print}{Rfits_keylist}(x, ...)
\method{print}{Rfits_list}(x, ...)
\method{length}{Rfits_vector}(x)
\method{length}{Rfits_image}(x)
\method{length}{Rfits_cube}(x)
\method{length}{Rfits_array}(x)
\method{length}{Rfits_pointer}(x)
\method{length}{Rfits_header}(x)
%\method{length}{Rfits_keylist}(x)
\method{dim}{Rfits_vector}(x)
\method{dim}{Rfits_image}(x)
\method{dim}{Rfits_cube}(x)
\method{dim}{Rfits_array}(x)
\method{dim}{Rfits_pointer}(x)
\method{dim}{Rfits_header}(x)
\method{dim}{Rfits_keylist}(x)
\method{dim}{Rfits_pointer_hdf5}(x)

\method{centre}{Rfits_image}(x, useraw=TRUE, ...)
\method{center}{Rfits_image}(x, useraw=TRUE, ...)
\method{corners}{Rfits_image}(x, useraw=TRUE, RAneg=FALSE,  ...)
\method{extremes}{Rfits_image}(x, useraw=TRUE, unit='amin', RAneg=FALSE, ...)
\method{pixscale}{Rfits_image}(x, useraw=TRUE, unit='asec', loc='cen',  ...)
\method{pixarea}{Rfits_image}(x, useraw=TRUE, unit='asec2', loc='cen',  ...)
\method{rotation}{Rfits_image}(x, keypass=TRUE, ...)

\method{centre}{Rfits_pointer}(x, useraw=TRUE, ...)
\method{center}{Rfits_pointer}(x, useraw=TRUE, ...)
\method{corners}{Rfits_pointer}(x, useraw=TRUE, RAneg=FALSE,  ...)
\method{extremes}{Rfits_pointer}(x, useraw=TRUE, unit='amin', RAneg=FALSE, ...)
\method{pixscale}{Rfits_pointer}(x, useraw=TRUE, unit='asec', loc='cen',  ...)
\method{pixarea}{Rfits_pointer}(x, useraw=TRUE, unit='asec2', loc='cen',  ...)
\method{rotation}{Rfits_pointer}(x, keypass=TRUE, ...)

\method{centre}{Rfits_header}(x, useraw=TRUE, ...)
\method{center}{Rfits_header}(x, useraw=TRUE, ...)
\method{corners}{Rfits_header}(x, useraw=TRUE, RAneg=FALSE,  ...)
\method{extremes}{Rfits_header}(x, useraw=TRUE, unit='amin', RAneg=FALSE, ...)
\method{pixscale}{Rfits_header}(x, useraw=TRUE, unit='asec', loc='cen',  ...)
\method{pixarea}{Rfits_header}(x, useraw=TRUE, unit='asec2', loc='cen',  ...)
\method{rotation}{Rfits_header}(x, keypass=TRUE, ...)

\method{centre}{Rfits_keylist}(x, useraw=TRUE, ...)
\method{center}{Rfits_keylist}(x, useraw=TRUE, ...)
\method{corners}{Rfits_keylist}(x, useraw=TRUE, RAneg=FALSE,  ...)
\method{extremes}{Rfits_keylist}(x, useraw=TRUE, unit='amin', RAneg=FALSE, ...)
\method{pixscale}{Rfits_keylist}(x, useraw=TRUE, unit='asec', loc='cen',  ...)
\method{pixarea}{Rfits_keylist}(x, useraw=TRUE, unit='asec2', loc='cen',  ...)
\method{rotation}{Rfits_keylist}(x, keypass=TRUE, ...)

Rfits_length(filename, ext=1)
Rfits_dim(filename, ext=1)
Rfits_centre(filename, ext=1, useraw=TRUE, ...)
Rfits_center(filename, ext=1, useraw=TRUE, ...)
Rfits_corners(filename, ext=1, useraw=TRUE, RAneg=FALSE, ...)
Rfits_extremes(filename, ext=1, useraw=TRUE, unit='amin', RAneg=FALSE, ...)
Rfits_pixscale(filename, ext=1, useraw=TRUE, unit='asec', loc='cen', ...)
Rfits_pixarea(filename, ext=1, useraw=TRUE, unit='asec2', loc='cen', ...)
Rfits_rotation(filename, ext=1, keypass=TRUE, ...)

\method{[}{Rfits_vector}(x, i, header=TRUE)
\method{[}{Rfits_image}(x, i, j, box=201, type='pix', header=TRUE)
\method{[}{Rfits_cube}(x, i, j, k, header=TRUE, collapse=TRUE)
\method{[}{Rfits_array}(x, i, j, k, m, header=TRUE, collapse=TRUE)
\method{[}{Rfits_pointer}(x, i, j, k, m, box=201, type='pix', header=x$header,
  sparse=x$sparse, scale_sparse=x$scale_sparse, collapse=TRUE)
\method{[}{Rfits_pointer}(x, i, j, k, m, allow_write=x$allow_write) <- value
\method{[}{Rfits_pointer_hdf5}(x, i, j, k, m, header=x$header)

\method{&}{Rfits_image}(e1, e2)
\method{|}{Rfits_image}(e1, e2)
\method{!=}{Rfits_image}(e1, e2)
\method{==}{Rfits_image}(e1, e2)
\method{<}{Rfits_image}(e1, e2)
\method{<=}{Rfits_image}(e1, e2)
\method{>}{Rfits_image}(e1, e2)
\method{>=}{Rfits_image}(e1, e2)
\method{+}{Rfits_image}(e1, e2)
\method{-}{Rfits_image}(e1, e2)
\method{*}{Rfits_image}(e1, e2)
\method{/}{Rfits_image}(e1, e2)
\method{^}{Rfits_image}(e1, e2)
\method{\%\%}{Rfits_image}(e1, e2)
\method{\%*\%}{Rfits_image}(x, y)
\method{\%/\%}{Rfits_image}(e1, e2)

\method{&}{Rfits_pointer}(e1, e2)
\method{|}{Rfits_pointer}(e1, e2)
\method{!=}{Rfits_pointer}(e1, e2)
\method{==}{Rfits_pointer}(e1, e2)
\method{<}{Rfits_pointer}(e1, e2)
\method{<=}{Rfits_pointer}(e1, e2)
\method{>}{Rfits_pointer}(e1, e2)
\method{>=}{Rfits_pointer}(e1, e2)
\method{+}{Rfits_pointer}(e1, e2)
\method{-}{Rfits_pointer}(e1, e2)
\method{*}{Rfits_pointer}(e1, e2)
\method{/}{Rfits_pointer}(e1, e2)
\method{^}{Rfits_pointer}(e1, e2)
\method{\%\%}{Rfits_pointer}(e1, e2)
\method{\%*\%}{Rfits_pointer}(x, y)
\method{\%/\%}{Rfits_pointer}(e1, e2)

}
\arguments{
  \item{filename}{
Character scalar; path to FITS file to operate the method on.
}
  \item{i}{
Integer/numeric vector/matrix, a contiguous vector of desired i indices to extract. Or Numeric scalar RA value if \option{type} = 'coord'. If the second element y of x:y is 'end' then the upper dimension is substituted. If \option{i} is a matrix then it allows non-contiguous random access with Rfits_pointer objects (see \code{\link{Rfits_point}}).
}
  \item{j}{
Integer/numeric vector, a contiguous vector of desired j indices to extract. Or Numeric scalar Dec value if \option{type} = 'coord'. If the second element y of x:y is 'end' then the upper dimension is substituted.
}
  \item{k}{
Integer/numeric vector, a contiguous vector of desired k indices to extract. If the second element y of x:y is 'end' then the upper dimension is substituted.
}
  \item{m}{
Integer/numeric vector, a contiguous vector of desired m indices to extract. If the second element y of x:y is 'end' then the upper dimension is substituted.
}
  \item{useraw}{
Logical; should the full raw header be used and passed to CFITSIO? This is often good (uses all the distortion terms etc), but there are failing cases with long messy headers so sometimes this cannot be used.
}
  \item{unit}{
Character scalar; for \code{pixscale} / \code{extremes} must be one of asec / amin / deg / rad, for \code{pixarea} must be one of asec2 / amin2 / deg2 / rad2 / str (where rad2 and str are both steradians).
}
  \item{loc}{
Character scalar / integer vector; defines the location to calculate the pixel scale or pixel area. Character options are 'cen'; 'bl' (bottom-left); 'tl' (top-left); 'tr' (top-right); 'br' (bottom-right). Supplying a two element integer vector will mean this loc[x,y] position is used (in R coordinates).
}
  \item{RAneg}{
When the \option{image} wraps through RA = 0, should values 'below' 0 (so 359) be treated as -ve (so -1). This is useful sometimes to not produce weird results when dealing with image corners in plots etc.}

  \item{keypass}{
Logical; should \code{Rwcs_keypass} be used to clean up the header?
}
  \item{header}{
Logical, should a modified \code{Rfits} object be output with a fixed header, making the coordinate system consistent with the cutout? If FALSE (default) then just the raw object type (e.g. vector, image, cube, array) is output. For \code{\link{Rfits_pointer}}, should the header also be extracted when sub-setting the pointer? Default inherits from the original call to \code{\link{Rfits_pointer}}.
}
  \item{collapse}{
Logical; should the array or cube be collapsed down dimensions if the 3rd/4th dimensions are 1L? If TRUE this means slices in cubes/arrays are converted to class Rfits_cube/Rfits_image as appropriate. Note the user needs to specify the dimension slice to be '1', so it is more specific than just relying on the dimension only having size 1. E.g. if ex is a 10x10x1 cube then ex[,,1,collapse=TRUE] would become a 10x10 image, but ex[,,,collapse=TRUE] would not, instead it would still return a 10x10x1 cube. This is really to stop users accidentally collapsing things, and deliberately forces active input (so a feature not a bug).
}
  \item{type}{
  For Rfits_image objects you can specify cutout locations either by pixel position (pix) or RA/Dec coordinates (coord). You will need \code{Rwcs} in the latter case and \option{i} and \option{j} must both be length 1.
}
  \item{box}{
If \option{i}/\option{j} are length 1 then box[1] and box[2] specifies how big the cutouts will be, where it will be \option{i} +/- (box[1]-1)/2 and \option{j} +/- (box[2]-1)/2.
}
  \item{x}{
An object of a relevant class.
}
  \item{y}{
An object of a relevant class.
}
  \item{value}{
The replacement data to write to disk.
}
  \item{e1}{
An object of class 'Rfits_image', where operation happens on e1$imDat.
}
  \item{e2}{
An object of class 'Rfits_image, or a sensible scalar, vector or matrix.
}
  \item{sparse}{
Integer scalar; default reads in a dense matrix (so all pixels in the rectangular region requested). Setting sparse to higher values means few pixels are read in, i.e. it skips every \option{sparse} pixels in each dimension.  
}
  \item{scale_sparse}{
Logical; should the image be scaled to compensate for sparse sampling (so roughly conserve flux). In an 2D image this would mean pixels are scaled by \option{sparse}^2, and more generally a N dimensional array is scaled by \option{sparse}^N. The default is to return just the native values unscaled.
}
  \item{allow_write}{
Logical; flag to indicate whether we should allow writing back into an on-disk pointer object. Default inherits from the original call to \code{\link{Rfits_pointer}}.
}
  \item{ext}{
Integer scalar; the extension to operate the method on.
}
  \item{\dots}{
Either unused, or pass on WCS related information for \code{Rwcs} related methods (centre, corners, pixscale etc).
}
}
\value{
  \item{length}{Integer scalar; these methods return the total number of elements present in the target object (so pixels in an image, spaxels in a cube, cells in a table etc.)}
  \item{dim}{Integer vector; these methods return the dimensions present in the target object.}
  \item{centre / center}{Numeric vector; these methods return the coordinates at the centre of the spatial dimensions. If this fails, returns NA.}
  \item{corners}{Numeric matrix; these methods return the coordinates at the corners of the spatial dimensions. Rows are TL / BL / TR / BR (top to bottom). If this fails, returns NA.}
  \item{extremes}{Numeric matrix; these methods return the coordinates at the absolute extremes of the spatial dimensions. Rows are min /max / range (top to bottom), where range is in units of \option{unit}. If this fails, returns NA. Note if the RA range crosses 0/360 then the min RA will be the larger value (say 359.9) and the max will be the smaller one (say 0.1), since that captures the correct sense of the low RA being West (effectively like having a -ve RA) of the high RA but correctly converts -ve RA to -ve \%\% 360.}
  \item{pixscale}{Numeric scalar; these methods return the pixel scale of the spatial dimensions in the specified units at the specified \option{loc}. If this fails, returns NA.}
  \item{pixarea}{Numeric scalar; these methods return the pixel area of the spatial dimensions in the specified units at the specified \option{loc}. If this fails, returns NA.}
  \item{rotation}{Numeric vector; these methods return the image rotation in degrees for the North and East vector. This is the vector of [1] North with respect to y-axis up clockwise (so ^ is 0; > is 90; < is 270); and East with respect to x-axis right clockwise (so ^ is 270; > is 0; < is 180) [2]. This should be clear when comparing to the compass drawn when making a \code{plot} of the \option{image} data. A 'standard' projection probably has North up and East right, so the output rotation will be [0,180]. If this fails, returns NA.}
  
  \code{\link{&}}, \code{\link{|}}, \code{\link{xor}}, \code{\link{!=}}, \code{\link{==}}, 
  \code{\link{<}}, \code{\link{<=}}, \code{\link{>}}, \code{\link{>=}} return a logical vector \cr
  \code{\link{^}} and \code{\link{/}} return a double vector\cr
  \code{\link{+}}, \code{\link{-}}, \code{\link{*}}, \code{\link{\%/\%}}, \code{\link{\%*\%}}, \code{\link{\%\%}}
   return a vector of class Matrix
}
\author{
Aaron Robotham
}
\seealso{
\code{\link{Rfits_image}}, \code{\link{Rfits_point}}
}
\examples{
#Read full image
file_image=system.file('extdata', 'image.fits', package = "Rfits")
temp_image=Rfits_read_image(file_image)

#For convenience we can access the matrix part directly
temp_image$imDat[1:5,1:5] #explicit
temp_image[1:5,1:5]$imDat #extracts the image and header, then just shows $imDat
temp_image[1:5,1:5,header=FALSE] #same as above

#If we want the header too, this is the default:
temp_image[1:5,1:5]

#Cutout with fixed header
\dontrun{
plot(temp_image[1:50,1:50])
plot(temp_image[352.2917,-31.82667,box=51,type='coord'])
}

#Same with a pointer:
temp_point = Rfits_point(file_image)
\dontrun{
plot(temp_point[352.2917,-31.82667,box=51,type='coord',header=TRUE])
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

#For convenience we can access the end dimension limit without explicitly checking:
temp_cube[46:end,46:end,3:end]$imDat #The dim is 50x50x4, so this creates a 5x5x2 array
}
\keyword{ classes }
\keyword{ manip }