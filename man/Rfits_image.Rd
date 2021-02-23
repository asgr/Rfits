\name{Rfits_image}
\alias{Rfits_image}
\alias{Rfits_cube}
\alias{Rfits_read_image}
\alias{Rfits_read_cube}
\alias{Rfits_read_array}
\alias{Rfits_read_vector}
\alias{Rfits_write_image}
\alias{Rfits_write_cube}
\alias{Rfits_write_array}
\alias{Rfits_write_vector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FITS Image and Cube Readers and Writers
}
\description{
Generic image, cube and array (4D) readers and writers. Currently reading and writing functions will work up to 4D.
}
\usage{
Rfits_read_image(filename = 'temp.fits', ext = 1, header = TRUE, xlo = NULL,
    xhi = NULL, ylo = NULL, yhi = NULL, zlo = NULL, zhi = NULL,  tlo = NULL,
    thi = NULL, remove_HIERARCH = FALSE)
  
Rfits_read_cube(filename = 'temp.fits', ext = 1, header = TRUE, xlo = NULL,
    xhi = NULL, ylo = NULL, yhi = NULL, zlo = NULL, zhi = NULL,  tlo = NULL,
    thi = NULL, remove_HIERARCH = FALSE)
    
Rfits_read_array(filename = 'temp.fits', ext = 1, header = TRUE, xlo = NULL,
    xhi = NULL, ylo = NULL, yhi = NULL, zlo = NULL, zhi = NULL,  tlo = NULL,
    thi = NULL, remove_HIERARCH = FALSE)

Rfits_read_vector(filename = 'temp.fits', ext = 1, header = TRUE, xlo = NULL,
    xhi = NULL, ylo = NULL, yhi = NULL, zlo = NULL, zhi = NULL,  tlo = NULL,
    thi = NULL, remove_HIERARCH = FALSE)

Rfits_write_image(data, filename = 'temp.fits', ext = 1, keyvalues, keycomments,
    keynames, comment, history, numeric = "single", integer = "long",
    create_ext = TRUE, create_file = TRUE, overwrite_file = TRUE, bzero = 0,
    bscale = 1) 
    
Rfits_write_cube(data, filename = 'temp.fits', ext = 1, keyvalues, keycomments,
    keynames, comment, history, numeric = "single", integer = "long",
    create_ext = TRUE, create_file = TRUE, overwrite_file = TRUE, bzero = 0,
    bscale = 1) 
    
Rfits_write_array(data, filename = 'temp.fits', ext = 1, keyvalues, keycomments,
    keynames, comment, history, numeric = "single", integer = "long",
    create_ext = TRUE, create_file = TRUE, overwrite_file = TRUE, bzero = 0,
    bscale = 1)

Rfits_write_vector(data, filename = 'temp.fits', ext = 1, keyvalues, keycomments,
    keynames, comment, history, numeric = "single", integer = "long",
    create_ext = TRUE, create_file = TRUE, overwrite_file = TRUE, bzero = 0,
    bscale = 1) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Numeric or integer matrix (i.e. image), vector, cube or 4D array; data to be written out as a FITS image or cube. For convenience you can pass the output of \code{Rfits_read_image} directly into this argument and it will split out the \option{imDat}, and various header inputs as required (if you also directly provide a header related input, this takes precedents.
}
  \item{filename}{
Character scalar; path to FITS file, either to be written to or read. There is good support for compressed reading and limited support for compressed writing (see Examples). For writing just \option{image} data (no WCS is allowed at this time) you can use the filename to compress the target image by appending FITS relevant compression flags such as [compress] (uses RICE) [compress GZIP], [compress PLIO] and [compress HCOMPRESS]. E.g \option{filename} = 'file.fits' for writing a normal uncompressed images, or \option{filename} = 'file.fits[compress]' for writing a compressed image. These have additional parameters that can be passed in, see \url{https://heasarc.gsfc.nasa.gov/docs/software/fitsio/compression.html}. Support for WCS writing of compressed images will come in the future, currently you should only write the image matrix to a compressed extenstion (see Examples).
}
  \item{ext}{
Integer scalar; the extension to read or write. Usually you want to use \option{ext} = 1 for normal images, or \option{ext} = 2 for compressed images (see Examples). For writing, what really happens is the specified extension is deleted (if \option{create_ext} = FALSE), all current extensions shuffle forward one place, and a new extension is appended at the end of the file. This means unless the target extension is already at the end of the FITS file, the extension order will change. This is how cfitsio works with extensions (not a \code{Rfits} design decision). If the order is really important then you will need to construct a new FITS file from scratch and build it in the order desired.
}
  \item{header}{
Logical; should the header be read along with the image matrix. If TRUE (default) then the output of \code{Rfits_write_image} is a list with components \option{imDat}, and the header list outputs created by \code{\link{Rfits_read_header}} (all at the same level), if FALSE then it is just the raw image matrix.
}
  \item{keyvalues}{
List; values to be written (updated or added) into header (can be any type, e.g. numeric, integer, string).
}
  \item{keycomments}{
Character vector; (not required) comments associated with updated or added \option{keynames}.  
}
  \item{keynames}{
Character vector; (not required) keynames in FITS header to be updated or added (if not present). If not supplied then the keynames will be taken from the names of \option{keyvalues}.
}
  \item{comment}{
Character scalar/vector; (not required) comments associated with FITS extension.  
}
  \item{history}{
Character scalar/vector; (not required) history information associated with with FITS extension. 
}
  \item{numeric}{
Character scalar; the numeric data type to be written out for images. Can be '32' (32 bit), 'single' (32 bit), 'float' (32 bit), '64' (64 bit), 'double' (64 bit).
}
  \item{integer}{
Character scalar; the integer data type to be written out for images. Can be '16' (16 bit), 'short' (16 bit), 'int' (16 bit), '32' (32 bit), 'long' (32 bit). 
}
  \item{xlo}{
Integer scalar; low x limit (usually Right Ascension axis) to cut out from \option{image} (1 or larger and less than \option{xhi}).
}
  \item{xhi}{
Integer scalar; high x limit (usually Right Ascension axis) to cut out from \option{image} (NAXIS1 or smaller and greater than \option{xhi}).
}
  \item{ylo}{
Integer scalar; low y limit (usually Declination axis) to cut out from \option{image} (1 or larger and less than \option{yhi}).
}
  \item{yhi}{
Integer scalar; high y limit (usually Declination axis) to cut out from \option{image} (NAXIS2 or smaller and greater than \option{yhi}).
}
  \item{zlo}{
Integer scalar; low z limit (usually Spectral axis) to cut out from \option{image} (1 or larger and less than \option{zhi}).
}
  \item{zhi}{
Integer scalar; high z limit (usually Spectral axis) to cut out from \option{image} (NAXIS3 or smaller and greater than \option{zhi}).
}
  \item{tlo}{
Integer scalar; low t limit (usually Temporal axis) to cut out from \option{image} (1 or larger and less than \option{thi}).
}
  \item{thi}{
Integer scalar; high z limit (usually Temporal axis) to cut out from \option{image} (NAXIS4 or smaller and greater than \option{thi}).
}
  \item{remove_HIERARCH}{
Logical, should the leading 'HIERARCH' be removed for extended keyword names (longer than 8 characters)?  
}
  \item{create_ext}{
Logical; should a new extension be created (TRUE) or not (FALSE). This will always be appended at the end of the FITS file (as per the FITS standard). If \option{create_ext} = TRUE then no matter what the \option{ext} argument is, the extension will be created at the end of the FITS file. If \option{create_ext} = FALSE then the extension specified by \option{ext} will be first deleted, and then the extension will be created at the end of the FITS file.
}
  \item{create_file}{
Logical; should a new file be created (TRUE) or not (FALSE). If not then the file at \option{filename} must exist for extensions to be written to it. This is really here to ensure safe operation, i.e. if \option{create_file}=TRUE and \option{overwrite_file}=FALSE and there is a file already at the path given by \option{filename} then you will get an error.
}
  \item{overwrite_file}{
Logical; if file exists at location \option{filename}, should it be overwritten (i.e. deleted and a fresh FITS file created)?
}
  \item{bzero}{
Numeric scalar; convenience BZERO zero level to apply to image, where VAL = (RAW x BSCALE) + BZERO, where VAL is what is seen in the R \code{matrix} or \code{array} and RAW is what you would find if inspecting the binary values stored on disk. If this is present in the \option{keyvalues} then the keyvalues$BZERO value takes priority.
}
  \item{bscale}{
Numeric scalar; convenience BSCALE scaling factor to apply to image, where VAL = (RAW x BSCALE) + BZERO, where VAL is what is seen in the R \code{matrix} or \code{array} and RAW is what you would find if inspecting the binary values stored on disk. If this is present in the \option{keyvalues} then the keyvalues$BSCALE value takes priority.
}
}
\details{
R FITS readers and writers that uses CFITSIO to do the interfacing with files. Requires \code{bit64} package for integer64 (i.e. long long) to work.

\code{Rfits_read_image} reads in data from a FITS file. This can be either an image or a cube. Note that image subsetting does not work for cubes (you have to read in the full cube).

\code{Rfits_read_cube} is an alias for \code{Rfits_read_image}, but is there for potentially clearer and more literal code.

\code{Rfits_read_array} is an alias for \code{Rfits_read_image}, but is there for potentially clearer and more literal code.

\code{Rfits_write_image} writes image or cube data to a FITS file. Note that when writing extensions are always added onto the end of the current FITS file. If doing this you should use settings \option{overwrite_file} = FALSE (we do not want to delete the current file), \option{create_file} = FALSE (we do not want to create a new file), \option{create_ext} = FALSE (we do not want to simply append an extension to the end of the current file), \option{ext} = N (we want to delete extension N, and add a new one to the end of the file).

\code{Rfits_write_cube} is an alias for \code{Rfits_write_image}, but is there for potentially neater code.

\code{Rfits_write_array} is an alias for \code{Rfits_write_image}, but is there for potentially neater code.

When reading, if \option{header}=TRUE (default) then the output of \option{Rfits_write_image} is a list with components: \option{imDat}; the header list outputs created by \code{\link{Rfits_read_header}} (all at the same level); the \option{filename} and \option{ext}. The object returned is of class \option{Rfits_image}, and has class dependent \code{print} and \code{dim} functions.

When reading, if \option{header}=FALSE then then the output is just the raw image matrix or cube array. You can either overwrite a current file or create a new extension to an already present FITS file. 
}
\note{
You can either specifify BZERO and BSCALE through the input \option{bzero} and \option{bscale} function parameters, or put them in the \option{keyvalues} header list (with matching entrie in \option{keycomments}). Be careful to remove the BZERO and BSCALE values from the \option{keyvalues} header list if you wish to use the function interface, since the \option{keyvalues} header values (if present) always take precedent (see Examples).
}
\value{
\code{Rfits_read_image}: List; read image into numeric or integer matrix (\option{imDat}) and also header information as per \code{\link{Rfits_read_header}}.

\code{Rfits_write_image} write out image to target FITS extension.
}
\references{
\url{https://heasarc.gsfc.nasa.gov/docs/software/fitsio/c/c_user/cfitsio.html}
}
\author{
Aaron Robotham,
Rodrigo Tobar
}

\seealso{
\code{\link{Rfits_table}}, \code{\link{Rfits_header}}, \code{FITSio}, \code{astro}
}
\examples{
#Read full image
file_image=system.file('extdata', 'image.fits', package = "Rfits")
temp_image=Rfits_read_image(file_image)

#For convenience we can access the matrix part directly
temp_image$imDat[1:5,1:5] #explicit
temp_image[1:5,1:5] #convenient

#And we can print and check dim easily
print(temp_image)
dim(temp_image)

#Take a look
library(magicaxis)
magimageWCS(temp_image)

#And again with just a subset
temp_image_sub=Rfits_read_image(file_image, xlo=150, xhi=250, ylo=100, yhi=200)
magimageWCS(temp_image_sub)

#check write and read consistency
file_image_temp = tempfile()
Rfits_write_image(temp_image, file_image_temp)
temp_image2 = Rfits_read_image(file_image_temp)

#And now a cube
temp_cube = Rfits_read_image(system.file('extdata', 'cube.fits',
  package = "Rfits"))
  
#For convenience we can access the array part directly
temp_cube$imDat[26:30,26:30,1:2] #explicit
temp_cube[26:30,26:30,1:2] #convenient

#And we can print and check dim easily
print(temp_cube)
dim(temp_cube)

#Use BSCALE and BZERO
file_image = system.file('extdata', 'image.fits', package = "Rfits")
temp_image = Rfits_read_image(file_image)
temp_image$keyvalues$BZERO = 100
temp_image$keyvalues$BSCALE = 10
temp_image$keycomments$BZERO = ""
temp_image$keycomments$BSCALE = ""
temp_image$keynames = c(temp_image$keynames, "BZERO", "BSCALE")
file_image_temp = tempfile()
Rfits_write_image(temp_image, file_image_temp)
temp_image = Rfits_read_image(file_image_temp)
Rfits_write_image(temp_image, file_image_temp)
temp_image2 = Rfits_read_image(file_image_temp)
sum(temp_image$imDat - temp_image2$imDat) 

#Use compression
file.size(file_image)
Rfits_write_image(temp_image$imDat, paste0(file_image_temp,'[compress]'))
file.size(file_image_temp) #about a factor 5 compressed
temp_image3 = Rfits_read_image(file_image_temp, ext=2)
magimage(temp_image3$imDat)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\concept{ FITS }% use one of  RShowDoc("KEYWORDS")
