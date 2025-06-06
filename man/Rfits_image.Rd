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
\alias{Rfits_create_image}
\alias{Rfits_write_pix}
\alias{Rfits_blank_image}
\alias{Rfits_check_image}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FITS Vector Image Cube and Array Readers and Writers
}
\description{
Generic image, cube and array (4D) readers and writers. Currently reading and writing functions will work up to 4D.
}
\usage{
Rfits_read_image(filename = 'temp.fits', ext = 1, header = TRUE, xlo = NULL,
  xhi = NULL, ylo = NULL, yhi = NULL, zlo = NULL, zhi = NULL,  tlo = NULL,
  thi = NULL, remove_HIERARCH = FALSE, force_logical = FALSE, bad = NULL,
  keypass = FALSE, zap = NULL, zaptype = 'full', sparse = 1L, scale_sparse = FALSE,
  collapse = FALSE, NAcheck = TRUE, nthreads = 1)
  
Rfits_read_vector(filename = 'temp.fits', ext = 1, header = TRUE, xlo = NULL,
  xhi = NULL, ylo = NULL, yhi = NULL, zlo = NULL, zhi = NULL,  tlo = NULL,
  thi = NULL, remove_HIERARCH = FALSE, force_logical = FALSE, bad = NULL,
  keypass = FALSE, zap = NULL, zaptype = 'full', sparse = 1L, scale_sparse = FALSE,
  collapse = FALSE, NAcheck = TRUE, nthreads = 1)
    
Rfits_read_cube(filename = 'temp.fits', ext = 1, header = TRUE, xlo = NULL,
  xhi = NULL, ylo = NULL, yhi = NULL, zlo = NULL, zhi = NULL,  tlo = NULL,
  thi = NULL, remove_HIERARCH = FALSE, force_logical = FALSE, bad = NULL,
  keypass = FALSE, zap = NULL, zaptype = 'full', sparse = 1L, scale_sparse = FALSE,
  collapse = FALSE, NAcheck = TRUE, nthreads = 1)
    
Rfits_read_array(filename = 'temp.fits', ext = 1, header = TRUE, xlo = NULL,
  xhi = NULL, ylo = NULL, yhi = NULL, zlo = NULL, zhi = NULL,  tlo = NULL,
  thi = NULL, remove_HIERARCH = FALSE, force_logical = FALSE, bad = NULL,
  keypass = FALSE, zap = NULL, zaptype = 'full', sparse = 1L, scale_sparse = FALSE,
  collapse = FALSE, NAcheck = TRUE, nthreads = 1)

Rfits_write_image(data, filename = 'temp.fits', ext = 1, keyvalues, keycomments,
  keynames, comment, history, numeric = "single", integer = "long",
  create_ext = TRUE, create_file = TRUE, overwrite_file = TRUE, bzero = 0,
  bscale = 1, compress = FALSE, bad_compress = 0L) 

Rfits_write_vector(data, filename = 'temp.fits', ext = 1, keyvalues, keycomments,
  keynames, comment, history, numeric = "single", integer = "long",
  create_ext = TRUE, create_file = TRUE, overwrite_file = TRUE, bzero = 0,
  bscale = 1, compress = FALSE, bad_compress = 0L) 
    
Rfits_write_cube(data, filename = 'temp.fits', ext = 1, keyvalues, keycomments,
  keynames, comment, history, numeric = "single", integer = "long",
  create_ext = TRUE, create_file = TRUE, overwrite_file = TRUE, bzero = 0,
  bscale = 1, compress = FALSE, bad_compress = 0L) 
    
Rfits_write_array(data, filename = 'temp.fits', ext = 1, keyvalues, keycomments,
  keynames, comment, history, numeric = "single", integer = "long",
  create_ext = TRUE, create_file = TRUE, overwrite_file = TRUE, bzero = 0,
  bscale = 1, compress = FALSE, bad_compress = 0L)
    
Rfits_create_image(data, keyvalues = NULL, keycomments = NULL, comment = NULL,
  history = NULL, filename = '', ext = 1, keypass = FALSE, ...)
    
Rfits_write_pix(data, filename, ext = 1, xlo = 1L, ylo = 1L, zlo = 1L, tlo = 1L)

Rfits_blank_image(filename, ext = 1, create_ext = TRUE, create_file = TRUE,
  overwrite_file=TRUE, bitpix = -32, naxis = 2, naxis1 = 100, naxis2 = 100, naxis3 = 1,
  naxis4 = 1)
  
Rfits_check_image(data, keypass = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Rfits_vector / Rfits_image / Rfits_cube / Rfits_array data to be processed; for some functions can also be numeric or integer matrix (i.e. image), vector, cube or 4D array (\code{Rfits_create_image}). The matrix can also contain 64 bit integers via the \code{bit64} package. Note that \code{R} allows for integer-like behaviour beyond 2^31 (up to around 2^53) via the use of the mantissa (significance) part of the numeric double type. If working with something like a segmentation map that flows over 2^31, you should explicitly convert this to a 64 bit integer via the \code{bit64} package before saving.
}
  \item{filename}{
Character scalar; path to FITS file, either to be written to or read. There is good support for compressed reading and limited support for compressed writing (see Examples). You can use the filename to compress the target image by appending FITS relevant compression flags such as [compress] (uses RICE) [compress GZIP], [compress PLIO] and [compress HCOMPRESS]. E.g \option{filename} = 'file.fits' for writing a normal uncompressed images, or \option{filename} = 'file.fits[compress]' for writing a compressed image. These have additional parameters that can be passed in, see \url{https://heasarc.gsfc.nasa.gov/docs/software/fitsio/compression.html}.
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
Character scalar; the integer data type to be written out for images. Can be '8' (8 bit) 'byte' (8 bit), '16' (16 bit), 'short' (16 bit), 'int' (32 bit), '32' (32 bit), 'long' (32 bit), '64' (64 bit), 'longlong' (64 bit). If using '64' or 'longlong' you almost certainly need to have converted your integers in \option{data} to integer64 via the \code{bit64} package first.
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
Logical scalar, should the leading 'HIERARCH' be removed for extended keyword names (longer than 8 characters)?  
}
  \item{force_logical}{
Logical scalar; should an integer image be converted to a logical R image. All values not equal to 0L (including negative values) are TRUE, and all values equal to 0L are FALSE. This is really a presentational issue, since internally 0/FALSE and 1/TRUE and R synonyms, and they are both stored as full integers, so there is not even a storage advantage. The main difference is how is.logical and isTRUE/isFALSE behave (0L and 1L are FALSE for all).
}
  \item{bad}{
Scalar; replacement scalar (e.g. NA, NaN etc) for all non-finite values. This can be useful since CFITSIO converts R NA to NaN when writing and reading, and within R NA is usually the preferable representation.
}
  \item{keypass}{
Logical; if you have the Rwcs package installed, then you can execute an extra keypass step where the \option{keyvalues} provided have various safety checks made, and standard mistakes are corrected to make a fully FITS legal WCS.  
}
  \item{zap}{
Character vector; optional unique strings to zap out of the header. These elements are passed through \code{\link{grep}} one at a time and all matches are removed. This is useful if there a problematic keywords you want to remove when passing raw headers into \code{Rwcs} functions. Be careful that the strings specified are quite unique and do not remove more of the header than intended, e.g. 'CRVAL' would remove all mentions of CRVAL1/2/3 etc. Useful tricks- for inclusive ranges you can use '[]', e.g. 'CD[1-2]_[1-2]' will match to CD1_1, CD1_2, CD2_1, CD2_2. See \code{\link{grep}} for more information on how you pattern match within strings. For HST data we have often need to use \option{zap} = c('LOOKUP', 'DP[1-2]') because of the presence of non-compliant WCS keywords. By default the full 80 character header per key is scanned for matches, but this can be changed with \option{zaptype}.
}
  \item{zaptype}{
Character scalar; the type of zap to carry out. Either the grep is done on the full 80 character string ('full'; the default) or just the keynames ('name'). The latter might be useful when you are certain a simpler grep can be made if just using the keynames. The former is useful if some other flag in the key entry (a comment etc) can help flag keys to be zapped.  
}
  \item{sparse}{
Integer scalar; default reads in a dense matrix (so all pixels in the rectangular region requested). Setting sparse to higher values means few pixels are read in, i.e. it skips every \option{sparse} pixels in each dimension. This can be useful for having a quick look at the data from a large image (since it is quick to read in). Note currently this is just for quick views of the pixels with a fairly rough WCS conversion, so do not treat with critical accuracy.
}
  \item{scale_sparse}{
Logical; should the image be scaled to compensate for sparse sampling (so roughly conserve flux). In an 2D image this would mean pixels are scaled by \option{sparse}^2, and more generally a N dimensional array is scaled by \option{sparse}^N. The default is to return just the native values unscaled.
}
  \item{collapse}{
Logical; should the array or cube be collapsed down dimensions if the 3rd/4th dimensions are 1L? If TRUE this means slices in cubes/arrays are converted to class Rfits_cube/Rfits_image as appropriate. Note the user needs to specify the dimension slice to be '1', so it is more specific than just relying on the dimension only having size 1. E.g. if ex is a 10x10x1 cube then ex[,,1,collapse=TRUE] would become a 10x10 image, but ex[,,,collapse=TRUE] would not, instead it would still return a 10x10x1 cube. This is really to stop users accidentally collapsing things, and deliberately forces active input (so a feature not a bug).
}
  \item{NAcheck}{
Logical; should the read in image be explictly checked for NAs? For very big images (10k x 10k +) this has a notable overhead, so if we know it can be avoided it perhaps should be. Otherwise things we would like to be NA could appear as NaN, which will still work for most purposes but not as clean.
}
  \item{create_ext}{
Logical scalar; should a new extension be created (TRUE) or not (FALSE). This will always be appended at the end of the FITS file (as per the FITS standard). If \option{create_ext} = TRUE then no matter what the \option{ext} argument is, the extension will be created at the end of the FITS file. If \option{create_ext} = FALSE then the extension specified by \option{ext} will be first deleted, and then the extension will be created at the end of the FITS file.
}
  \item{create_file}{
Logical scalar; should a new file be created (TRUE) or not (FALSE). If not then the file at \option{filename} must exist for extensions to be written to it. This is really here to ensure safe operation, i.e. if \option{create_file}=TRUE and \option{overwrite_file}=FALSE and there is a file already at the path given by \option{filename} then you will get an error.
}
  \item{overwrite_file}{
Logical scalar; if file exists at location \option{filename}, should it be overwritten (i.e. deleted and a fresh FITS file created)?
}
  \item{bzero}{
Numeric scalar; convenience BZERO zero level to apply to image, where VAL = (RAW x BSCALE) + BZERO, where VAL is what is seen in the R \code{matrix} or \code{array} and RAW is what you would find if inspecting the binary values stored on disk. If this is present in the \option{keyvalues} then the keyvalues$BZERO value takes priority.
}
  \item{bscale}{
Numeric scalar; convenience BSCALE scaling factor to apply to image, where VAL = (RAW x BSCALE) + BZERO, where VAL is what is seen in the R \code{matrix} or \code{array} and RAW is what you would find if inspecting the binary values stored on disk. If this is present in the \option{keyvalues} then the keyvalues$BSCALE value takes priority.
}
  \item{compress}{
Logical scalar or character scalar; a different mechanism for telling Rfits to compress a target image. Can either be TRUE which means it will use the standard CFITSIO compression (equivalent to \option{filename} = 'example.fits[compress]'), or you can provide the contents of the additional compression options that will be written within the square brackets, e.g. \option{compress} = 'GZIP' (equivalent to \option{filename} = 'example.fits[compress GZIP]'). Allowed values are 'RICE', 'GZIP', 'HCOMPRESS' and 'PLIO'.
}
  \item{bad_compress}{
Numeric scalar; non-numeric replacement value to use when compression is being done. CFITSIO does not work well with any of the usual R non-numeric values (NA, NaN, Inf etc) when compressing since it makes bad guesses at how to correctly re-scale the image for compression. This flag offers a simple numeric value to replace all non-numeric elements with when compression is requested (so it is not applied for uncompressed image writing).
}
  \item{bitpix}{
Integer scalar; the bitpix for the blank image (8/16/32/64 for integer, and -32/-64 for float).  
}
  \item{naxis}{
Integer scalar; number of dimensions for the blank image.
}
  \item{naxis1}{
Integer scalar; pixels in dimension 1 for the blank image.
}
  \item{naxis2}{
Integer scalar; pixels in dimension 2 for the blank image.
}
  \item{naxis3}{
Integer scalar; pixels in dimension 3 for the blank image.
}
  \item{naxis4}{
Integer scalar; pixels in dimension 4 for the blank image.
}
  \item{nthreads}{
Number of threads used to read images in parallel.
}
  \item{\dots}{
Other arguments to pass to Rwcs_keypass.
}
}

\details{
R FITS readers and writers that uses CFITSIO to do the interfacing with files. Requires \code{bit64} package for integer64 (i.e. long long) to work.

\code{Rfits_read_xxx} reads in data from a FITS file. This can be either a vector, image, cube or 4D array. The different variants actually all point to \code{Rfits_read_image} internally, but for code clarity it is often good to make the data type more explicit.

\code{Rfits_write_xxx} writes vector, image, cube or 4D array data to a FITS file. The different variants actually all point to \code{Rfits_write_image} internally, but for code clarity it is often good to make the data type more explicit. Note that when writing extensions are always added onto the end of the current FITS file. If doing this you should use settings \option{overwrite_file} = FALSE (we do not want to delete the current file), \option{create_file} = FALSE (we do not want to create a new file), \option{create_ext} = FALSE (we do not want to simply append an extension to the end of the current file), \option{ext} = N (we want to delete extension N, and add a new one to the end of the file).

When reading, if \option{header}=TRUE (default) then the output of \option{Rfits_write_image} is a list with components: \option{imDat}; the header list outputs created by \code{\link{Rfits_read_header}} (all at the same level); the \option{filename} and \option{ext}. The object returned is of class \option{Rfits_image}, and has class dependent \code{print} and \code{dim} functions.

When reading, if \option{header}=FALSE then then the output is just the raw image matrix or cube array. You can either overwrite a current file or create a new extension to an already present FITS file. 
}
\note{
You can either specify BZERO and BSCALE through the input \option{bzero} and \option{bscale} function parameters, or put them in the \option{keyvalues} header list (with matching entry in \option{keycomments}). Be careful to remove the BZERO and BSCALE values from the \option{keyvalues} header list if you wish to use the function interface, since the \option{keyvalues} header values (if present) always take precedence (see Examples).

In general, you need to be very when working with integer64 types. Such data should read in fine, but you will find some simple operations (like sub-setting and some mathematical operations) might suddenly coerce it to a numeric type. This is because integer64 is not a first-class citizen in \code{R} world (it is a bolted on package), so it can break in unexpected places.
}
\value{
\code{Rfits_read_xxx}: List; read vector/image/cube/array into numeric or integer vector/matrix/array (\option{imDat}) and also header information as per \code{\link{Rfits_read_header}}. Also provides the \option{filename}, \option{ext}, \option{extname} and \option{WCSref} present.

\code{Rfits_write_xxx} write out vector/image/cube/array to target FITS extension. For convenience it also silently returns a list with the \option{filename} / \option{ext} / \option{naxis} / \option{naxes} (vector of relevant \option{naxis[1-4]}).

\code{Rfits_create_image} create an Rfits class object from user inputs. In this context \option{data} should be a normal R vector / matrix / cube / array. The output object returned will be of class Rfits_vector / Rfits_image / Rfits_cube / Rfits_array, and can be used like a standard FITS file internally, and can also be written out as one.

\code{Rfits_write_pix} is a low level function to write simply the pixel data to an already created FITS files. Note, bad things will happen if the target extension does not match the data type being written (e.g. you are trying to write a matrix of floats to a matrix of integers or the data sizes do not match). In general users should never use this functionality, since it removes almost all the safety features present in the higher level routines above (that guarantee the header and data tally). However, it does offer a method to work with larger than memory data on disk.

\code{Rfits_blank_image} is a low level function to create a blank (filled with 0) image on disk. Note, bad things will happen if the target extension does not match the data type being written (e.g. you are trying to write a matrix of floats to a matrix of integers or the data sizes do not match). In general users should never use this functionality, since it removes almost all the safety features present in the higher level routines above (that guarantee the header and data tally). However, it does offer a method to work with larger than memory data on disk. For convenience it also silently returns a list with the \option{filename} / \option{ext} / \option{naxis} / \option{naxes} (vector of relevant \option{naxis[1-4]}).

\code{Rfits_check_image} is a high level function to check the consistency of the provided Rfits_vector / Rfits_image / Rfits_cube / Rfits_array class object, and fix issues where possible. The keyvalues list item takes precedence over other (perhaps conflicting) versions of the header. Issues checked: the keynames not correctly matching the keyvalues; the keycomments not all being present (blank comments will be created where possible). It also guarantees the various other header items (hdr, header, raw) are completely consistent with the keyvalues. This means you can modify keyvalues and pass a target Rfits_image through this function before e.g. warping etc.
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
library(Rwcs)
library(magicaxis)
#Read full image
file_image = system.file('extdata', 'image.fits', package = "Rfits")
temp_image = Rfits_read_image(file_image)

#For convenience we can access the matrix part directly
temp_image$imDat[1:5,1:5] #explicit
temp_image[1:5,1:5,header=FALSE] #convenient

#And we can print and check dim easily
print(temp_image)
dim(temp_image)

#Take a look
plot(temp_image)

#And again with just a subset
temp_image_sub = Rfits_read_image(file_image, xlo=150, xhi=250, ylo=100, yhi=200)
plot(temp_image_sub)

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
Rfits_write_image(temp_image$imDat, file_image_temp, compress=TRUE)
file.size(file_image_temp) #about a factor 5 compressed
temp_image3 = Rfits_read_image(file_image_temp, ext=2)
magimage(temp_image3$imDat)

#Sparse sample reading (notice the WCS is modified as needed)
plot(Rfits_read_image(file_image, sparse=4))
plot(Rfits_read_image(file_image, sparse=8))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\concept{ FITS }% use one of  RShowDoc("KEYWORDS")
