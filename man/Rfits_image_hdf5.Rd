\name{Rfits_image_hdf5}
\alias{Rfits_image_hdf5}
\alias{Rfits_read_vector_hdf5}
\alias{Rfits_read_image_hdf5}
\alias{Rfits_read_cube_hdf5}
\alias{Rfits_read_array_hdf5}
\alias{Rfits_write_vector_hdf5}
\alias{Rfits_write_image_hdf5}
\alias{Rfits_write_cube_hdf5}
\alias{Rfits_write_array_hdf5}
\alias{Rfits_point_hdf5}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FITS Style HDF5 Vector Image Cube and Array Readers and Writers
}
\description{
Image readers and writers that use HDF5 for the storage back-end, rather than FITS. The read in files will look almost identical to those read in with \code{\link{Rfits_read_image}}.
}
\usage{
Rfits_read_image_hdf5(filename = "temp.h5", extname = "data1", ext = NULL, header = TRUE,
  xlo = NULL, xhi = NULL, ylo = NULL, yhi = NULL, zlo = NULL, zhi = NULL,  tlo = NULL,
  thi = NULL, remove_HIERARCH = FALSE, force_logical = FALSE)

Rfits_read_vector_hdf5(filename = "temp.h5", extname = "data1", ext = NULL, header = TRUE,
  xlo = NULL, xhi = NULL, ylo = NULL, yhi = NULL, zlo = NULL, zhi = NULL,  tlo = NULL,
  thi = NULL, remove_HIERARCH = FALSE, force_logical = FALSE)

Rfits_read_cube_hdf5(filename = "temp.h5", extname = "data1", ext = NULL, header = TRUE,
  xlo = NULL, xhi = NULL, ylo = NULL, yhi = NULL, zlo = NULL, zhi = NULL,  tlo = NULL,
  thi = NULL, remove_HIERARCH = FALSE, force_logical = FALSE)

Rfits_read_array_hdf5(filename = "temp.h5", extname = "data1", ext = NULL, header = TRUE,
  xlo = NULL, xhi = NULL, ylo = NULL, yhi = NULL, zlo = NULL, zhi = NULL,  tlo = NULL,
  thi = NULL, remove_HIERARCH = FALSE, force_logical = FALSE) 

Rfits_write_image_hdf5(data, filename = 'temp.h5', extname = 'data1', create_ext = TRUE,
  overwrite_file = FALSE)
  
Rfits_write_vector_hdf5(data, filename = 'temp.h5', extname = 'data1', create_ext = TRUE,
  overwrite_file = FALSE)
  
Rfits_write_cube_hdf5(data, filename = 'temp.h5', extname = 'data1', create_ext = TRUE,
  overwrite_file = FALSE)
  
Rfits_write_array_hdf5(data, filename = 'temp.h5', extname = 'data1', create_ext = TRUE,
  overwrite_file = FALSE)
  
Rfits_point_hdf5(filename = 'temp.h5', extname = 'data1', ext = NULL, header = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Numeric or integer matrix (i.e. image), vector, cube or 4D array; data to be written out as a HDF5 image. For convenience you can pass the output of \code{Rfits_read_image} directly into this argument and it will split out the \option{imDat}, and various header inputs as required (if you also directly provide a header related input, this takes precedents.
}
  \item{filename}{
Character scalar; path to HDF5 file, either to be written to or read.
}
  \item{extname}{
Character scalar; name of the extension to be read in.
}
  \item{ext}{
Integer scalar; the extension to read. If proved this takes precedence over \option{extname}.
}
  \item{header}{
Logical; should the header be read along with the image matrix. If TRUE (default) then the output is a list with components \option{imDat}, and the header list outputs created by \code{\link{Rfits_read_header}} (all at the same level), if FALSE then it is just the raw image matrix.
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
  \item{create_ext}{
Logical scalar; should a new extension be created (TRUE) or not (FALSE).
}
  \item{overwrite_file}{
Logical scalar; if file exists at location \option{filename}, should it be overwritten (i.e. deleted and a fresh HSF5 file created)?
}
}
\details{
This funcation attempted to mimic a FITS like data stucture using HDF5, making use to the attributes of the group for writing the header.
}
\value{
\code{Rfits_read_xxx_hdf5} reads in image data from a HDF5 file. The different variants actually all point to \code{Rfits_read_image_hdf5} internally, but for code clarity it is often good to make the data type more explicit.

\code{Rfits_write_xxx_hdf5} writes image data to a HDF5 file. This function will not allow you to write an extension with the same name as one that is currently present (you will need to manually delete it). The different variants actually all point to \code{Rfits_write_image_hdf5} internally, but for code clarity it is often good to make the data type more explicit.
}
\author{
Aaron Robotham
}
\note{
These functions are a proof of concept for creating more generic astronomy data formats that still have meta data.
}

\seealso{
\code{\link{Rfits_image}}
}
\examples{
\dontrun{
library(hdf5r)

#Read full image
file_image = system.file('extdata', 'image.fits', package = "Rfits")
temp_image = Rfits_read_image(file_image)

#write it out
file_image_temp = tempfile()
Rfits_write_image_hdf5(temp_image, file_image_temp)

#Read it in again
temp_image_hdf5 = Rfits_read_image_hdf5(file_image_temp)

#Check
sum(temp_image$imDat - temp_image_hdf5$imDat)
}
}

\concept{ HDF5 }
