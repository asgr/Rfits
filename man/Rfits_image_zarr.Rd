\name{Rfits_image_zarr}
\alias{Rfits_image_zarr}
\alias{Rfits_read_vector_zarr}
\alias{Rfits_read_image_zarr}
\alias{Rfits_read_cube_zarr}
\alias{Rfits_read_array_zarr}
\alias{Rfits_write_vector_zarr}
\alias{Rfits_write_image_zarr}
\alias{Rfits_write_cube_zarr}
\alias{Rfits_write_array_zarr}
\alias{Rfits_point_zarr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FITS Style Zarr Vector Image Cube and Array Readers and Writers
}
\description{
Image readers and writers that use Zarr for the storage back-end, rather than FITS. The read in files will look almost identical to those read in with \code{\link{Rfits_read_image}}, and mirror the behaviour of the HDF5 back-end in \code{\link{Rfits_image_hdf5}}.
}
\usage{
Rfits_read_image_zarr(filename = "temp.zarr", extname = "data1", ext = NULL, header = TRUE,
  xlo = NULL, xhi = NULL, ylo = NULL, yhi = NULL, zlo = NULL, zhi = NULL, tlo = NULL,
  thi = NULL, remove_HIERARCH = FALSE, force_logical = FALSE, collapse = FALSE)

Rfits_read_vector_zarr(filename = "temp.zarr", extname = "data1", ext = NULL, header = TRUE,
  xlo = NULL, xhi = NULL, ylo = NULL, yhi = NULL, zlo = NULL, zhi = NULL, tlo = NULL,
  thi = NULL, remove_HIERARCH = FALSE, force_logical = FALSE, collapse = FALSE)

Rfits_read_cube_zarr(filename = "temp.zarr", extname = "data1", ext = NULL, header = TRUE,
  xlo = NULL, xhi = NULL, ylo = NULL, yhi = NULL, zlo = NULL, zhi = NULL, tlo = NULL,
  thi = NULL, remove_HIERARCH = FALSE, force_logical = FALSE, collapse = FALSE)

Rfits_read_array_zarr(filename = "temp.zarr", extname = "data1", ext = NULL, header = TRUE,
  xlo = NULL, xhi = NULL, ylo = NULL, yhi = NULL, zlo = NULL, zhi = NULL, tlo = NULL,
  thi = NULL, remove_HIERARCH = FALSE, force_logical = FALSE, collapse = FALSE)

Rfits_write_image_zarr(data, filename = 'temp.zarr', extname = 'data1', create_ext = TRUE,
  overwrite_file = FALSE, data_type = NULL, chunk_shape = NULL, clevel = 6L,
  keyvalues, keycomments, keynames, comment, history)

Rfits_write_vector_zarr(data, filename = 'temp.zarr', extname = 'data1', create_ext = TRUE,
  overwrite_file = FALSE, data_type = NULL, chunk_shape = NULL, clevel = 6L,
  keyvalues, keycomments, keynames, comment, history)

Rfits_write_cube_zarr(data, filename = 'temp.zarr', extname = 'data1', create_ext = TRUE,
  overwrite_file = FALSE, data_type = NULL, chunk_shape = NULL, clevel = 6L,
  keyvalues, keycomments, keynames, comment, history)

Rfits_write_array_zarr(data, filename = 'temp.zarr', extname = 'data1', create_ext = TRUE,
  overwrite_file = FALSE, data_type = NULL, chunk_shape = NULL, clevel = 6L,
  keyvalues, keycomments, keynames, comment, history)

Rfits_point_zarr(filename = 'temp.zarr', extname = 'data1', ext = NULL, header = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Numeric or integer matrix (i.e. image), vector, cube or 4D array; data to be written out as a Zarr image. For convenience you can pass the output of \code{Rfits_read_image} directly into this argument and it will split out the \option{imDat}, and various header inputs as required (if you also directly provide a header related input, this takes precedence).
}
  \item{filename}{
Character scalar; path to the Zarr store, either to be written to or read. Unlike FITS and HDF5, a Zarr file is a directory. A store object may be given instead of a path (see Details), in which case it is used directly.
}
  \item{extname}{
Character scalar; name of the extension to be read in, which is the name of the array node within the store. Must be a valid Zarr node name (no \code{/} or spaces).
}
  \item{ext}{
Integer scalar; the extension to read, counted over the arrays in the store sorted by name. If provided this takes precedence over \option{extname}.
}
  \item{header}{
Logical; should the header be read along with the image matrix. If TRUE (default) then the output is a list with components \option{imDat}, and the header list outputs created by \code{\link{Rfits_read_header}} (all at the same level), if FALSE then it is just the raw image matrix.
}
  \item{xlo}{
Integer scalar; low x limit (usually Right Ascension axis) to cut out from \option{image} (1 or larger and less than \option{xhi}).
}
  \item{xhi}{
Integer scalar; high x limit (usually Right Ascension axis) to cut out from \option{image} (NAXIS1 or smaller and greater than \option{xlo}).
}
  \item{ylo}{
Integer scalar; low y limit (usually Declination axis) to cut out from \option{image} (1 or larger and less than \option{yhi}).
}
  \item{yhi}{
Integer scalar; high y limit (usually Declination axis) to cut out from \option{image} (NAXIS2 or smaller and greater than \option{ylo}).
}
  \item{zlo}{
Integer scalar; low z limit (usually Spectral axis) to cut out from \option{image} (1 or larger and less than \option{zhi}).
}
  \item{zhi}{
Integer scalar; high z limit (usually Spectral axis) to cut out from \option{image} (NAXIS3 or smaller and greater than \option{zlo}).
}
  \item{tlo}{
Integer scalar; low t limit (usually Temporal axis) to cut out from \option{image} (1 or larger and less than \option{thi}).
}
  \item{thi}{
Integer scalar; high t limit (usually Temporal axis) to cut out from \option{image} (NAXIS4 or smaller and greater than \option{tlo}).
}
  \item{remove_HIERARCH}{
Logical scalar, should the leading 'HIERARCH' be removed for extended keyword names (longer than 8 characters)?
}
  \item{force_logical}{
Logical scalar; should an integer image be converted to a logical R image. All values not equal to 0L (including negative values) are TRUE, and all values equal to 0L are FALSE.
}
  \item{collapse}{
Logical scalar; if the resulting image has trailing singleton dimensions should they be collapsed away (turning a cube into an image, or an array into a cube or image)?
}
  \item{create_ext}{
Logical scalar; should a new extension be created (TRUE) or not (FALSE). If TRUE and an extension of the same name already exists, it is replaced.
}
  \item{overwrite_file}{
Logical scalar; if a store exists at location \option{filename}, should it be overwritten (i.e. deleted and a fresh store created)? Note that unlike the HDF5 back-end this is easiest combined with \option{create_ext} = FALSE.
}
  \item{data_type}{
Character scalar; the Zarr data type to store the data as. One of \code{'int8'}, \code{'int16'}, \code{'int32'}, \code{'uint8'}, \code{'uint16'}, \code{'float32'}, \code{'float64'} or \code{'bool'}. The default, NULL, is chosen from the R type of \option{data}, giving \code{'float64'} for numeric, \code{'int32'} for integer and \code{'int8'} for logical data. Logical data default to \code{'int8'} rather than \code{'bool'} because Zarr bool arrays use FALSE as their missing value marker and so cannot store NA.
}
  \item{chunk_shape}{
Integer vector; the chunk shape to use when storing the array, with one entry per dimension, each between 1 and the size of that dimension. The default, NULL, uses \code{\link[zarr]{optimal_chunking}}, which targets roughly 4 MB chunks. Chunks are what make partial reads of large archives efficient.
}
  \item{clevel}{
Integer scalar between 0 and 9; the blosc compression level to apply.
}
  \item{keyvalues}{
Named list; keyword values to store, as produced by \code{\link{Rfits_read_header}}. Only needed for data which is not already an \option{Rfits_image} style object.
}
  \item{keycomments}{
Named list; keyword comments, matching \option{keyvalues}.
}
  \item{keynames}{
Character vector; keyword names. Currently derived from \option{keyvalues}, and accepted for signature compatibility with \code{\link{Rfits_write_image}}.
}
  \item{comment}{
Character vector; COMMENT lines to store.
}
  \item{history}{
Character vector; HISTORY lines to store.
}
}
\details{
These functions mimic a FITS like data structure using Zarr. A Zarr store is a directory rather than a single file, and each extension is stored as an array node inside it. The FITS style meta data is stored in the attributes of each array node, in \code{zarr.json} alongside the array metadata, under the names \option{header} (a character vector of 80 column FITS card images), \option{keyvalues}, \option{keycomments}, \option{comment} and \option{history}.

The card images are authoritative; on read they are parsed in exactly the same way as the HDF5 back-end parses its stored header, so a file written with \code{Rfits_write_image_zarr} can be read back by any tool that understands Zarr attributes. The structured keyword lists are stored as well, so that non-R consumers can reach the metadata directly without having to parse FITS cards.

Subsets are read by asking Zarr for only the requested chunk block, so cutting a small region out of a large cube does not require loading the whole array. Because missing data is represented by the array fill value, the types available in \option{data_type} each have their own missing value marker, and pixels which happen to equal that marker will read back as NA (a warning is issued when writing integer data that contains the marker).

\strong{Remote stores}

Wherever \option{filename} is expected, a store object created by \code{\link[zarr]{zarr_s3store}} (or \code{zarr_localstore}, \code{zarr_memorystore}, \code{zarr_httpstore}) may be given instead of a path. This is the only way to reach a store that needs credentials, because \code{\link[zarr]{open_zarr}} builds the store itself and cannot be passed one, so it has no way to accept a key or a non-AWS endpoint. The store must already be configured and authenticated; Rfits never sees a secret key. An open \code{zarr} object may also be given, so the result of \code{zarr::zarr$new(store)} can be reused across calls.

For a store object, the \option{filename} component of the output holds the store URI (for example \code{s3://bucket/prefix/}) rather than a local path, and falls back to a description of the store type when there is no URI. A writer will bootstrap a store that has no root group yet, which is the state a newly created local or memory store is in, but a reader will not: reading refuses rather than creating anything, so an empty store is never mistaken for a populated one and nothing can be written over existing data. Note that \code{zarr_s3store} cannot itself be constructed over a prefix that holds no store, so a brand new remote store has to be created by other means first.

Writes require a store created with \option{read_only} = FALSE; that flag cannot be changed afterwards, so the error is reported rather than a silently failing write. Because there is no directory to remove, \option{overwrite_file} = TRUE empties a store object in place using its \code{clear} method. Note that subsetting over a remote store is only as efficient as the store's chunking, and that \code{Rfits_write_image_zarr} on a remote store transfers the whole array.
}
\value{
\code{Rfits_read_xxx_zarr} reads image data from a Zarr store. The different variants actually all point to \code{Rfits_read_image_zarr} internally, but for code clarity it is often good to make the data type more explicit.

\code{Rfits_write_xxx_zarr} writes image data to a Zarr store, adding it as a new array node (or replacing the node of that name if it already exists). The different variants actually all point to \code{Rfits_write_image_zarr} internally, but for code clarity it is often good to make the data type more explicit.

\code{Rfits_point_zarr} returns an object of class \option{Rfits_pointer_zarr} without reading any data. Subsetting that object with \code{[} reads only the requested region.
}
\author{
Aaron Robotham
}
\note{
These functions are a proof of concept for creating more generic astronomy data formats that still have meta data. Chunked Zarr stores are particularly well suited to remote or cloud based access, where the ability to read a single chunk without transferring a whole file matters.
}

\seealso{
\code{\link{Rfits_image}}, \code{\link{Rfits_image_hdf5}}, \code{\link[zarr]{create_zarr}}
}
\examples{
\dontrun{
library(zarr)

#Read full image
cut <- Rfits_read_image_zarr(store, extname = "data1", xlo = 101, xhi = 200, ylo = 301, yhi = 400)
cat("dim:", dim(cut$imDat), "\n")cut <- Rfits_read_image_zarr(store, extname = "data1", xlo = 101, xhi = 200, ylo = 301, yhi = 400)
cat("dim:", dim(cut$imDat), "\n") = system.file('extdata', 'image.fits', package = "Rfits")
temp_image = Rfits_read_image(file_image)

#write it out
file_image_temp = file.path(tempdir(), 'image.zarr')
Rfits_write_image_zarr(temp_image, file_image_temp, extname = 'image')

#Read it in again
temp_image_zarr = Rfits_read_image_zarr(file_image_temp, extname = 'image')

#Check
sum(temp_image$imDat - temp_image_zarr$imDat)

#A pointer, which only reads what you ask of it
point = Rfits_point_zarr(file_image_temp, extname = 'image')
point
small = point[c(1, 10), c(1, 10)]

#A remote store, given as a store object rather than a path. The store has to
#already exist, since zarr_s3store cannot be constructed over an empty prefix.
#The session_token must be a string, since passing NULL is mishandled by paws at
#the time of writing. Use an AWS_PROFILE, or keys held outside the code, rather
#than hard coding one.
store = zarr::zarr_s3store$new(
  bucket = 'my-bucket',
  prefix = 'images/example.zarr/',
  region = 'auto',
  endpoint = 'https://account.r2.cloudflarestorage.com',
  access_key = Sys.getenv('R2_KEY'),
  secret_key = Sys.getenv('R2_SECRET'),
  session_token = '')

#Reads only the chunks the cut needs, however far away the store is
image = Rfits_read_image_zarr(store, extname = 'data1', xlo = 1, xhi = 100, ylo = 1, yhi = 100)
image$filename
#Write back to the same store, creating the root group if it has none yet
Rfits_write_image_zarr(image, store, extname = 'cutout')
}
}

\concept{ Zarr }
