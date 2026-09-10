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
\alias{Rfits_append_vector_zarr}
\alias{Rfits_append_image_zarr}
\alias{Rfits_append_cube_zarr}
\alias{Rfits_append_array_zarr}
\alias{Rfits_point_zarr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FITS Style Zarr Vector Image Cube and Array Readers and Writers
}
\description{
Image readers and writers that use Zarr for the storage back-end, rather than FITS. The read in files will look almost identical to those read in with \code{\link{Rfits_read_image}}, and mirror the behaviour of the HDF5 back-end in \code{\link{Rfits_image_hdf5}}.
}
\usage{
Rfits_read_image_zarr(filename = "temp.zarr", extname = "data1", ext = NULL, header =TRUE,
  xlo = NULL, xhi = NULL, ylo = NULL, yhi = NULL, zlo = NULL, zhi = NULL, tlo = NULL,
  thi = NULL, remove_HIERARCH = FALSE, force_logical = FALSE, collapse = FALSE)

Rfits_read_vector_zarr(filename = "temp.zarr", extname = "data1", ext = NULL, header=TRUE,
  xlo = NULL, xhi = NULL, ylo = NULL, yhi = NULL, zlo = NULL, zhi = NULL, tlo = NULL,
  thi = NULL, remove_HIERARCH = FALSE, force_logical = FALSE, collapse = FALSE)

Rfits_read_cube_zarr(filename = "temp.zarr", extname = "data1", ext = NULL, header = TRUE,
  xlo = NULL, xhi = NULL, ylo = NULL, yhi = NULL, zlo = NULL, zhi = NULL, tlo = NULL,
  thi = NULL, remove_HIERARCH = FALSE, force_logical = FALSE, collapse = FALSE)

Rfits_read_array_zarr(filename = "temp.zarr", extname = "data1", ext = NULL, header =TRUE,
  xlo = NULL, xhi = NULL, ylo = NULL, yhi = NULL, zlo = NULL, zhi = NULL, tlo = NULL,
  thi = NULL, remove_HIERARCH = FALSE, force_logical = FALSE, collapse = FALSE)

Rfits_write_image_zarr(data, filename = 'temp.zarr', extname = 'data1', create_ext = TRUE,
  overwrite_file = FALSE, data_type = NULL, chunk_shape = NULL, clevel = 6L,
  compressor = 'blosc', shuffle = NULL, append = FALSE, update_root = TRUE,
  keyvalues, keycomments, keynames, comment, history)

Rfits_write_vector_zarr(data, filename = 'temp.zarr', extname = 'data1', create_ext =TRUE,
  overwrite_file = FALSE, data_type = NULL, chunk_shape = NULL, clevel = 6L,
  compressor = 'blosc', shuffle = NULL, append = FALSE, update_root = TRUE,
  keyvalues, keycomments, keynames, comment, history)

Rfits_write_cube_zarr(data, filename = 'temp.zarr', extname = 'data1', create_ext = TRUE,
  overwrite_file = FALSE, data_type = NULL, chunk_shape = NULL, clevel = 6L,
  compressor = 'blosc', shuffle = NULL, append = FALSE, update_root = TRUE,
  keyvalues, keycomments, keynames, comment, history)

Rfits_write_array_zarr(data, filename = 'temp.zarr', extname = 'data1', create_ext = TRUE,
  overwrite_file = FALSE, data_type = NULL, chunk_shape = NULL, clevel = 6L,
  compressor = 'blosc', shuffle = NULL, append = FALSE, update_root = TRUE,
  keyvalues, keycomments, keynames, comment, history)

Rfits_append_image_zarr(data, filename = 'temp.zarr', extname = 'data1', ...)

Rfits_append_vector_zarr(data, filename = 'temp.zarr', extname = 'data1', ...)

Rfits_append_cube_zarr(data, filename = 'temp.zarr', extname = 'data1', ...)

Rfits_append_array_zarr(data, filename = 'temp.zarr', extname = 'data1', ...)

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
Integer scalar between 0 and 9; the blosc compression level to apply. Note that zarr's own default is 1, whereas this is 6, because a FITS archive is written rarely and read often. Whatever is applied is what gets recorded in the store metadata, so the two cannot disagree.
}
  \item{compressor}{
Character scalar; the blosc compressor to use. One of \code{'blosc'} (the default, which means \code{'zstd'}, the zarr default), \code{'blosclz'}, \code{'lz4'}, \code{'lz4hc'}, \code{'zstd'}, \code{'zlib'} or \code{'gzip'} (an alias for \code{'zlib'}). The names \code{'bz2'} and \code{'lzma'} are accepted but cannot be produced by the blosc codec, so they warn and fall back to \code{'zstd'} rather than being recorded under a name that does not match the bytes on disk. The compressor of an existing array cannot be changed by an append, which says so rather than doing nothing.
}
  \item{shuffle}{
Logical or character scalar; the blosc shuffle to apply. Either \code{TRUE}/\code{FALSE} (which map to \code{'shuffle'}/\code{'noshuffle'}) or one of \code{'shuffle'}, \code{'noshuffle'}, \code{'bitshuffle'}. The default, NULL, leaves the choice to zarr, which picks a shuffle suited to the data type.
}
  \item{append}{
Logical scalar; if TRUE the data is appended along dimension 1 of an existing array rather than replacing it. See Details.
}
  \item{update_root}{
Logical scalar; if TRUE (default) the store level description is rebuilt after a successful write. Set FALSE when writing many arrays in a loop, and then do at least one final write with it left at TRUE, since the refresh reads the metadata of every array in the store. Note that \code{\link{Rfits_inspect_zarr}} only reports what is there, it never repairs it.
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
  \item{...}{
For the \code{Rfits_append_xxx_zarr} wrappers, any of the other arguments of
\code{Rfits_write_image_zarr} except \option{append}, which is always set to TRUE.
}
}
\details{
These functions mimic a FITS like data structure using Zarr. A Zarr store is a directory rather than a single file, and each extension is stored as an array node inside it. The FITS style meta data is stored in the attributes of each array node, in \code{zarr.json} alongside the array metadata, under the names \option{header} (a character vector of 80 column FITS card images), \option{keyvalues}, \option{keycomments}, \option{comment} and \option{history}.

The card images are authoritative; on read they are parsed in exactly the same way as the HDF5 back-end parses its stored header, so a file written with \code{Rfits_write_image_zarr} can be read back by any tool that understands Zarr attributes. The structured keyword lists are stored as well, so that non-R consumers can reach the metadata directly without having to parse FITS cards.

Subsets are read by asking Zarr for only the requested chunk block, so cutting a small region out of a large cube does not require loading the whole array. Because missing data is represented by the array fill value, the types available in \option{data_type} each have their own missing value marker, and pixels which happen to equal that marker will read back as NA (a warning is issued when writing integer data that contains the marker).

\strong{Store self-description}

A Zarr store has no header of its own, so the root group carries a summary of everything the store holds: the array names, and for each array the shape, chunk shape, data type, codec, compressor, level, shuffle and fill value. Each array additionally carries those same technical parameters as its own attributes. Every recorded value is read back off the array that was created rather than echoed from the request, so the metadata cannot claim a compression setting that was never applied.

The key names follow the wider images-to-Zarr convention where the meaning transfers, so a foreign reader can at least locate the arrays. Two differences matter. \code{convention} is always \code{'Rfits.zarr'}, which is what distinguishes these stores from a converter output. And \code{total_images} is the total number of \emph{elements} across all arrays rather than a count of images, since a single FITS file may hold one large cube; the per-array truth is in \code{images_shape}.

Stores written before this existed have no root description, which is reported honestly rather than filled in, and is repaired by the next write. Reading a store that advertises the images-to-Zarr keys without the \code{convention} marker produces a message saying the FITS headers were never stored, rather than the generic warning about missing metadata.

\strong{Appending}

With \option{append} = TRUE the data is the increment, not the complete final array: writing \code{3 x 6} to an existing \code{4 x 6} array produces a \code{7 x 6} array. Growth happens along dimension 1 only, which is the slowest varying axis in FITS, R and Zarr alike, so the elements already in the store keep their indices. Every other dimension must match exactly, and the rank must not change.

The stored data type is never widened or narrowed: an unrequested type difference is coerced to the existing array's type with a message, while an explicit \option{data_type} that disagrees is an error. The compressor of an existing array cannot be changed, which is reported rather than ignored. \option{NAXIS1} (or \option{ZNAXIS1} for a compressed image) is updated to follow the array, and a HISTORY line records the event on the array and in the root's \code{append_history}. Keywords already in the header win over any supplied with the append, since the alternative would be silently rewriting the WCS of a whole extension. Appending to an array with no FITS metadata leaves nothing invented behind: the array grows, and a message says no keys were updated.

\strong{Remote stores}

Wherever \option{filename} is expected, a store object created by \code{\link[zarr]{zarr_s3store}} (or \code{zarr_localstore}, \code{zarr_memorystore}, \code{zarr_httpstore}) may be given instead of a path. This is the only way to reach a store that needs credentials, because \code{\link[zarr]{open_zarr}} builds the store itself and cannot be passed one, so it has no way to accept a key or a non-AWS endpoint. The store must already be configured and authenticated; Rfits never sees a secret key. An open \code{zarr} object may also be given, so the result of \code{zarr::zarr$new(store)} can be reused across calls.

For a store object, the \option{filename} component of the output holds the store URI (for example \code{s3://bucket/prefix/}) rather than a local path, and falls back to a description of the store type when there is no URI. A writer will bootstrap a store that has no root group yet, which is the state a newly created local or memory store is in, but a reader will not: reading refuses rather than creating anything, so an empty store is never mistaken for a populated one and nothing can be written over existing data. Note that \code{zarr_s3store} cannot itself be constructed over a prefix that holds no store, so a brand new remote store has to be created by other means first.

Writes require a store created with \option{read_only} = FALSE; that flag cannot be changed afterwards, so the error is reported rather than a silently failing write. Because there is no directory to remove, \option{overwrite_file} = TRUE empties a store object in place using its \code{clear} method. Note that subsetting over a remote store is only as efficient as the store's chunking, and that \code{Rfits_write_image_zarr} on a remote store transfers the whole array.
}
\value{
\code{Rfits_read_xxx_zarr} reads image data from a Zarr store. The different variants actually all point to \code{Rfits_read_image_zarr} internally, but for code clarity it is often good to make the data type more explicit.

\code{Rfits_write_xxx_zarr} writes image data to a Zarr store, adding it as a new array node (or replacing the node of that name if it already exists). The different variants actually all point to \code{Rfits_write_image_zarr} internally, but for code clarity it is often good to make the data type more explicit.

Invisibly, a list with components \option{filename}, \option{extname}, \option{dim}, \option{data_type}, \option{chunk_shape}, \option{compressor} and \option{compression_level}. The last two are the values applied to the array as created, which is not necessarily what was asked for.

\code{Rfits_append_xxx_zarr} is \code{Rfits_write_image_zarr} called with \option{append} = TRUE. Its return value additionally carries \option{start_index}, \option{end_index} and \option{appended}, so the range that was written is known without reading the array back.

\code{\link{Rfits_inspect_zarr}} summarises a store without reading any pixel data.

\code{Rfits_point_zarr} returns an object of class \option{Rfits_pointer_zarr} without reading any data. Subsetting that object with \code{[} reads only the requested region, which is the point of a pointer.

Subsetting an \option{Rfits_pointer_zarr} mirrors \code{\link{Rfits_image}} (and so \code{\link{Rfits_point}}), so the same calls work whichever back-end the pointer was made from:

\itemize{
\item \option{i} and \option{j} may be ranges (\code{p[50:150, 50:150]}), a pair of bounds given as a vector (\code{p[c(50,150), c(60,160)]}), or a single location to centre a box on.
\item \code{a:end} reads from \code{a} to the end of that dimension, and works for \option{i}, \option{j}, \option{k} and \option{m}.
\item \option{box} sets the size of the cutout around a single location, with \option{box}[1] for x and \option{box}[2] for y (a scalar is used for both). With \option{i} and \option{j} omitted, the box is centred on the image.
\item \option{type} = 'coord' treats \option{i} and \option{j} as RA and Dec rather than pixels, which needs \code{Rwcs} and requires both to be length 1.
\item \option{collapse} drops a trailing dimension that the caller sliced down to a single value, turning a cube slice into an \option{Rfits_image} and so on.
}

Two FITS pointer features are deliberately not available here, because the Zarr reader has no equivalent: matrix indexing for random access (\code{p[cbind(x, y)]}), which is rejected with an error rather than silently returning the wrong shape, and \option{sparse} / \option{scale_sparse} subsampling.
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

#Read a region of a store object without reading the whole array
cut <- Rfits_read_image_zarr(store, extname = "data1", xlo = 101, xhi = 200, ylo = 301, yhi = 400)
cat("dim:", dim(cut$imDat), "\n")

#The same region via a local file, working from the package example image
file_image = system.file('extdata', 'image.fits', package = "Rfits")
temp_image = Rfits_read_image(file_image)

#write it out
file_image_temp = file.path(tempdir(), 'image.zarr')
Rfits_write_image_zarr(temp_image, file_image_temp, extname = 'image')

#Read it in again
temp_image_zarr = Rfits_read_image_zarr(file_image_temp, extname = 'image')

#Check
sum(temp_image$imDat - temp_image_zarr$imDat)

#Append to an existing extension. The data is the increment, so this grows the
#array along dimension 1 rather than replacing it
first = matrix(as.numeric(1:24), 4, 6)
Rfits_write_image_zarr(first, file_image_temp, extname = 'stack',
                       keyvalues = temp_image$keyvalues)
grown = Rfits_write_image_zarr(matrix(as.numeric(101:112), 2, 6), file_image_temp,
                               extname = 'stack', append = TRUE)
grown$start_index
grown$end_index
dim(Rfits_read_image_zarr(file_image_temp, extname = 'stack')$imDat)

#Choose a compressor; what is applied is what gets recorded
Rfits_write_image_zarr(first, file_image_temp, extname = 'fast', compressor = 'lz4',
                       clevel = 3L)
Rfits_inspect_zarr(file_image_temp, print = FALSE)$arrays$fast

#A pointer, which only reads what you ask of it
point = Rfits_point_zarr(file_image_temp, extname = 'image')
point
small = point[c(1, 10), c(1, 10)]

#Subsetting works the same as for a FITS pointer
dim(point[50:150, 50:150])      #a range
dim(point[c(50, 150), c(60, 160)]) #the same, given as bounds
dim(point[101, 101, box = 100]) #a box centred on a location
dim(point[box = 50])            #a box centred on the image
dim(point[50:end])              #from 50 to the end of that dimension

#By RA/Dec rather than pixels, which needs Rwcs
library(Rwcs)
coords = Rwcs_p2s(101, 101, temp_image$keyvalues)
dim(point[coords[1], coords[2], box = 100, type = 'coord'])

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
