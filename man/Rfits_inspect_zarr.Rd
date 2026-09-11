\name{Rfits_inspect_zarr}
\alias{Rfits_inspect_zarr}
\title{
Summarise a FITS Style Zarr Store
}
\description{
Reports what a Zarr store written by \code{\link{Rfits_image_zarr}} contains: the
arrays it holds, their shape, chunking, data type and the compression actually
applied, the number of chunks and bytes each occupies, and the FITS metadata
summary for each. No pixel data is read, so the cost stays low even for a store
that is not local.
}
\usage{
Rfits_inspect_zarr(filename, print = TRUE, ...)
}
\arguments{
  \item{filename}{
Character scalar; path to the Zarr store, or a store object (see
\code{\link{Rfits_image_zarr}}).
}
  \item{print}{
Logical scalar; if TRUE (default) the summary is printed. The invisible return
value is produced either way.
}
  \item{...}{
Currently ignored.
}
}
\details{
Everything reported is read from the store metadata and the sizes of the chunk
keys, never from the arrays themselves. In particular no minimum, maximum or mean
of the pixel values is computed, because for a remote store that would require
transferring the whole array, which is the thing the Zarr back end exists to
avoid.

The store level self-description is reported if present, and its absence is
stated plainly rather than worked around. A store with no self-description is
either one written by an older Rfits, or one not written by Rfits at all; a store
that advertises the images-to-Zarr root keys without the Rfits \code{convention}
marker is identified as probably foreign, since such stores carry no FITS headers.

For a local path the total size on disk is reported in bytes. For a store object
it is \code{NA}, because there is nothing local to measure.
}
\value{
Invisibly, a list with components \option{filename}, \option{is_store_object},
\option{zarr_format}, \option{zarr_version}, \option{Rfits_version},
\option{self_describing}, \option{convention}, \option{created},
\option{n_arrays}, \option{array_names}, \option{total_elements},
\option{total_chunks}, \option{store_bytes}, \option{append_history} and
\option{arrays}. The last is a named list, one entry per array, each holding
\option{shape}, \option{image_shape}, \option{data_type}, \option{chunk_shape},
\option{codec}, \option{compressor}, \option{compression_level}, \option{shuffle},
\option{fill_value}, \option{n_chunks}, \option{chunk_bytes},
\option{avg_chunk_bytes}, \option{elements}, \option{fits_header},
\option{key_count}, \option{first_key}, \option{last_key} and \option{has_wcs}.
}
\author{
Aaron Robotham
}
\seealso{
\code{\link{Rfits_image_zarr}}
}
\examples{
\dontrun{
file_image = system.file('extdata', 'image.fits', package = "Rfits")
temp_image = Rfits_read_image(file_image)

file_image_zarr = file.path(tempdir(), 'image.zarr')
Rfits_write_image_zarr(temp_image, file_image_zarr, extname = 'image')

Rfits_inspect_zarr(file_image_zarr)

#The same information, for use in code
info = Rfits_inspect_zarr(file_image_zarr, print = FALSE)
info$array_names
info$arrays$image$shape
info$arrays$image$compressor

#A store written by an older Rfits has no self-description, and says so
Rfits_inspect_zarr(file_image_zarr, print = FALSE)$self_describing
}
}
\concept{ Zarr }
