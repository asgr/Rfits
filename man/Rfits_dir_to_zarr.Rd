\name{Rfits_dir_to_zarr}
\alias{Rfits_dir_to_zarr}
\title{
Convert A Directory of FITS Files To Zarr Stores
}
\description{
Reads each FITS file in turn with \code{\link{Rfits_read_image}} and writes it to its
own Zarr store with \code{\link{Rfits_write_image_zarr}}, either into a local
directory or into a bucket over S3. The store for \code{image_example.fits} is
\code{image_example.zarr}, so the name of each store follows the name of the file it
came from, and the extension inside every store is the same.
}
\usage{
Rfits_dir_to_zarr(dir = NULL, filelist = NULL, pattern = NULL, recursive = TRUE,
  target = NULL, bucket = NULL, prefix = '',
  region = NULL, endpoint = NULL,
  access_key = NULL, secret_key = NULL, session_token = NULL,
  ext = 1L, extname = 'data1', verbose = TRUE, ...)
}
\arguments{
  \item{dir}{
Character scalar; the directory to collect FITS files from. Ignored when
\option{filelist} is given.
}
  \item{filelist}{
Character vector; explicit paths to use instead of searching \option{dir}. Files
without a FITS extension are dropped the same way as in a directory search.
}
  \item{pattern}{
Character vector; regular expressions used to narrow the files, applied in turn as
\code{\link{grep}} would apply them. In a directory search this matches against the
path relative to \option{dir}.
}
  \item{recursive}{
Logical scalar; whether to search sub directories of \option{dir}.
}
  \item{target}{
Character scalar; the local directory to write the stores into. Created if it does
not exist. Give either \option{target} or \option{bucket}, not both.
}
  \item{bucket}{
Character scalar; the S3 bucket to write to. Unlike every other credential, this is
never taken from the environment: naming a bucket is what asks for a remote run, so
ambient credentials cannot silently redirect a local batch.
}
  \item{prefix}{
Character scalar; a path within \option{bucket} that the stores are created under, so
that a batch can be kept in one place. A trailing slash is added if missing.
}
  \item{region}{
Character scalar; the S3 region, \code{'auto'} for Cloudflare R2. Defaults to
\code{RFITS_S3_REGION} when that is set.
}
  \item{endpoint}{
Character scalar; the S3 endpoint, needed by any service that is not AWS. Defaults
to \code{RFITS_S3_ENDPOINT} when that is set.
}
  \item{access_key}{
Character scalar; the access key. Defaults to \code{RFITS_S3_ACCESS_KEY}. Required
for a remote batch, because without it \code{\link[zarr]{zarr_s3store}} silently
falls back to anonymous credentials, which can read but never write.
}
  \item{secret_key}{
Character scalar; the secret key. Defaults to \code{RFITS_S3_SECRET_KEY}.
}
  \item{session_token}{
Character scalar; the session token, or \code{''} when there is none. Defaults to
\code{RFITS_S3_SESSION_TOKEN}. Passing \code{NULL} is mishandled by paws at the time
of writing, which is why the empty string is the way to say none.
}
  \item{ext}{
Integer scalar; the FITS extension to read from each file.
}
  \item{extname}{
Character scalar; the name given to the array inside every store. Since each file
gets a store of its own, the same name can be used for all of them.
}
  \item{verbose}{
Logical scalar; whether to report each file as it is converted, and to warn about
files that fail.
}
  \item{\dots}{
Passed to \code{\link{Rfits_write_image_zarr}}, so that \option{data_type},
\option{chunk_shape}, \option{compressor}, \option{clevel} and \option{shuffle} can
be set for the whole batch.
}
}
\details{
Each file is read, written and released in turn, so the memory used is that of one
image rather than the whole directory. One file failing does not stop the rest; the
failure is recorded in the returned table, and reported as a warning when
\option{verbose} is set.

Because a store is named after the file it came from, two files with the same base
name in different sub directories would claim the same store. That is refused before
anything is written, rather than discovered half way through when the second file
overwrites the first. Either set \option{recursive} = FALSE or run one directory at a
time.

A prefix with a store already in it is opened rather than created, so a batch can be
re-run to update the stores it made. Only a prefix with nothing in it is created,
which is what \code{\link{Rfits_s3_store_new}} alone cannot do for you, since that
function refuses a prefix that already holds a store.

Writes go through the whole array at once, so the cost of a remote batch is the size
of every image, transferred out of the directory and into the bucket. Chunking is
fixed when an array is created and cannot be changed by a later write, so
\option{chunk_shape} is a decision made once per batch.
}
\value{
Invisibly, a data frame with one row per file and components \option{fits}, the
source path, \option{store}, where it was written, \option{extname}, the name of the
array inside the store, \option{dim}, \option{data_type}, \option{nkey}, the number
of FITS keywords stored, \option{status}, either \code{'ok'} or \code{'error'}, and
\option{error}, the message for a file that failed.
}
\author{
Aaron Robotham
}
\note{
This is a batch convenience over \code{\link{Rfits_image_zarr}}, which does the same
thing for one file at a time and should be used directly when finer control is
needed.
}
\seealso{
\code{\link{Rfits_image_zarr}}, \code{\link{Rfits_s3_store_new}},
\code{\link{Rfits_inspect_zarr}}, \code{\link{Rfits_cutout_zarr_dir}}
}
\examples{
\dontrun{
#A local directory of FITS files, one store each
out = Rfits_dir_to_zarr(dir = 'path/to/images', target = 'path/to/zarr')
out[, c('fits', 'store', 'dim')]

#The same batch into a bucket. The credentials are read from the environment here,
#which is the reason that option exists: a key belongs in a script only as a name.
out = Rfits_dir_to_zarr(
  dir = 'path/to/images',
  bucket = 'my-bucket',
  prefix = 'survey/release-1/',
  region = 'auto',
  endpoint = 'https://account.r2.cloudflarestorage.com',
  access_key = Sys.getenv('R2_KEY'),
  secret_key = Sys.getenv('R2_SECRET'),
  session_token = '',
  chunk_shape = c(256, 256))

#Any one store can then be read as usual, or cut out remotely without transferring
#the whole image
library(zarr)
store = zarr_s3store$new(bucket = 'my-bucket',
                         prefix = paste0('survey/release-1/', basename(out$store[1])),
                         region = 'auto',
                         endpoint = 'https://account.r2.cloudflarestorage.com',
                         access_key = Sys.getenv('R2_KEY'),
                         secret_key = Sys.getenv('R2_SECRET'),
                         session_token = '', read_only = TRUE)
point = Rfits_point_zarr(store, extname = 'data1')
plot(point[box = 500])
}
}
\concept{ Zarr }
