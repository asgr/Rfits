\name{Rfits_s3_store_new}
\alias{Rfits_s3_store_new}
\title{
Create a New Zarr Store over S3
}
\description{
Makes a brand new Zarr store at an S3 prefix (or on any S3 compatible service,
which includes Cloudflare R2) and returns the store object for use by
\code{\link{Rfits_image_zarr}}. The zarr package can open a store over S3 but
cannot create one, so this writes the root group first and then calls
\code{\link[zarr]{zarr_s3store}} as normal.
}
\usage{
Rfits_s3_store_new(bucket, prefix = "", region = NULL, endpoint = NULL,
  access_key = NULL, secret_key = NULL, session_token = NULL, force = FALSE)
}
\arguments{
  \item{bucket}{
Character scalar; name of the bucket to create the store in. The bucket itself
must already exist; this creates a store within it, not a bucket.
}
  \item{prefix}{
Character scalar; path within the bucket at which to create the store. It does
not have to end in a slash, and neither a bare name nor the same name with a
trailing slash is required, since both are normalised to the same key prefix. The
convention of naming the store after a file, e.g. \code{images/example.zarr}, is
worth keeping because that prefix is what identifies the store.
}
  \item{region}{
Character scalar or \code{NULL}; AWS region, or \code{'auto'} for R2.
}
  \item{endpoint}{
Character scalar or \code{NULL}; service endpoint, needed for anything that is
not AWS proper. Supplying one turns on path style addressing, which is what
\code{\link[zarr]{zarr_s3store}} assumes, so the two always agree.
}
  \item{access_key}{
Character scalar; access key id. Required, because without it
\code{\link[zarr]{zarr_s3store}} falls back to anonymous credentials, which
cannot write. See Details for why it cannot be omitted here either.
}
  \item{secret_key}{
Character scalar; secret access key.
}
  \item{session_token}{
Character scalar or \code{NULL}; session token, for temporary credentials. For a
long lived R2 key, pass \code{''} rather than \code{NULL}.
}
  \item{force}{
Logical scalar; if TRUE, overwrite the root group of a store that already exists
at this prefix. If FALSE (default) and a root group is found, an error is raised
rather than anything being written.
}
}
\details{
A Zarr v3 store is defined by a single \code{zarr.json} document at its root
declaring a group; that one object is what distinguishes a store from an
otherwise empty prefix. \code{\link[zarr]{zarr_s3store}} checks for it as the
last thing it does, and stops with \code{"No compatible store found at
s3://bucket/prefix"} when it is absent, which means the object is never returned
and so cannot be used to write the root group it was asking for.
\code{\link[zarr]{create_zarr}} cannot fill the gap either, because it only ever
builds a local or memory store.

This function therefore writes the root group directly with its own client, and
only then constructs the store. The consequences are that it cannot be handed an
already authenticated client, so the credentials have to be supplied on every
call, and that a failed create can leave the root group behind while leaving the
store unusable, which a later call with \option{force} = TRUE will clean up.

\strong{Overwriting}

Only the root group is written. With \option{force} = TRUE an existing root group
is replaced by a bare one, which discards the store level self-description that
\code{\link{Rfits_write_image_zarr}} maintains (the array names, shapes, chunking
and compression). The arrays themselves are untouched and remain individually
readable, and the next write with \option{update_root} = TRUE rebuilds the
description. \option{force} = TRUE is thus intended for a prefix you believe to be
empty, or one you are deliberately rebuilding, rather than as a way to add an
extension to a live store; adding an extension needs no store creation at all.

\strong{Credentials}

Read them from the environment or a profile rather than writing them into a
script, since a key in source tends to end up committed. Rfits never stores or
logs a key: the values are passed to the client and to
\code{\link[zarr]{zarr_s3store}} and are not returned in any object you are
likely to print, though the store object does hold a live client.

\strong{Reuse the store object}

Constructing a \code{zarr_s3store} means a round trip per prefix object, and every
\code{Rfits_xxx_zarr} call given a path or a raw store reopens it. Keep the object
returned here and pass it to subsequent calls, or make a pointer once with
\code{\link{Rfits_point_zarr}} and subset that repeatedly, rather than reopening
per cutout.
}
\value{
Invisibly, a \code{zarr_s3store} object for the new store, ready to be used
wherever \code{\link{Rfits_image_zarr}} accepts a \option{filename}.
}
\author{
Aaron Robotham
}
\seealso{
\code{\link{Rfits_image_zarr}}, \code{\link{Rfits_inspect_zarr}},
\code{\link{Rfits_dir_to_zarr}}, \code{\link[zarr]{zarr_s3store}}
}
\examples{
\dontrun{
#On R2. Use an AWS_PROFILE, or keys held outside the code, rather than hard
#coding one; session_token must be a string, since passing NULL is mishandled by
#paws at the time of writing.
store = Rfits_s3_store_new(
  bucket = 'my-bucket',
  prefix = 'images/example.zarr',
  region = 'auto',
  endpoint = 'https://account.r2.cloudflarestorage.com',
  access_key = Sys.getenv('R2_KEY'),
  secret_key = Sys.getenv('R2_SECRET'),
  session_token = '')

#Write an extension into it, then read only the part of it that is wanted
file_image = system.file('extdata', 'image.fits', package = "Rfits")
temp_image = Rfits_read_image(file_image)
Rfits_write_image_zarr(temp_image, store, extname = 'image')
cutout = Rfits_read_image_zarr(store, extname = 'image',
                               xlo = 1, xhi = 100, ylo = 1, yhi = 100)
dim(cutout$imDat)

#The store now exists, so it can also be reopened by zarr directly
store2 = zarr::zarr_s3store$new(bucket = 'my-bucket', prefix = 'images/example.zarr',
                                region = 'auto',
                                endpoint = 'https://account.r2.cloudflarestorage.com',
                                access_key = Sys.getenv('R2_KEY'),
                                secret_key = Sys.getenv('R2_SECRET'),
                                session_token = '')

#Creating again over the same store is refused rather than silently rebuilt
try(Rfits_s3_store_new(bucket = 'my-bucket', prefix = 'images/example.zarr',
                       region = 'auto',
                       endpoint = 'https://account.r2.cloudflarestorage.com',
                       access_key = Sys.getenv('R2_KEY'),
                       secret_key = Sys.getenv('R2_SECRET'), session_token = ''))
}
}
\concept{ Zarr }
