\name{Rfits_zarr_index}
\alias{Rfits_zarr_index}
\alias{Rfits_zarr_index_query}
\alias{Rfits_zarr_index_info}
\alias{print.Rfits_zarr_index_info}
\title{
A Parquet Index of the Search Metadata of a Directory of Zarr Stores
}
\description{
Builds, queries and summarises an index of the header metadata of a directory (or S3
prefix) of Zarr stores, held in a single parquet file. \code{\link{Rfits_cutout_zarr_dir}}
decides which stores are worth opening from their headers alone, which costs one small
read per store; the index holds that answer once, so a release can publish one file
beside its tiles and every user's first query costs that file instead of thousands of
requests.

\code{Rfits_zarr_index} extracts the metadata of every store and writes the index.
\code{Rfits_zarr_index_query} answers which stores could overlap a position, without
reading pixel data. \code{Rfits_zarr_index_info} reports what an index holds.
}
\usage{
Rfits_zarr_index(dir = NULL, filelist = NULL, pattern = NULL, recursive = TRUE,
  bucket = NULL, prefix = '', region = NULL, endpoint = NULL, access_key = NULL,
  secret_key = NULL, session_token = NULL, extname = 'data1', index = NULL,
  refresh = FALSE, data.table = TRUE, verbose = TRUE, ...)

Rfits_zarr_index_query(index, RA = NULL, Dec = NULL, loc = NULL, box = 101,
  box.unit = c('pix', 'arcsec'), bucket = NULL, prefix = NULL, region = NULL,
  endpoint = NULL, access_key = NULL, secret_key = NULL, session_token = NULL,
  buffer = 0, safety = 0.1, keywords = FALSE, data.table = TRUE, verbose = TRUE)

Rfits_zarr_index_info(index, bucket = NULL, prefix = NULL, region = NULL,
  endpoint = NULL, access_key = NULL, secret_key = NULL, session_token = NULL)

\method{print}{Rfits_zarr_index_info}(x, ...)
}
\arguments{
  \item{dir}{
Character scalar; the directory to index, holding \code{*.zarr} stores at any depth
when \option{recursive} is set. A directory that is itself named \code{*.zarr} is
indexed as that one store. Give one of \option{dir}, \option{filelist}, \option{bucket}
or an existing \option{index}.
}
  \item{filelist}{
Character vector; explicit store paths to index instead of \option{dir}.
}
  \item{pattern}{
Character vector; regular expressions used to narrow the stores, applied in turn as
\code{\link{grep}} would apply them. Matching is against the whole path. Narrowing an
index run does not narrow the index: rows written by a broader run are kept.
}
  \item{recursive}{
Logical scalar; whether to search below \option{dir}, or below the S3 prefix.
}
  \item{bucket}{
Character scalar; the S3 bucket holding the stores, which makes the run remote. A bare
\option{index} name is then read and written within that bucket.
}
  \item{prefix}{
Character scalar; a path within \option{bucket}, either a directory of stores or a
single store. When \option{index} is a bare name, this is where the index file is
looked for and written, which is how an index comes to sit beside the tiles it
describes and be reachable with the same credentials.
}
  \item{region}{
Character scalar; the S3 region, \code{'auto'} for Cloudflare R2. Defaults to
\code{RFITS_S3_REGION} when that is set.
}
  \item{endpoint}{
Character scalar; the S3 endpoint, needed by any service that is not AWS. Defaults to
\code{RFITS_S3_ENDPOINT} when that is set.
}
  \item{access_key}{
Character scalar; the access key. Defaults to \code{RFITS_S3_ACCESS_KEY}. Required for
a remote run.
}
  \item{secret_key}{
Character scalar; the secret key. Defaults to \code{RFITS_S3_SECRET_KEY}.
}
  \item{session_token}{
Character scalar; the session token, or \code{''} when there is none. Defaults to
\code{RFITS_S3_SESSION_TOKEN}.
}
  \item{extname}{
Character vector; the name(s) of the array within each store to record, as written by
\code{\link{Rfits_dir_to_zarr}}. The first that exists in a given store is the one
kept, and each store is held as its own row.
}
  \item{index}{
Character scalar; where the index is. A local path, a bare name (which, with
\option{bucket}, addresses an object under \option{prefix}), or a full \code{s3://}
URI. The extension may be left off, since the file is always named
\code{<stem>.parquet}; a path ending in \code{.rds} keeps its stem, which is where the
older cache format left it.
}
  \item{refresh}{
Logical scalar; whether to rebuild from the stores even when \option{index} already
exists. Without it, an existing index is returned unread and the stores are never
opened, which is the point of having built one. When writing, existing rows are merged
rather than replaced.
}
  \item{data.table}{
Logical scalar; whether to return a \code{data.table} rather than a plain
\code{data.frame}.
}
  \item{verbose}{
Logical scalar; whether to report what was read, recorded and written.
}
  \item{\dots}{
Passed to the store search, so that \option{max_dirs} can be raised for a deep prefix.
}
  \item{RA}{
Numeric vector; right ascension(s), in degrees, to search for. Give \option{RA} and
\option{Dec}, or \option{loc}.
}
  \item{Dec}{
Numeric vector; declination(s), in degrees.
}
  \item{loc}{
Numeric matrix with two columns; positions as RA/Dec, one row each, given instead of
\option{RA} and \option{Dec} rather than as well.
}
  \item{box}{
Numeric scalar or vector of length two; the size of the requested cutout, which sets
how far a store's reference position may be from the requested one and still be
returned. Same as the argument of
\code{\link{Rfits_cutout_zarr_dir}}, and deliberately generous in the same way.
}
  \item{box.unit}{
Character scalar; whether \option{box} is in pixels or arcsec. In arcsec the box is
converted per store, using that store's own pixel scale.
}
  \item{buffer}{
Numeric scalar; extra reach in arcsec, applied on every side of the box. Use it to
catch a store that lies just outside the request.
}
  \item{safety}{
Numeric scalar; a relative margin on the candidate radius. Raising it makes the test
less strict, at the cost of returning more stores.
}
  \item{keywords}{
Logical scalar; whether to fetch the serialised header of each surviving store into a
\code{keyvalues} column. Those bytes are the bulk of the file, so the default keeps a
query to the columns a search filters with.
}
  \item{x}{
An object of class \code{Rfits_zarr_index_info}, as returned by
\code{Rfits_zarr_index_info}.
}
}
\details{
The index holds three kinds of row, named by the \code{kind} column. Store rows carry
the search metadata of one array of one store. Walk rows carry the list of stores found
under a remote prefix, kept because finding them costs a request per directory. Meta
rows are scalars about the file itself, of which the format version is the only one
anyone else reads.

Every column a search filters with is scalar, so a query is answered with an arrow scan
that projects them alone and never decodes the keyword blob of the thousands of stores
nowhere near the position. The keywords of the survivors are then fetched by store key,
from a file written in sorted store order so that whole row groups can be skipped.

The candidate test is the same generous one the search uses, and is shared with it, so
a store this reports cannot be one the search would have rejected. That includes the
offset between a store's reference point and its centre, which a stricter version of
the test would have missed. A position that is nearer than a store's half diagonal plus
the box plus \option{buffer} and \option{safety} is returned; anything whose radius
could not be worked out is returned too, since a pre-filter that quietly dropped a
match would look exactly like a directory of tiles that did not overlap.

An index written by a later version of the package is refused rather than half read.
One with no version is treated the same way, and says to rebuild with
\option{refresh} = \code{TRUE}.
}
\value{
\code{Rfits_zarr_index} and \code{Rfits_zarr_index_query} return, invisibly, one row
per store: the \code{kind}, \code{cache_key}, \code{store}, \code{label},
\code{extname}, \code{status} and \code{error} columns, the scalar WCS and shape
columns the filter reads (\code{ra}, \code{dec}, \code{crpix1}, \code{crpix2},
\code{naxis1}, \code{naxis2}, \code{pixscale_x}, \code{pixscale_y}), \code{array_dim},
a \code{stamp} of the metadata document the row was read from, and
\code{has_keyvalues}. A query with \option{keywords} = \code{TRUE} adds the
\code{keyvalues} column, holding each store's serialised header as a raw vector.

\code{Rfits_zarr_index_info} returns, invisibly, a list of class
\code{Rfits_zarr_index_info} with the index \option{path}, format \option{version},
number of store \option{n_rows}, the \option{extnames} and \option{stores} it holds, a
\code{status} table of how many rows are \code{'ok'} and how many are not, the number
of cached \option{n_walks} store lists, and the time the index was last written
(\option{created}).
}
\author{
Aaron Robotham
}
\note{
Requires the suggested \code{arrow} package, which is the reader that can push both a
row filter and a column projection into the scan. Remote runs also need
\code{paws.storage}.
}
\examples{
\dontrun{
#A release of tiles, each its own store, indexed once beside them
ix = Rfits_zarr_index(dir = 'path/to/tiles', index = 'path/to/tiles/index')
nrow(ix)
table(ix$status)

#What a search would look at, for one position, without reading a pixel
hit = Rfits_zarr_index_query('path/to/tiles/index', RA = 53.1628, Dec = -27.7444,
                             box = 5, box.unit = 'arcsec')
hit$label

#Build it beside the tiles in a bucket. A bare index name resolves under the prefix
#being indexed, so the index is published next to the data it describes
Rfits_zarr_index(bucket = 'my-bucket', prefix = 'survey/release-1/',
                 index = 'index', region = 'auto',
                 endpoint = 'https://account.r2.cloudflarestorage.com',
                 access_key = Sys.getenv('R2_KEY'),
                 secret_key = Sys.getenv('R2_SECRET'), session_token = '')

#Someone else, handed the URI of the published index, needs only the credentials
seen = Rfits_zarr_index_query(
  's3://my-bucket/survey/release-1/index.parquet',
  loc = targets$loc, box = 101,
  region = 'auto', endpoint = 'https://account.r2.cloudflarestorage.com',
  access_key = Sys.getenv('R2_KEY'), secret_key = Sys.getenv('R2_SECRET'),
  session_token = '')
#The headers of the survivors, to build pointers from without opening any store
head = Rfits_zarr_index_query('s3://my-bucket/survey/release-1/index.parquet',
                              RA = 53.1628, Dec = -27.7444, keywords = TRUE,
                              region = 'auto', endpoint = '...', access_key = '...',
                              secret_key = '...', session_token = '')

#Is the index current, and what does it hold?
Rfits_zarr_index_info('path/to/tiles/index')

#Hand the same file to the search, which then costs one read rather than one per store
cut = Rfits_cutout_zarr_dir(dir = 'path/to/tiles', RA = 53.1628, Dec = -27.7444,
                            box = 101, cache = 'path/to/tiles/index')
}
}
\seealso{
\code{\link{Rfits_cutout_zarr_dir}}, \code{\link{Rfits_dir_to_zarr}},
\code{\link{Rfits_point_zarr}}, \code{\link{Rfits_image_zarr}},
\code{\link{Rfits_inspect_zarr}}
}
\concept{ Zarr }
