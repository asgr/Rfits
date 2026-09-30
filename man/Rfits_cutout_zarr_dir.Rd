\name{Rfits_cutout_zarr_dir}
\alias{Rfits_cutout_zarr_dir}
\title{
Cut Out A Box From Every Zarr Store That Overlaps A Position
}
\description{
Searches a directory of Zarr stores, whether local or held in a bucket over S3, for
the ones whose field of view overlaps a requested RA/Dec position, and cuts a box out
of each of them. The result is a multi-extension list of class \code{Rfits_list}, one
extension per store that matched, in the same shape as the output of
\code{\link{Rfits_read_all}}.

Which stores matter is decided from their headers alone. Each candidate is asked for
one small metadata document rather than opened and read, so the pixel data of the
stores that do not overlap is never transferred. That is the point of the function:
finding one position in a directory of a thousand tiles costs a thousand small reads,
not a thousand images.
}
\usage{
Rfits_cutout_zarr_dir(dir = NULL, filelist = NULL, pattern = NULL,
  recursive = TRUE,
  bucket = NULL, prefix = '',
  region = NULL, endpoint = NULL,
  access_key = NULL, secret_key = NULL, session_token = NULL,
  RA = NULL, Dec = NULL, loc = NULL,
  box = 101, box.unit = c('pix', 'arcsec'),
  extname = 'data1',
  buffer = 0, safety = 0.1,
  cache = NULL, refresh = FALSE,
  header = TRUE, extract = TRUE,
  max_dirs = 1000,
  verbose = TRUE, ...)
}
\arguments{
  \item{dir}{
Character scalar; the directory to search. Locally this is a directory holding
\code{*.zarr} stores, at any depth when \option{recursive} is set. A directory that is
itself named \code{*.zarr} is searched as that one store. Give one of \option{dir},
\option{filelist} or \option{bucket}.
}
  \item{filelist}{
Character vector; explicit store paths to search instead of \option{dir}.
}
  \item{pattern}{
Character vector; regular expressions used to narrow the stores, applied in turn as
\code{\link{grep}} would apply them. Matching is against the whole path, so a nested
store can be selected by its directory as well as by its name.
}
  \item{recursive}{
Logical scalar; whether to search below \option{dir}, or below the S3 prefix. Both
back ends find stores at any depth, so the answer does not depend on which one the
data sits in.
}
  \item{bucket}{
Character scalar; the S3 bucket to search, which makes the run remote. Like
\option{dir} and \option{filelist} this is one of the three ways to say what to
search, and \option{bucket} is never taken from the environment.
}
  \item{prefix}{
Character scalar; a path within \option{bucket} to search, either a directory of
stores or a single store.
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
a remote search, since without it \code{\link[zarr]{zarr_s3store}} falls back to
anonymous credentials.
}
  \item{secret_key}{
Character scalar; the secret key. Defaults to \code{RFITS_S3_SECRET_KEY}.
}
  \item{session_token}{
Character scalar; the session token, or \code{''} when there is none. Defaults to
\code{RFITS_S3_SESSION_TOKEN}.
}
  \item{RA}{
Numeric vector; right ascension in degrees of the centre of the box. Given with
\option{Dec}, and recycled against it when one of the two has length 1.
}
  \item{Dec}{
Numeric vector; declination in degrees of the centre of the box.
}
  \item{loc}{
Numeric matrix with two columns; the same positions as \option{RA} and \option{Dec},
one row each. Used instead of them rather than as well.
}
  \item{box}{
Numeric scalar or vector of two; the size of the cutout, the same in both axes or
\option{box}[1] by \option{box}[2]. In the units of \option{box.unit}.
}
  \item{box.unit}{
Character scalar; \code{'pix'} to give the box in pixels, or \code{'arcsec'} to give
it as an angle. In arcsec the box is converted separately for every store, so tiles
with different pixel scales all get the same angular region.
}
  \item{extname}{
Character vector; the name of the array within each store, as written by
\code{\link{Rfits_dir_to_zarr}}. Several may be offered, and the first that exists in
a given store is the one searched.
}
  \item{buffer}{
Numeric scalar; extra reach in arcsec, applied on every side of the box when deciding
whether a store overlaps. It widens the search only; a cutout is always the size that
was asked for. Use it to catch a position that falls just outside a store, for
instance when the WCS of one tile is slightly off.
}
  \item{safety}{
Numeric scalar; a relative margin on the first, cheap stage of the search. Leave it
alone unless stores that clearly overlap are being missed, in which case raising it
makes that stage less strict at the cost of more work in the second.
}
  \item{cache}{
Character scalar; a path to a \code{Rfits_zarr_index} parquet file holding the header
information of the stores, and (for a remote search) the list of stores found under the
prefix, so that a directory searched repeatedly is not read over the network each time.
The \code{.parquet} extension may be left off, and a path ending in \code{.rds} keeps
its stem, which is where an older cache left the index. Local stores are checked
against their metadata files and so cannot go stale quietly; remote ones are
trusted, which is what \option{refresh} is for.
}
  \item{refresh}{
Logical scalar; whether to ignore the cache at \option{cache} and read everything
again, including the list of stores under a remote prefix. The entries it does not
touch are kept, so a run narrowed by \option{pattern} does not discard the rest.
}
  \item{header}{
Logical scalar; whether each cutout is returned as a full object with its header
rewritten for the box, or as the bare array.
}
  \item{extract}{
Logical scalar; whether to read the pixels at all. With \option{extract} = FALSE the
matching stores are returned as \code{\link{Rfits_point_zarr}} pointers, which is a
cheap way to see what a request would fetch before fetching it.
}
  \item{max_dirs}{
Integer scalar; how many S3 directories to walk before giving up on the search. The
default is generous for a survey release and protective against a prefix pointed at
the head of an unrelated bucket.
}
  \item{verbose}{
Logical scalar; whether to report what was found and matched. Stores that cannot be
read or searched are warned about either way, since data going missing is not the same
as a store declining to overlap.
}
  \item{\dots}{
Passed to the cutout, so that \option{collapse} and the other arguments of
\code{\link{Rfits_methods}} can be set per request.
}
}
\details{
The search runs in two stages. Every store is first asked for its own
\code{zarr.json}, the single small document that holds the array metadata and, for a
store written by Rfits, the whole FITS header beside it. The reference position, pixel
scale and shape from that header give a radius beyond which the store cannot possibly
overlap, and that test is arithmetic and vectorised over every store at once. Only the
handful of stores left are then tested properly, by projecting the boundary of the
requested box into the store's own frame with \code{\link[Rwcs]{Rwcs_overlap}}, which
is what copes with rotation, skew and projection.

The cutout itself is made by subsetting a \code{\link{Rfits_point_zarr}} pointer, so
it is the same box, with the same rewritten header, that \code{point[RA, Dec, box = b,
type = 'coord']} would give. Only the requested region is read from disk, which for a
remote store is the difference between transferring a box and transferring a tile.

The pointers are built from the metadata the search has already read, rather than by
opening each matched store, and a store is opened only when one of its pointers is
actually sliced. That matters because an open costs a request for the store's layout
even when no pixels are wanted, so with \option{extract} = FALSE a search over S3
transfers nothing but metadata and the header documents. The handle a slice opens is
kept in the pointer it came from, so slicing the same store again does not go looking
for it a second time.

\subsection{Caching}{

A \option{cache} file makes a repeated search over the network cost about the same as
reading a local directory. It is a \code{\link{Rfits_zarr_index}}, and holds two kinds
of entry: the search metadata of every store, which is what removes a small read per
store, and the list of stores found under a remote prefix, which is what removes the
walk that finds them. Both are held in one parquet file, which may be built ahead of
the search and published beside the tiles it describes.

Remote entries cannot be checked for staleness without making the request they exist
to avoid, so they are trusted, and \option{refresh} = \code{TRUE} is how a bucket
whose tiles have been rewritten is picked up. Local entries are validated against the
size and modification time of each store's metadata document, which costs a
\code{stat()}, so a changed header is noticed on its own. A listing is cached only
when the walk that produced it reached the end of the directory; one that stopped at
\option{max_dirs} holds a partial answer and is not kept.

A row written by an earlier version of the package, which recorded only the two
celestial axes, is re-read rather than used, since a pointer needs the whole header: a
cube or 4D array would otherwise come back having lost the keywords for its third and
fourth axes.
}

Where the box runs past the edge of a store, the store is matched all the same and the
missing pixels come back as \code{NA}, so every extension in the result is the size
that was asked for and sits at the same sky position. A cube or 4D array is searched
like anything else, but a box is a two dimensional thing, so its planes are returned
whole rather than sliced.

Stores that are not stores, and arrays with no header or no WCS in them, do not stop
the search. Each is recorded in the scan table with the reason, and the reasons are
then reported as warnings, one per distinct reason rather than one per store, because a
typo in \option{extname} across a directory of a thousand tiles is one mistake and
should read like one. A store that cannot be read warns whatever \option{verbose} says,
since data going missing is not the same as a store declining to match.

The extension name of each result is the name of the store it came from. Where two
matched stores share a name, which nested directories make possible, the path is
prepended to tell them apart. When several positions are requested the name is
suffixed with the position number.
}
\value{
Invisibly, an object of class \code{Rfits_list} holding one cutout per matching
store, named after the store it came from, with \option{filename} the path or URI of
each. Two further components record what the search did rather than what it found:
\option{scan}, one row per store examined, with its shape, pixel scale, reference
position, the radius it was searched with, a status and any error; and \option{matches},
one row per cutout returned, saying which request it answers and what size it is.
}
\author{
Aaron Robotham
}
\note{
Requires the suggested \code{Rwcs} package, since overlap is decided by projecting
boundaries. The first, cheap stage needs no WCS library at all, so a directory in
which only one tile is near the position is searched with arithmetic and one metadata
read per store.
}
\examples{
\dontrun{
#A release of 200x200 pixel tiles, each its own store. One position, a 5 arcsecond box
cut = Rfits_cutout_zarr_dir(
  dir = 'path/to/tiles',
  RA = 53.1628, Dec = -27.7444,
  box = 5, box.unit = 'arcsec')
length(cut)
plot(cut[[1]])

#What the search looked at, and what it took
attributes(cut)$scan
attributes(cut)$matches

#A position that falls on the corner where four tiles meet returns all four, which is
#usually the reason for asking this way rather than opening one file
corner = Rfits_cutout_zarr_dir(dir = 'path/to/tiles',
                               RA = 53.1314, Dec = -27.7722, box = 101)
names(corner)

#Several positions at once, and the store list narrowed before any of them is searched
many = Rfits_cutout_zarr_dir(dir = 'path/to/tiles', loc = targets$loc, box = 101,
                             pattern = 'field-07', cache = 'tiles_index')

#The same over S3, where the headers are the only thing transferred until a box is
#actually matched. extract = FALSE answers the question for the price of the scan
seen = Rfits_cutout_zarr_dir(
  bucket = 'my-bucket', prefix = 'survey/release-1/',
  region = 'auto', endpoint = 'https://account.r2.cloudflarestorage.com',
  access_key = Sys.getenv('R2_KEY'), secret_key = Sys.getenv('R2_SECRET'),
  session_token = '',
  RA = 53.1628, Dec = -27.7444, box = 101, extract = FALSE)
names(seen)
plot(seen[[1]][box = 101])
}
}
\seealso{
\code{\link{Rfits_dir_to_zarr}}, \code{\link{Rfits_point_zarr}},
\code{\link{Rfits_image_zarr}}, \code{\link{Rfits_methods}},
\code{\link{Rfits_read_all}}, \code{\link[Rwcs]{Rwcs_overlap}},
\code{\link{Rfits_zarr_index}}
}
\concept{ Zarr }
