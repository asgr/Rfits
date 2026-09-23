\name{Rfits_cutout_app}
\alias{Rfits_cutout_app}
\title{
A Shiny App for Browsing and Cutting Out a Directory of Zarr Stores
}
\description{
Launches the Shiny application bundled with the package, which gives a graphical
front end to \code{\link{Rfits_cutout_zarr_dir}} and \code{\link{Rfits_zarr_index}}. A
directory of Zarr stores, or an S3 prefix holding one, is described once; the app then
takes RA/Dec positions, shows the cutout of every overlapping store, and offers the
result as a zip of FITS files or a zip of JPEGs.

The application is a single file, installed under \file{inst/shiny/Rfits_cutout_app}, and
can equally be run directly with \code{shiny::runApp(system.file('shiny',
'Rfits_cutout_app', package = 'Rfits'))}. This wrapper exists so that the dependency
checks happen in R, where they can name the missing package.
}
\usage{
Rfits_cutout_app(launch.browser = getOption('shiny.launch.browser', interactive()),
  display.mode = c('normal', 'static'), port = NULL, ...)
}
\arguments{
  \item{launch.browser}{
Logical, or a function taking a URL; passed straight to \code{shiny::runApp}. The default
follows the usual Shiny option, so the app opens a browser when the session is
interactive.
}
  \item{display.mode}{
Character scalar, one of \code{'normal'} or \code{'static'}, passed to
\code{shiny::runApp}.
}
  \item{port}{
Integer port to serve on, or \code{NULL} to let Shiny choose.
}
  \item{...}{
Further arguments passed to \code{shiny::runApp}, such as \code{host} or
\code{quiet}.
}
}
\details{
The app has four tabs.

\strong{Store} collects the credentials and location. Give either a bucket with an access
key and secret, or a local directory of \code{*.zarr} stores, and press Load index. The
index is read from \code{s3://<bucket>/index.parquet} by default, which can be retyped as
a path to look beside the tiles instead. When no index is found, one is built from the
tiles with \code{\link{Rfits_zarr_index}} and kept in the session's own temporary file;
nothing is ever written back to the store, because a shared release is not the place for
a private cache. Whoever publishes the tiles should decide whether an index ships with
them. This is also why the search is issued per store rather than with the \code{cache}
argument of \code{\link{Rfits_cutout_zarr_dir}}, which would write what it learns back to
the bucket. Credentials fall back to the environment variables
\env{RFITS_S3_REGION}, \env{RFITS_S3_ENDPOINT}, \env{RFITS_S3_ACCESS_KEY},
\env{RFITS_S3_SECRET_KEY} and \env{RFITS_S3_SESSION_TOKEN} when the fields are left
blank, as they do for \code{\link{Rfits_cutout_zarr_dir}}.

\strong{Cutouts} takes positions one per line, as \code{RA Dec} in degrees (separated by a
space or a comma, and either is accepted from a pasted list), and a box in pixels or
arcsec. Each position is narrowed to the stores that could overlap it by
\code{\link{Rfits_zarr_index_query}}, and only those stores are opened, so the number of
pixel reads a request makes is bounded by the number of tiles that matter rather than by
the size of the release. Stores may be narrowed further by a regular expression on the
store name, and by a header keyword and value; the keyword is read out of the index, so
filtering by band or target costs a decode rather than a pixel read. Some common filters
are index columns rather than header keywords \emph{(EXTNAME, STATUS, TYPE, LABEL,
STORE)} and are recognised as such. The request and display controls are stacked down the
left of the tab and the answers fill the wider panel on the right, where each cutout is
drawn as a square of the chosen side rather than across the full width of the page. Only
\code{Max cutouts to plot} of them are drawn, since a request that overlaps a thousand
tiles would otherwise bind a thousand plots; all of them are still listed and downloadable.
Each cutout is plotted with the WCS aware \code{\link{plot}} method, with \code{qdiff} and
the \code{magimage} clipping controls exposed. The display options are live: changing a
stretch redraws the plots from the pixels already in memory rather than re-cutting them.
A store that was opened cleanly but holds no overlap at the position asked for is named in
the run log, since that is otherwise indistinguishable from a lost result.

\strong{Index} shows the rows of the index the search reads, one per store per extension,
with the reference position, shape and pixel scale each overlap test works from, plus the
reason any store was found not searchable. The table is wide and scrolls horizontally
inside its own card.

\strong{Frames} plots the footprint of every store in the index, from the same numbers and
without reading a pixel, as an interactive plotly panel: scroll to zoom (or use the zoom
tool in the mode bar), double click to reset, and the RA axis is reversed the way a sky map
is read. Hovering a footprint or its centre gives the store name, extension, type,
reference position, shape and pixel scale. \emph{Clicking} a frame adds its exact reference
position to the picks, at any number of frames and any zoom level, and
\strong{Send picked positions} appends those centres to the position list on the Cutouts
tab and switches to it. A drag with the Select box tool instead highlights the frames it
intersects, and \strong{Send selected frames} appends the centre of every frame in that
selection; the centre of the box itself is never sent, since it generally falls between
tiles. The picked and selected frames are listed in the table beside the plot, which shows
the frames in view when nothing has been picked yet. The boxes are axis aligned and ignore
rotation, which is the same approximation the search's own coarse filter makes. Filters set
on the Index tab apply here too, and a frame picked before a filter change keeps its pick.

FITS files are written from the extracted pixels and header with
\code{\link{Rfits_write_image}}, so each one keeps the WCS of the tile it came from. The
JPEGs are re-rendered square, at the side given for the on screen panels, with the current
display options, since the on screen plots are drawn into the browser's device and there
is nothing on the server to copy.
}
\value{
Called for its side effect of serving the application. The value of
\code{shiny::runApp} is returned, which is \code{NULL} unless the app is run
asynchronously.
}
\references{
\code{\link{Rfits_cutout_zarr_dir}}, \code{\link{Rfits_zarr_index}},
\code{\link{Rfits_dir_to_zarr}}.
}
\seealso{
\code{\link{Rfits_image_zarr}} for the Zarr readers and writers, and
\code{\link{Rfits_s3_store_new}} for creating a store over S3.
}
\examples{
\dontrun{
#A local directory of stores, with no index yet
Rfits_cutout_app()

#Or an S3 prefix, with credentials in the environment
Sys.setenv(RFITS_S3_ACCESS_KEY='...', RFITS_S3_SECRET_KEY='...')
Rfits_cutout_app(port=8123)

#The same app, run without the wrapper
shiny::runApp(system.file('shiny', 'Rfits_cutout_app', package = 'Rfits'))
}
}
\keyword{utilities}
