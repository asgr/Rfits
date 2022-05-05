\name{Rfits_gunzip}
\alias{Rfits_gunzip}
\alias{Rfits_gunzip_clear}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gunzip FITS File on the Fly
}
\description{
Utility function to smartly gunzip files, since fits.gz is quite common.
}
\usage{
Rfits_gunzip(filename)
Rfits_gunzip_clear(filenames='all')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
Character scalar; path to FITS.gz file to be read.
}
  \item{filenames}{
Character vector; path to FITS.gz files to be cleared from the current referencing. The default will remove all links. You will need to run this if you restart and R session within RStudio, since the older options()$Rfits_gunzip will no longer work.
}
}
\details{
This function with gunzip a target ?.fits.gz and save the resultant ?.fits to a temporary directory. The global options()$Rfits_gunzip will then contain an Rfits_gunzip object that links the original name with the unzipped one. This means for the rest of the session you will work on the gunzipped version rather than decompressing the target file every time you interact with it. The tmp folder is deleted when you end your R session.

All \code{Rfits} reading functions will run \code{Rfits_gunzip} on the filename provided, so for the most part all the unzipping will happen magically in the background, and you can keep using the original \option{filename}.

\code{Rfits_gunzip_clear} is automatically run on package startup, so older references are always cleared out.
}
\value{
\code{Rfits_gunzip} returns a Character scalar; the path to the new gunzipped temporary version of the ?.fits file.

\code{Rfits_gunzip_clear} returns nothing, but is run for its side effect of clearing the options()$Rfits_gunzip information in case of a session restart.
}
\author{
Aaron Robotham
}
\seealso{
\code{\link{Rfits_read_all}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
}

\concept{FITS}
\concept{compress}

