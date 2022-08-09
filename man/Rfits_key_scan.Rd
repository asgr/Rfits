\name{Rfits_key_scan}
\alias{Rfits_key_scan}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Key Word Scanner
}
\description{
Scans files or list of directories for keywords in FITS files. This is useful for e.g. finding all CRVAL1/2 of large sets of observations (if looking for a particular region of sky, say).
}
\usage{
Rfits_key_scan(filelist, dirlist = NULL, keylist = "SIMPLE", extlist = 1,
  pattern = ".fits$", recursive = TRUE, fileinfo = "Stub")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filelist}{
Character vector; vector of full paths of FITS files to analyse.
}
  \item{dirlist}{
Character vector; vector of full paths of directories to analyse.
}
  \item{keylist}{
Character vector; vector of keywords to scan the files for.
}
  \item{extlist}{
Integer vector; the extensions to use. If length 1 then it will be used for all \option{filelist}, but otherwise it should be the same length as the final \option{filelist}
}
  \item{pattern}{
Scalar character; regular expression to filter \option{filelist} by. The default limits to files ending ".fits", which is usually what you want. Most people find it easier to work via \code{\link{glob2rx}} since then you can specify file wild cards in the usual shell way, e.g. glob2rx("*F200W*fits") becomes "^.*F200W.*fits$"
}
  \item{recursive}{
Logical; if using \option{dirlist} should all sub-directories be checked recursively?
}
  \item{fileinfo}{
Character vector; additional file info to append to the output. Options are 'Full' ('/Full/Path/To/FinalFile.fits'), 'File' ('FinalFile.fits'), 'Stub' ('FinalFile'), 'Path' ('/Full/Path/To/'). If more than one is provided then these appear in the order Full / File / Stub / Path. You can also specify 'All', which will return all four options.
}
}
\details{
This uses \code{\link{Rfits_read_key}} to scan the files with the \option{keytype} = 'auto' setting to sensibly convert the values. Usually this works well.
}
\value{
Data.frame containing one row for each filtered \option{filelist} input, and the columns of \option{fileinfo} requested followed by the specified \option{keylist}. If a keyword is missing that entry will be NA.
}
\author{
Aaron Robotham
}

\seealso{
\code{\link{Rfits_read_key}} 
}
\examples{
file_image = system.file('extdata', 'image.fits', package = "Rfits")
file_cube = system.file('extdata', 'cube.fits', package = "Rfits")
  
Rfits_key_scan(filelist = c(file_image, file_cube),
  keylist=c('NAXIS1', 'CRVAL1', 'EQUINOX', 'CUNIT1'))
}

