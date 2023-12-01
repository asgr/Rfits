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
Rfits_key_scan(filelist = NULL, dirlist = NULL, image_list = NULL, keylist = NULL,
  extlist = 1, pattern = NULL, recursive = TRUE, fileinfo = "All", keep_ext = TRUE,
  cores = 1, get_length = FALSE, get_dim = FALSE, get_centre = FALSE,
  get_rotation = FALSE, get_corners = FALSE, get_extremes = FALSE,
  get_pixscale = FALSE, get_pixarea = FALSE, get_all = FALSE, remove_HIERARCH = FALSE,
  keypass = FALSE, zap = NULL, data.table = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filelist}{
Character vector; vector of full paths of FITS files to analyse. Both \option{filelist} and \option{dirlist} can be provided, and the unique superset of both is scanned.
}
  \item{dirlist}{
Character vector; vector of full paths of directories to analyse. Both \option{filelist} and \option{dirlist} can be provided, and the unique superset of both is scanned.
}
  \item{image_list}{
List of the input \code{\link{Rfits_image}} or \code{\link{Rfits_pointer}} objects from e.g. \code{\link{Rfits_make_list}}.
}
  \item{keylist}{
Character vector; vector of keywords to scan the files for.
}
  \item{extlist}{
Integer vector; the extensions to use. If length 1 then it will be used for all \option{filelist}, but otherwise it should be the same length as the final \option{filelist}
}
  \item{pattern}{
Character vector; regular expressions to filter \option{filelist} by. Most people find it easier to work via \code{\link{glob2rx}} since then you can specify file wild cards in the usual shell way, e.g. glob2rx("*F200W*fits") becomes "^.*F200W.*fits$". Note an extra check that all files are FITS files (a pattern of ".fits$") will be made whether requested or not. Note you should nearly always use a leading and trailing '*' search because the pattern match is made on the full file path string, i.e. glob2rx('u*.fits') = '^u.*\\\\.fits$' would fail on '/path/to/file/u_GAMA.fits' (because of the '/path/to/file/' before 'u_GAMA.fits'), but glob2rx('*u*.fits') = '^.*u.*\\\\.fits$' would work as expected.
}
  \item{recursive}{
Logical; if using \option{dirlist} should all sub-directories be checked recursively?
}
  \item{fileinfo}{
Character vector; additional file info to append to the output. Options are 'Full' ('/Full/Path/To/FinalFile.fits'), 'File' ('FinalFile.fits'), 'Stub' ('FinalFile'), 'Path' ('/Full/Path/To/'). If more than one is provided then these appear in the order Full / File / Stub / Path. You can also specify 'All' (default), which will return all four options. Setting to NULL will mean no file information is prepended.
}
  \item{keep_ext}{
Logical; should extension information be kept in the output (under column called 'ext')?  
}
  \item{cores}{
Integer scalar; the number of cores to run on.
}
  \item{get_length}{
Logical, should target length be extracted? (See \code{\link{Rfits_methods}}). 
}
  \item{get_dim}{
Logical, should target dimensions be extracted? (See \code{\link{Rfits_methods}}). 
}
  \item{get_centre}{
Logical, should target central coordinates be extracted? (See \code{\link{Rfits_methods}}).
}
  \item{get_rotation}{
Logical, should frame rotation be extracted? (See \code{\link{Rfits_methods}}). 
}
  \item{get_corners}{
Logical, should target corner coordinates be extracted? (See \code{\link{Rfits_methods}}).
}
  \item{get_extremes}{
Logical, should the minimum, maximum and range of RA/Dec of the corners coordinates be extracted?. Note depending on the WCS this might not be trivial from the output corners, since both the min/max x/y pixels can be the min/max RA/Dec. The range is always in amin. (See \code{\link{Rfits_methods}}).
}
  \item{get_pixscale}{
Logical, should target pixel scale be extracted? The pixel scale is always in asec. (See \code{\link{Rfits_methods}}).
}
  \item{get_pixarea}{
Logical, should target pixel area be extracted? The pixel scale is always in asec2. (See \code{\link{Rfits_methods}}).
}
  \item{get_all}{
Logical, should all of the \option{get_XXX} methods be extracted? (See \code{\link{Rfits_methods}}).
}
  \item{remove_HIERARCH}{
Logical, should the leading 'HIERARCH' be removed for extended keyword names (longer than 8 characters)? (Only relevant for \option{get_XXX} methods).
}
  \item{keypass}{
Logical; if you have the Rwcs package installed, then you can execute an extra keypass step where the \option{keyvalues} provided have various safety checks made, and standard mistakes are corrected to make a fully FITS legal WCS. (Only relevant for \option{get_XXX} methods).
}
  \item{zap}{
Character vector; optional unique strings to zap out of the header. These elements are passed through code{\link{grep}} one at a time and all matches are removed. This is useful if there a problematic keywords you want to remove when passing raw headers into \code{Rwcs} functions. Be careful that the strings specified are quite unique and do not remove more of the header than intended, e.g. 'CRVAL' would remove all mentions of CRVAL1/2/3 etc. Useful tricks- for inclusive ranges you can use '[]', e.g. 'CD[1-2]_[1-2]' will match to CD1_1, CD1_2, CD2_1, CD2_2. See \code{\link{grep}} for more information on how you pattern match within strings. For HST data we have often need to use \option{zap} = c('LOOKUP', 'DP[1-2]') because of the presence of non-compliant WCS keywords. (Only relevant for \option{get_XXX} methods).
}
  \item{data.table}{
Logical; should a \code{data.table} be returned? Otherwise a data.frame is returned.
}
  \item{\dots}{
  
}
}
\details{
This uses \code{\link{Rfits_read_key}} to scan the files with the \option{keytype} = 'auto' setting to sensibly convert the values. Usually this works well.
}
\value{
Data.frame/data.table (depending on \option{data.table}) containing one row for each filtered \option{filelist} input, and the columns of \option{fileinfo} requested followed by the specified \option{keylist}. If a keyword is missing that entry will be NA.
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
  
temp_scan = Rfits_key_scan(filelist = c(file_image, file_cube),
  keylist=c('NAXIS1', 'CRVAL1', 'EQUINOX', 'CUNIT1'))

print(temp_scan)

temp_scan2 = Rfits_key_scan(filelist=file_image, get_all=TRUE)

print(temp_scan2)

image_list = Rfits_make_list(filelist=file_image)

temp_scan3 = Rfits_key_scan(image_list=image_list, get_all=TRUE)

print(temp_scan3)
}
