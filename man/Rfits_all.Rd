\name{Rfits_all}
\alias{Rfits}
\alias{Rfits_all}
\alias{Rfits_read_all}
\alias{Rfits_write_all}
\alias{Rfits_read}
\alias{Rfits_write}
\alias{Rfits_make_list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read and Write all FITS Extensions
}
\description{
Simple functions that either read in or write out all supported FITS extensions. This is useful if the data volume is small and you just want to have a quick look around the data. In general for processing scripts you should only read in the parts of the FITS you actually need. Note that files with compressed image extensions should usually be detected automatically, but there might be some un-tested corner cases.
}
\usage{
Rfits_read_all(filename = 'temp.fits', pointer = 'auto', header = TRUE,
  data.table = TRUE, anycompress = TRUE, bad = NULL, zap = NULL, zaptype= 'full',
  NAcheck = FALSE, nthreads = 1)
Rfits_read(filename = 'temp.fits', pointer = 'auto', header = TRUE,
  data.table = TRUE, anycompress = TRUE, bad = NULL, zap = NULL, zaptype= 'full',
  NAcheck = FALSE, nthreads = 1)
  
Rfits_write_all(data, filename = 'temp.fits', flatten = FALSE, overwrite_Main = TRUE, 
  compress = FALSE, bad_compress = 0, list_sub = NULL)
Rfits_write(data, filename = 'temp.fits', flatten = FALSE, overwrite_Main = TRUE,
  compress = FALSE, bad_compress = 0, list_sub = NULL)
  
Rfits_make_list(filelist = NULL, dirlist = NULL, extlist = 1, pattern = NULL,
  recursive = TRUE, header = TRUE, pointer = TRUE, cores = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
Character scalar; path to FITS file, either to be written to or read. This is written as an attribute (called \option{filename}) to the output 'Rfits_list' object.
}
  \item{pointer}{
Logical scalar; specifies whether the images in each extension should be pointers rather than loaded into memory (tables are always loaded into memory). See \code{\link{Rfits_point}} for more details. For small data sets that easily fit within memory, this should probably be set as FALSE, but for very large multi-extension FITS image files (especially those which contain compressed images) setting to TRUE will hugely reduce memory consumption and speed up access times of subsets. The default of 'auto' will set to TRUE if the target FITS file is larger than 100 MB (perhaps a bit unwieldy and slow to load in) and FALSE if it is smaller than this. For \code{Rfits_make_list} the only allowed values are TRUE (default) and FALSE, no 'auto' option is available.
}
  \item{data}{
List; a list of images and tables to be written out to a target FITS file. Supported types are: Rfits_vector, Rfits_image, Rfits_cube, Rfits_array, array, matrix, integer, numeric, Rfits_table, data.frame, data.table, Rfits_header and Rfits_pointer. The format of this should look like the output of \code{Rfits_read_all}. If building more manually, this should be a list where each element has much the same format as the \option{data} and \option{table} arguments for \code{\link{Rfits_write_image}} and \code{\link{Rfits_write_table}} respectively (which is pretty much the format created by \code{\link{Rfits_read_image}} and \code{\link{Rfits_read_table}}).
}
  \item{header}{
Logical vector; should headers by extracted for each FITS extension (TRUE, default), or just the data (FALSE). If this is the same length as the number of extensions then this will be set on a per extension bases, but if length 1 then the same logic will be used for all extensions.
}
  \item{data.table}{
Logical; should a \code{data.table} be returned?
}
  \item{flatten}{
Logical; should the input \option{data} list structure be fully flattened before writing out. This means all elements will become their own extensions at all depths of recursion (as long as they are legal data types). Default is FALSE, which will keep header structures associated with their data.
}
  \item{overwrite_Main}{
Logical; FITS tables are by default written with EXTNAME = 'Main', if the \option{overwrite_Main} flag is set to TRUE then the EXTNAME will be over-written with the name of the list object. If FALSE then the default EXTNAME of 'Main' is left, with the associated comments.
}
  \item{anycompress}{
Logical; might some of the extensions be compressed images? If this is definitely not the case and you are having issues reading in the target FITS then set to FALSE, since it means all binary tables will be treated as FITS tables (not compressed images). In general leaving this set to TRUE should be okay though since \code{Rfits_read_all} / \code{Rfits_read} checks whether the keyword 'ZIMAGE' is TRUE, FALSE or not present (which means it is not a compressed image).
}
  \item{bad}{
Vector; replacement scalar (e.g. NA, NaN etc) for all non-finite values. This can be useful since CFITSIO converts R NA to NaN when writing and reading, and within R NA is usually the preferable representation. If set to a single value then this is used for all images, otherwise you can specify a vector specifying the value for each extension (although only actually used for images).
}
  \item{zap}{
Character vector; optional unique strings to zap out of the header. These elements are passed through code{\link{grep}} one at a time and all matches are removed. This is useful if there a problematic keywords you want to remove when passing raw headers into \code{Rwcs} functions. Be careful that the strings specified are quite unique and do not remove more of the header than intended, e.g. 'CRVAL' would remove all mentions of CRVAL1/2/3 etc. Useful tricks- for inclusive ranges you can use '[]', e.g. 'CD[1-2]_[1-2]' will match to CD1_1, CD1_2, CD2_1, CD2_2. See \code{\link{grep}} for more information on how you pattern match within strings. By default the full 80 character header per key is scanned for matches, but this can be changed with \option{zaptype}.
}
  \item{zaptype}{
Character scalar; the type of zap to carry out. Either the grep is done on the full 80 character string ('full'; the default) or just the keynames ('name'). The latter might be useful when you are certain a simpler grep can be made if just using the keynames. The former is useful if some other flag in the key entry (a comment etc) can help flag keys to be zapped.  
}
  \item{NAcheck}{
Logical; should the read in images be explictly checked for NAs? For very big images (10k x 10k +) this has a notable overhead, so if we know it can be avoided it perhaps should be. Otherwise things we would like to be NA could appear as NaN, which will still work for most purposes but not as clean.
}
  \item{nthreads}{
Number of threads used to read images in parallel.
}
  \item{compress}{
Logical vector; option to specify per extension image compression. If set to a single value then this is used for all images, otherwise you can specify a logical vector specifying the compression of each extension (although only an option for images). A semi-hidden convenience feature is to set this to a value, and only extensions with lengths longer than this will be compressed. E.g. you might want to use 1e4 to ensure images larger than 100x100 are compressed, but small distortion matrices are not.
}
  \item{bad_compress}{
Numeric vector; non-numeric replacement value to use when compression is being done. CFITSIO does not work well with any of the usual R non-numeric values (NA, NaN, Inf etc) when compressing since it makes bad guesses at how to correctly re-scale the image for compression. This flag offers a simple numeric value to replace all non-numeric elements with when compression is requested (so it is not applied for uncompressed image writing). If set to a single value then this is used for all images, otherwise you can specify a logical vector specifying the compression of each extension (although only an option for images).
}
  \item{list_sub}{
Character vector; if supplied the output list elements will be limited to those named here. This is a convenient way to only write out a subset of a large list by list component name.  
}
  \item{filelist}{
Character vector; vector of full paths of FITS files to analyse. Both \option{filelist} and \option{dirlist} can be provided, and the unique super-set of both is used. This is written as an attribute (called \option{filename}) to the output 'Rfits_list' object.
}
  \item{dirlist}{
Character vector; vector of full paths of directories to analyse. Both \option{filelist} and \option{dirlist} can be provided, and the unique super-set of both is used.
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
  \item{cores}{
Integer scalar or \code{NULL}. If given, the number of cores to run on. This will create and register an implicit doParallel cluster that will then have to be explicitly destroyed by the caller. Consider explicitly creating, registering and stopping a doParallel cluster around your whole session instead.
}
  \item{\dots}{
Further arguments to pass to \code{\link{Rfits_read_image}} (when \option{pointer} = FALSE) or \code{\link{Rfits_point}} (when \option{pointer} = TRUE). Note these arguments will be inherited by all files loaded into the 'Rfits_list'.
}
}
\details{
The interface here is very simple. Partly this is to discourage people using this as a complete replacement of the finer control available in lower level functions available in \code{Rfits}, i.e. do not expect to be able to complete all operations through the use of \code{Rfits_read_all} and \code{Rfits_write_all} alone. That said, they cover an awful lot of use cases in practice.
}
\value{
\code{Rfits_read_all} a list containing the full outputs of \code{\link{Rfits_read_image}}, \code{\link{Rfits_read_table}} as relevant. The output is of class 'Rfits_list', where each list element will have its own respective class (e.g. 'Rfits_image' or 'Rfits_table'). The name of the list component will be set to that of the EXTNAME in the FITS extension.

\code{Rfits_read} is simply a convenience pointer to \code{Rfits_read_all}.

\code{Rfits_write_all} write the contents of \option{data} to target FITS file \option{filename}.

\code{Rfits_write} is simply a convenience pointer to \code{Rfits_write_all}.

\code{Rfits_make_list} creates a list of images or image pointers. The output is of class 'Rfits_list', so in most respects it is like you have read in a single multi-extension FITS rather than assembling it from multiple different target files. This can be useful when wanting to rapidly interrogate images interactively. 
}
\author{
Aaron Robotham
}
\seealso{
\code{\link{Rfits_image}}, \code{\link{Rfits_table}}, \code{\link{Rfits_header}}, \code{\link{Rfits_info}}
}
\examples{
file_image = system.file('extdata', 'image.fits', package = "Rfits")
temp_image = Rfits_read_image(file_image, header=TRUE)
file_table = system.file('extdata', 'table.fits', package = "Rfits")
temp_table = Rfits_read_table(file_table, header=TRUE)

data = list(temp_image, temp_table)
  
file_mix_temp = tempfile()

Rfits_write_all(data, file_mix_temp)

data2 = Rfits_read_all(file_mix_temp)

sum(data[[1]]$imDat - data2[[1]]$imDat)

cols_check = which(sapply(temp_table[1,], is.numeric))
sum(data[[2]][,..cols_check] - data2[[2]][,..cols_check])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\concept{ FITS }% use one of  RShowDoc("KEYWORDS")
