\name{Rfits_all}
\alias{Rfits}
\alias{Rfits_all}
\alias{Rfits_read_all}
\alias{Rfits_write_all}
\alias{Rfits_read}
\alias{Rfits_write}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read and Write all FITS Extensions
}
\description{
Simple functions that either read in or write out all supported FITS extensions. This is useful if the data volume is small and you just want to have a quick look around the data. In general for processing scripts you should only read in the parts of the FITS you actually need.
}
\usage{
Rfits_read_all(filename = 'temp.fits', pointer = FALSE, header = TRUE,
  data.table = TRUE)
Rfits_write_all(data, filename = 'temp.fits', flatten = FALSE, overwrite_Main = TRUE)
Rfits_read(filename = 'temp.fits', pointer = FALSE, header = TRUE,
  data.table = TRUE)
Rfits_write(data, filename = 'temp.fits', flatten = FALSE, overwrite_Main = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
Character scalar; path to FITS file, either to be written to or read.
}
  \item{pointer}{
Logical scalar; specifies whether the images in each extension should be pointers rather than loaded into memory. See \code{\link{Rfits_point}} for more details. For small datasets that easily fit within memory, this should probably be left as FALSE, but for very large multi-extension FITS image files setting to TRUE will hugely reduce memory consumption.
}
  \item{data}{
List; a list of images and tables to be written out to a target FITS file. The format of this should look like the output of \code{Rfits_read_all}. If building more manually, this should be a list where each element has much the same format as the \option{data} and \option{table} arguments for \code{\link{Rfits_write_image}} and \code{\link{Rfits_write_table}} respectively (which is pretty much the format created by \code{\link{Rfits_read_image}} and \code{\link{Rfits_read_table}}).
}
  \item{header}{
Logical; should headers by extracted for each FITS extensions (TRUE, default), or just the data (FALSE). 
}
  \item{data.table}{
Logical; should a \code{data.table} be returned?
}
  \item{flatten}{
Logical; should the input \option{data} list structure be fully flattened before writing out. Tthis means all elements will become their own extensions at all depths of recursion (as long as they are legal data types). Default is FALSE, which will keep header structures associated with their data.
}
  \item{overwrite_Main}{
Logical; FITS tables are by default written with EXTNAME = 'Main', if the \option{overwrite_Main} flag is set to TRUE then the EXTNAME will be over-written with the name of the list object. If FALSE then the default EXTNAME of 'Main' is left, with the assoicated comments.
}
}
\details{
The interface here is very simple. Partly this is to discourage people using this as a complete replacement of the finer control available in lower level functions available in \code{Rfits}, i.e. do not expect to be able to complete all operations through the use of \code{Rfits_read_all} and \code{Rfits_read_all} alone.


}
\value{
\code{Rfits_read_all} a list containing the full outputs of \code{\link{Rfits_read_image}}, \code{\link{Rfits_read_table}} as relevant. Uses \option{header}=TRUE for images and \option{header}=FALSE for tables. The output is of class 'Rfits_all', where each list element will have its own respective class ('Rfits_image' or 'Rfit_table').

\code{Rfits_read} is simply a convenience pointer to \code{Rfits_read_all}.

\code{Rfits_write_all} write the contents of \option{data} to target FITS file \option{filename}.

\code{Rfits_write} is simply a convenience pointer to \code{Rfits_write_all}.
}
\author{
Aaron Robotham
}
\seealso{
\code{\link{Rfits_image}}, \code{\link{Rfits_table}}, \code{\link{Rfits_header}}, \code{\link{Rfits_info}}
}
\examples{
file_image=system.file('extdata', 'image.fits', package = "Rfits")
temp_image=Rfits_read_image(file_image, header=TRUE)
file_table = system.file('extdata', 'table.fits', package = "Rfits")
temp_table = Rfits_read_table(file_table, header=TRUE)

data=list(temp_image, temp_table)
  
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
