\name{Rfits_all_hdf5}
\alias{Rfits_hdf5}
\alias{Rfits_all_hdf5}
\alias{Rfits_read_all_hdf5}
\alias{Rfits_write_all_hdf5}
\alias{Rfits_read_hdf5}
\alias{Rfits_write_hdf5}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read and Write all HDF5 Extensions
}
\description{
Simple functions that either read in or write out all supported HDF5 extensions. This is useful if the data volume is small and you just want to have a quick look around the data. In general for processing scripts you should only read in the parts of the HDF5 you actually need. Note that files with compressed image extensions should usually be detected automatically, but there might be some un-tested corner cases.
}
\usage{
Rfits_read_all_hdf5(filename = "temp.h5", header = TRUE, data.table = TRUE) 
Rfits_read_hdf5(filename = "temp.h5", header = TRUE, data.table = TRUE) 
  
Rfits_write_all_hdf5(data, filename = "temp.h5", flatten = FALSE, list_sub = NULL) 
Rfits_write_hdf5(data, filename = "temp.h5", flatten = FALSE, list_sub = NULL) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
Character scalar; path to HDF5 file, either to be written to or read.
}
  \item{data}{
List; a list of images and tables to be written out to a target HDF5 file. The format of this should look like the output of \code{Rfits_read_all_hdf5}. If building more manually, this should be a list where each element has much the same format as the \option{data} and \option{table} arguments for \code{\link{Rfits_write_image_hdf5}} and \code{\link{Rfits_write_table_hdf5}} respectively (which is pretty much the format created by \code{\link{Rfits_read_image_hdf5}} and \code{\link{Rfits_read_table_hdf5}}).
}
  \item{header}{
Logical vector; should headers by extracted for each HDF5 extension (TRUE, default), or just the data (FALSE). If this is the same length as the number of extensions then this will be set on a per extension bases, but if length 1 then the same logic will be used for all extensions.
}
  \item{data.table}{
Logical; should a \code{data.table} be returned?
}
  \item{flatten}{
Logical; should the input \option{data} list structure be fully flattened before writing out. Tthis means all elements will become their own extensions at all depths of recursion (as long as they are legal data types). Default is FALSE, which will keep header structures associated with their data.
}
  \item{list_sub}{
Character vector; if supplied the output list elements will be limited to those named here. This is a convenient way to only write out a subset of a large list by list component name.  
}
}
\details{
The interface here is very simple. Partly this is to discourage people using this as a complete replacement of the finer control available in lower level functions available in \code{Rfits}, i.e. do not expect to be able to complete all operations through the use of \code{Rfits_read_all_hdf5} and \code{Rfits_write_all_hdf5} alone.


}
\value{
\code{Rfits_read_all_hdf5} a list containing the full outputs of \code{\link{Rfits_read_image_hdf5}}, \code{\link{Rfits_read_table_hdf5}} as relevant. The output is of class 'Rfits_list', where each list element will have its own respective class (e.g. 'Rfits_image' or 'Rfit_table'). The name of the list component will be set to that of the EXTNAME in the FITS extension.

\code{Rfits_read_hdf5} is simply a convenience pointer to \code{Rfits_read_all_hdf5}.

\code{Rfits_write_all_hdf5} write the contents of \option{data} to target HDF5 file \option{filename}.

\code{Rfits_write_hdf5} is simply a convenience pointer to \code{Rfits_write_all_hdf5}.
}
\author{
Aaron Robotham
}
\seealso{
\code{\link{Rfits_image_hdf5}}, \code{\link{Rfits_table_hdf5}}
}
\examples{
\dontrun{
library(hdf5r)

file_image = system.file('extdata', 'image.fits', package = "Rfits")
temp_image = Rfits_read_image(file_image, header=TRUE)
file_table = system.file('extdata', 'table.fits', package = "Rfits")
temp_table = Rfits_read_table(file_table, header=TRUE)

data = list(temp_image, temp_table)
  
file_mix_temp = tempfile()

Rfits_write_all_hdf5(data, file_mix_temp)

data2 = Rfits_read_all_hdf5(file_mix_temp)

sum(data[[1]]$imDat - data2[[1]]$imDat)

cols_check = which(sapply(temp_table[1,], is.numeric))
sum(data[[2]][,..cols_check] - data2[[2]][,..cols_check])
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\concept{ HDF5 }% use one of  RShowDoc("KEYWORDS")
