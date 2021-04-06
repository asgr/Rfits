\name{Rfits_table_hdf5}
\alias{Rfits_table_hdf5}
\alias{Rfits_read_table_hdf5}
\alias{Rfits_write_table_hdf5}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FITS Style HDF5 Table Reader and Writer
}
\description{
Table readers and writers that use HDF5 for the storage back-end, rather than FITS. The read in files will look almost identical to those read in with \code{\link{Rfits_read_table}}.
}
\usage{
Rfits_read_table_hdf5(filename = 'temp.h5', extname = 'table1', ext = NULL,
  data.table = TRUE, header = FALSE, remove_HIERARCH = FALSE)
Rfits_write_table_hdf5(table, filename = "temp.h5", extname = "table1", create_ext = TRUE, 
  overwrite_file = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table}{
Data.frame or data.table; data to be written out as a binary HDF5 table.
}
  \item{filename}{
Character scalar; path to HDF5 file, either to be written to or read.
}
  \item{extname}{
Character scalar; name of the extension to be read in.
}
  \item{ext}{
Integer scalar; the extension to read. If proved this takes precedence over \option{extname}.
}
  \item{data.table}{
Logical; should a \code{data.table} be returned?
}
  \item{header}{
Logical; if TRUE then output gains attributes containing the header information (see Examples). Default is FALSE, so no table header information is explictly copied over. If \option{header}=TRUE is set then the output inherits an extra class of 'Rfits_table' (it keeps its 'data.frame' and 'data.table' classes as relevant though).
}
  \item{remove_HIERARCH}{
Logical scalar, should the leading 'HIERARCH' be removed for extended keyword names (longer than 8 characters)?  
}
  \item{create_ext}{
Logical scalar; should a new extension be created (TRUE) or not (FALSE).
}
  \item{overwrite_file}{
Logical scalar; if file exists at location \option{filename}, should it be overwritten (i.e. deleted and a fresh HSF5 file created)?
}
}
\details{
This funcation attempted to mimic a FITS like data stucture using HDF5, making use to the attributes of the group for writing the header.
}
\value{
\code{Rfits_read_table_hdf5} reads in table data from a HDF5 file.

\code{Rfits_write_table_hdf5} writes table data to a HDF5 file. This funcation will not allow you to write an extension with the same name as one that is currently present (you will need to manually delete it).
}
\author{
Aaron Robotham
}
\note{
These functions are a proof of concept for creating more generic astronomy data formats that still have meta data.
}

\seealso{
\code{\link{Rfits_table}}
}
\examples{
library(hdf5r)

#Read full table
file_table = system.file('extdata', 'table.fits', package = "Rfits")
temp_table = Rfits_read_table(file_table, header=TRUE)

#Extra header attributes (does not affect the data.frame or data.table class)
attributes(temp_table)$header[1:5]

#Write it out
file_table_temp = tempfile()
Rfits_write_table_hdf5(temp_table, filename=file_table_temp)

#Read it in again
temp_table_hdf5 = Rfits_read_table_hdf5(file_table_temp, header=TRUE)
temp_table_hdf5[1:5,]

#Check
cols_check = which(sapply(temp_table[1,], is.numeric))
sum(temp_table[,..cols_check] - temp_table_hdf5[,..cols_check])

#Extra header attributes are still present in the HDF5 file
attributes(temp_table_hdf5)$header[1:5]
}

\concept{ HDF5 }

