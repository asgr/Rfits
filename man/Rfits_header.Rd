\name{Rfits_header}
\alias{Rfits_header}
\alias{Rfits_key}
\alias{Rfits_read_header}
\alias{Rfits_write_header}
\alias{Rfits_info}
\alias{Rfits_read_key}
\alias{Rfits_write_key}
\alias{Rfits_delete_key}
\alias{Rfits_write_comment}
\alias{Rfits_write_history}
\alias{Rfits_write_date}
\alias{Rfits_write_chksum}
\alias{Rfits_verify_chksum}
\alias{Rfits_get_chksum}
\alias{Rfits_encode_chksum}
\alias{Rfits_decode_chksum}
\alias{Rfits_nhdu}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FITS Header Readers and Writers
}
\description{
Generic header key readers and writers. Works with a wide range of data types including 64 bit integers (long long).
}
\usage{
Rfits_read_header(filename, ext = 1, remove_HIERARCH = FALSE)

Rfits_write_header(filename, keyvalues, keycomments, keynames, comment, history, ext = 1) 

Rfits_info(filename, remove_HIERARCH = FALSE)

Rfits_read_key(filename, keyname, keytype = 'numeric', ext = 1)

Rfits_write_key(filename, keyname, keyvalue, keycomment = "", ext = 1)

Rfits_delete_key(filename, keyname, ext = 1)

Rfits_write_comment(filename, comment = "", ext = 1)

Rfits_write_history(filename, history = "", ext = 1) 

Rfits_write_date(filename, ext = 1)

Rfits_write_chksum(filename)

Rfits_verify_chksum(filename, verbose = TRUE)

Rfits_get_chksum(filename)

Rfits_encode_chksum(checksum, complement = FALSE)

Rfits_decode_chksum(checksum, complement = FALSE)

Rfits_nhdu(filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
Character scalar; path to FITS file, either to be written to or read.
}
  \item{ext}{
Integer scalar; the extension to read or write (usually starts at 1 for images and 2 for tables). For writing, what really happens is the specified extension is deleted (if \option{create_ext} = FALSE), all current extensions shuffle forward one place, and a new extension is appended at the end of the file. This means unless the target extension is already at the end of the FITS file, the extension order will change. This is how cfitsio works with extensions (not a \code{Rfits} design decision). If the order is really important then you will need to construct a new FITS file from scratch and build it in the order desired.
}
  \item{keyname}{
Character scalar; keyname in FITS header to be updated, added (if not present), or deleted.  
}
  \item{keynames}{
Character vector; (not required) keynames in FITS header to be updated or added (if not present). If not supplied then the keynames will be taken from the names of \option{keyvalues}.
}
  \item{keytype}{
Character scalar; type of key to be read in, either "numeric" or "string". 
}
  \item{keyvalue}{
Scalar; value to be written (updated or added) into header (can be any type, e.g. numeric, integer, string).
}
  \item{keyvalues}{
List; values to be written (updated or added) into header (can be any type, e.g. numeric, integer, string).
}
  \item{keycomment}{
Character scalar; (not required) comments associated with updated or added \option{keyname}.  
}
  \item{keycomments}{
Character vector; (not required) comments associated with updated or added \option{keynames}.  
}
  \item{comment}{
Character scalar/vector; (not required) comments associated with FITS extension.  
}
  \item{history}{
Character scalar/vector; (not required) history information associated with with FITS extension. 
}
  \item{remove_HIERARCH}{
Logical, should the leading 'HIERARCH' be removed for extended keyword names (longer than 8 characters)?  
}
  \item{verbose}{
Logical; should DATASUM and CHECKSUM results be directly printed to screen?  
}
  \item{checksum}{
Either numeric or character scalar; values to be encoded/decoded respectively (only for advanced users).
}
  \item{complement}{
Logical; should the checksums be complemented (only for advanced users).  
}
}
\details{
R FITS readers and writers that uses CFITSIO to do the interfacing with files. Requires \code{bit64} package for integer64 (i.e. long long) to be work.

\code{Rfits_read_} reads in key and header data from a FITS file.

\code{Rfits_write_} writes key and header data to a FITS file.

\code{key} functions work with any FITS header and single keyname inputs and outputs (there is not a full header read yet). Note when writing, if key is already present it will be replaced, if not present the new key will be added at the end of the header.

\code{header} functions work with any FITS header, reading and writing them in their entirety.
}
\value{
\code{Rfits_read_key}: read single key entry into scalar numeric or string.

\code{Rfits_read_header}: read header into list containing the unformatted header string vector (\option{header}), the keyname / keyvalue stripped version (\option{hdr}), the named \option{keyvalues} list, the named \option{comments} list, and the vector of non-comment keynames (\option{keynames}).

\code{Rfits_info}: read headers and summary into list containing top level summary (\option{summary}), and list of all headers contained in target file (\option{headers}).

\code{Rfits_write_header} write out full header to target FITS extension.

\code{Rfits_write_key} write single key to target FITS extension header.

\code{Rfits_write_comment} write single comment (COMMENT) entry to target FITS extension header.

\code{Rfits_write_history} write single history (HISTORY) entry to target FITS extension header.

\code{Rfits_write_date} write date entry (DATE) to target FITS extension header.

\code{Rfits_write_chksum} write the FITS CHECKSUM (for the HDU) and DATASUM (for the data) header values. These are computed by CFITSIO and are either updated or added to the first FITS extension (1).

\code{Rfits_delete_key} deletes the specified \option{keyname}.

\code{Rfits_verify_chksum} verifies the FITS CHECKSUM and DATASUM header values. If verbose=TRUE, it will print the status for both CHECKSUM and DATASUM, where either one can be: present and correct, present and incorrect, or missing. It also returns a two element vector for DATASUM and CHECKSUM checks respectively, where 'correct' = present and correct, 'incorrect' = present and incorrect, 'missing' = missing.

\code{Rfits_get_checksum} return 64 bit integer vector containing DATASUM and CHECKSUM check sums for target file.

\code{Rfits_encode_chksum} converts a numeric checksum to an ascii one (advanced users only).

\code{Rfits_decode_chksum} converts an ascii checksum to a numeric one (advanced users only).

\code{Rfits_nhdu} returns the total number of extensions in the target file.
}
\references{
\url{https://heasarc.gsfc.nasa.gov/docs/software/fitsio/c/c_user/cfitsio.html}
}
\author{
Aaron Robotham
Rodrigo Tobar
}

\seealso{
\code{\link{Rfits_table}}, \code{\link{Rfits_image}}, \code{FITSio}, \code{astro}
}
\examples{
#Read full image
file_image=system.file('extdata', 'image.fits', package = "Rfits")
temp_image=Rfits_read_image(file_image)

#View header info
temp_image$keyvalues$NAXIS1
temp_image$keyvalues$CTYPE1

#Read just the header
file_table = system.file('extdata', 'table.fits', package = "Rfits")
temp_table_header_ext1=Rfits_read_header(file_table, ext = 1) #Meta table data
temp_table_header_ext2=Rfits_read_header(file_table, ext = 2) #Column info
temp_image_header=Rfits_read_header(file_image, ext = 1) #Meta image data
temp_image_header$keyvalues$NAXIS1
temp_image_header$keyvalues$CTYPE1

#Check some things
temp_table_header_ext1$keyvalues$NAXIS1
temp_table_header_ext2$keyvalues$NAXIS1
temp_table_header_ext2$keyvalues$NAXIS2
temp_table_header_ext2$keyvalues$TTYPE3

#Read individual key words on target file
Rfits_read_key(file_image, 'NAXIS1')
Rfits_read_key(file_image, 'CTYPE1', keytype = 'string')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\concept{ FITS }% use one of  RShowDoc("KEYWORDS")
