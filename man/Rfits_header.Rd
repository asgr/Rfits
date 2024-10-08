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
\alias{Rfits_nkey}
\alias{Rfits_read_header_raw}
\alias{Rfits_extnames}
\alias{Rfits_extname_to_ext}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FITS Header Readers and Writers
}
\description{
Generic header key readers and writers. Works with a wide range of data types including 64 bit integers (long long).
}
\usage{
Rfits_read_header(filename = 'temp.fits', ext = 1, remove_HIERARCH = FALSE,
  keypass = FALSE, zap = NULL, zaptype = 'full')

Rfits_write_header(filename = 'temp.fits', keyvalues, keycomments, keynames,
  comment, history, ext = 1, create_ext = FALSE, create_file = FALSE,
  overwrite_file = FALSE) 

Rfits_info(filename = 'temp.fits', remove_HIERARCH = FALSE)

Rfits_read_key(filename = 'temp.fits', keyname, keytype = 'auto', ext = 1)

Rfits_write_key(filename = 'temp.fits', keyname, keyvalue, keycomment = "",
    ext = 1)

Rfits_delete_key(filename = 'temp.fits', keyname, ext = 1)

Rfits_write_comment(filename = 'temp.fits', comment = "", ext = 1)

Rfits_write_history(filename = 'temp.fits', history = "", ext = 1) 

Rfits_write_date(filename = 'temp.fits', ext = 1)

Rfits_write_chksum(filename = 'temp.fits')

Rfits_verify_chksum(filename = 'temp.fits', verbose = TRUE)

Rfits_get_chksum(filename = 'temp.fits')

Rfits_encode_chksum(checksum, complement = FALSE)

Rfits_decode_chksum(checksum, complement = FALSE)

Rfits_nhdu(filename)

Rfits_nkey(filename = 'temp.fits', ext = 1)

Rfits_read_header_raw(filename = 'temp.fits', ext = 1)

Rfits_extnames(filename)

Rfits_extname_to_ext(filename, extname='')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
Character scalar; path to FITS file, either to be written to or read.
}
  \item{ext}{
Integer scalar; the extension to read or write (usually starts at 1 for images and 2 for tables). For writing, what really happens is the specified extension is deleted (if \option{create_ext} = FALSE), all current extensions shuffle forward one place, and a new extension is appended at the end of the file. This means unless the target extension is already at the end of the FITS file, the extension order will change. This is how cfitsio works with extensions (not a \code{Rfits} design decision). If the order is really important then you will need to construct a new FITS file from scratch and build it in the order desired.
}
  \item{remove_HIERARCH}{
Logical, should the leading 'HIERARCH' be removed for extended keyword names (longer than 8 characters)?  
}
  \item{keypass}{
Logical; if you have the Rwcs package installed, then you can execute an extra keypass step where the \option{keyvalues} provided have various safety checks made, and standard mistakes are corrected to make a fully FITS legal WCS.  
}
  \item{zap}{
Character vector; optional unique strings to zap out of the header. These elements are passed through \code{\link{grep}} one at a time and all matches are removed. This is useful if there a problematic keywords you want to remove when passing raw headers into \code{Rwcs} functions. Be careful that the strings specified are quite unique and do not remove more of the header than intended, e.g. 'CRVAL' would remove all mentions of CRVAL1/2/3 etc. Useful tricks- for inclusive ranges you can use '[]', e.g. 'CD[1-2]_[1-2]' will match to CD1_1, CD1_2, CD2_1, CD2_2. See \code{\link{grep}} for more information on how you pattern match within strings. For HST data we have often need to use \option{zap} = c('LOOKUP', 'DP[1-2]') because of the presence of non-compliant WCS keywords. By default the full 80 character header per key is scanned for matches, but this can be changed with \option{zaptype}.
}
  \item{zaptype}{
Character scalar; the type of zap to carry out. Either the grep is done on the full 80 character string ('full'; the default) or just the keynames ('name'). The latter might be useful when you are certain a simpler grep can be made if just using the keynames. The former is useful if some other flag in the key entry (a comment etc) can help flag keys to be zapped.  
}
  \item{keyname}{
Character scalar; keyname in FITS header to be updated, added (if not present), or deleted.  
}
  \item{keynames}{
Character vector; (not required) keynames in FITS header to be updated or added (if not present). If not supplied then the keynames will be taken from the names of \option{keyvalues}.
}
  \item{keytype}{
Character scalar; type of key to be read in, either "numeric", "integer", or "string", "character" or "char". There is also the special option "auto" that tried its best to guess the correct type of the output based on what is returned.
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
  \item{create_ext}{
Logical; should a new extension be created (TRUE) or not (FALSE). This will always be appended at the end of the FITS file (as per the FITS standard). If \option{create_ext} = TRUE then no matter what the \option{ext} argument is, the extension will be created at the end of the FITS file. If \option{create_ext} = FALSE then the extension specified by \option{ext} will be first deleted, and then the extension will be created at the end of the FITS file.
}
  \item{create_file}{
Logical; should a new file be created (TRUE) or not (FALSE). If not then the file at \option{filename} must exist for extensions to be written to it. This is really here to ensure safe operation, i.e. if \option{create_file}=TRUE and \option{overwrite_file}=FALSE and there is a file already at the path given by \option{filename} then you will get an error.
}
  \item{overwrite_file}{
Logical; if file exists at location \option{filename}, should it be overwritten (i.e. deleted and a fresh FITS file created)?
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
  \item{extname}{
Character scalar; the extension name of interest.  
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
\code{Rfits_read_header}: read header into list containing:

  \item{keyvalues}{
List; named key value list.
}
  \item{keycomments}{
List; named key comment list.
}
 \item{keynames}{
Character vector; key name vector.
}
  \item{header}{
Character vector; unformatted header with one header entry per vector element.
}
  \item{hdr}{
Character vector; keyname / keyvalue stripped version of header.
}
  \item{raw}{
Character scalar; raw unformatted header (one long string, mostly for passing to CFITSio routines).
}
  \item{comment}{
Character vector; header comment entries starting with COMMENT.
}
  \item{history}{
Character vector; header comment entries starting with HISTORY.
}
  \item{nkey}{
Integer scalar; total number of keys in the header.  
}

\code{Rfits_read_key}: read single key entry into scalar numeric or string.

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

\code{Rfits_nkey} returns the total number of keys in the target file extension.

\code{Rfits_read_header_raw} fully concatenated header output with full padding. This is really for later passing into \code{Rwcs} functions that can accept such headers, \code{Rfits} does not use this otherwise.

\code{Rfits_extnames} character vector of all the extension names present in the target fits file. Literally the string contained in EXTNAME. This will have the value of NA if EXTNAME is entirely missing from a particular extention.

\code{Rfits_extname_to_ext} integer vector of the extension location/s for a particular filename / extname combination. This is useful when extensions might vary in location. Note it will provide all extension locations in multiple matches are found.
}

\references{
\url{https://heasarc.gsfc.nasa.gov/docs/software/fitsio/c/c_user/cfitsio.html}
}
\author{
Aaron Robotham,
Rodrigo Tobar
}

\seealso{
\code{\link{Rfits_table}}, \code{\link{Rfits_image}}, \code{FITSio}, \code{astro}
}
\examples{
#Read full image
file_image = system.file('extdata', 'image.fits', package = "Rfits")
temp_image = Rfits_read_image(file_image)

#View header info
temp_image$keyvalues$NAXIS1
temp_image$keyvalues$CTYPE1

#Read just the header
file_table = system.file('extdata', 'table.fits', package = "Rfits")
temp_table_header_ext1 = Rfits_read_header(file_table, ext = 1) #Meta table data
temp_table_header_ext2 = Rfits_read_header(file_table, ext = 2) #Column info
temp_image_header = Rfits_read_header(file_image, ext = 1) #Meta image data
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
