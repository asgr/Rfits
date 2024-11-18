\name{Rfits_header_convert}
\alias{Rfits_header_convert}
\alias{Rfits_header_to_hdr}
\alias{Rfits_hdr_to_keyvalues}
\alias{Rfits_header_to_keyvalues}
\alias{Rfits_keyvalues_to_hdr}
\alias{Rfits_keyvalues_to_header}
\alias{Rfits_header_to_raw}
\alias{Rfits_raw_to_header}
\alias{Rfits_header_zap}
\alias{Rfits_raw_to_keyvalues}
\alias{Rfits_keyvalues_to_raw}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Header Converters
}
\description{
Utility functios to convert between header formats.
}
\usage{
Rfits_header_to_hdr(header, remove_HIERARCH = FALSE)
Rfits_hdr_to_keyvalues(hdr)
Rfits_header_to_keyvalues(header, remove_HIERARCH = FALSE)
Rfits_keyvalues_to_hdr(keyvalues)
Rfits_keyvalues_to_header(keyvalues, keycomments = NULL, comment = NULL,
  history = NULL)
Rfits_header_to_raw(header, zap = NULL, zaptype = 'full')
Rfits_raw_to_header(raw)
Rfits_raw_to_keyvalues(raw, remove_HIERARCH = FALSE)
Rfits_keyvalues_to_raw(keyvalues, keycomments = NULL, comment = NULL, history = NULL,
  zap = NULL, zaptype = 'full')
Rfits_header_zap(header, zap = NULL, zaptype= 'full', remove_HIERARCH = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{header}{
Character vector; a full character style vector header, where each element contains all of the content from a FITS header line. This generally need further processing to be useful. See Examples. Format will usually look a bit like this (where each of these lines is a separate character string in a vector):\cr
\cr
SIMPLE  =                    T / file does conform to FITS standard \cr
BITPIX  =                  -32 / number of bits per data pixel \cr
NAXIS   =                    2 / number of data axes \cr
NAXIS1  =                  356 / length of data axis 1 \cr
NAXIS2  =                  356 / length of data axis 2 \cr
EXTEND  =                    T / FITS dataset may contain extensions \cr
EQUINOX =                 2000 / equinox of celestial coord. system \cr
EPOCH   =                 2000 / epoch of celestial coord. system \cr
WCSAXES =                    2 / Number of World Coordinate System axes
}
  \item{hdr}{
Character vector; alternates keyname/kevalue pairs. This is the same format used in the \code{FITSio} package. See Examples.
}
  \item{raw}{
Character scalar; raw header where all keys are exactly separated by 80 characters, and all are concatenated together into a single string. This is mostly for use with \code{Cwcs_head_s2p} and \code{Cwcs_head_p2s} which takes a raw form header as input and passes it through WCSLIB.
}
  \item{remove_HIERARCH}{
Logical, should the leading 'HIERARCH' be removed for extended keyword names (longer than 8 characters)?  
}
  \item{keyvalues}{
List; standard \code{Rfits} list format key values header. As a convenience, for \code{Rfits_keyvalues_to_header} alone if you pass in an oject of class Rfits_header, Rfits_vector, Rfits_image, Rfits_cube, Rfits_array it will strip out the \option{keyvalues}, \option{keycomments}, \option{comment} and \option{history} and use as appropriate.
}
  \item{keycomments}{
List; standard \code{Rfits} list format key comments header.
}
  \item{comment}{
Character vector; additional header comments to write to header.
}
  \item{history}{
Character vector; additional header history to write to header.
}
  \item{zap}{
Character vector; optional unique strings to zap out of the header. These elements are passed through code{\link{grep}} one at a time and all matches are removed. This is useful if there a problematic keywords you want to remove when passing raw headers into \code{Rwcs} functions. Be careful that the strings specified are quite unique and do not remove more of the header than intended, e.g. 'CRVAL' would remove all mentions of CRVAL1/2/3 etc. Useful tricks- for inclusive ranges you can use '[]', e.g. 'CD[1-2]_[1-2]' will match to CD1_1, CD1_2, CD2_1, CD2_2. See \code{\link{grep}} for more information on how you pattern match within strings. By default the full 80 character header per key is scanned for matches, but this can be changed with \option{zaptype}.
}
  \item{zaptype}{
Character scalar; the type of zap to carry out. Either the grep is done on the full 80 character string ('full'; the default) or just the keynames ('name'). The latter might be useful when you are certain a simpler grep can be made if just using the keynames. The former is useful if some other flag in the key entry (a comment etc) can help flag keys to be zapped.  
}
  \item{\dots}{
Other arguments to be passed on to \code{\link{grep}}.
}
}
\details{
Utility converstion functions, should you be stuck with an older format header.
}
\note{
\option{header} also supports the extended name format, where the first 8 letters have to be "HIERARCH", which is common from ESO facilities. When using this format, keynames can be longer than the FITS standard 8 letters.
}
\value{
\code{Rfits_header_to_hdr} return a \option{hdr} character format header. See Examples.

\code{Rfits_hdr_to_keyvalues} return a \option{keyvalues} list format key values header that is the standard in \code{Rfits}. See Examples.

\code{Rfits_header_to_keyvalues} return a \option{keyvalues} list format key values header that is the standard in \code{Rfits}. Note this will remove non key entries such as random text, HISTORY and COMMENTS. See Examples.

\code{Rfits_keyvalues_to_hdr} return a \option{hdr} character format header. See Examples.

\code{Rfits_keyvalues_to_header} return a \option{header} character format header. See Examples.

\code{Rfits_header_to_raw} return a raw \option{header} where all keys are exactly separated by 80 characters, and all are concatenated together into a single string. This is mostly for use with \code{Cwcs_head_s2p} and \code{Cwcs_head_p2s} which takes a raw form header as input and passes it through WCSLIB.

\code{Rfits_raw_to_header} return a \option{header} vector, as used in most instances of \option{header} in \code{Rfits}.

\code{Rfits_raw_to_keyvalues} return a \option{keyvalues} list format key values header that is the standard in \code{Rfits}.

\code{Rfits_keyvalues_to_raw} return a raw \option{header} where all keys are exactly separated by 80 characters, and all are concatenated together into a single string. This is mostly for use with \code{Cwcs_head_s2p} and \code{Cwcs_head_p2s} which takes a raw form header as input and passes it through WCSLIB.

\code{Rfits_header_zap} returns a header vector with header lines containing specified text zapped out. This can be useful for cleaning bad headers up easily.
}
\author{
Aaron Robotham
}

\seealso{
\code{\link{Rfits_read_header}}, \code{\link{Rfits_write_header}}
}
\examples{
#Read full image
file_image = system.file('extdata', 'image.fits', package = "Rfits")
temp_image = Rfits_read_image(file_image)
print(temp_image$header)

temp_hdr = Rfits_header_to_hdr(temp_image$header)
print(temp_hdr)

temp_keyvalues = Rfits_hdr_to_keyvalues(temp_hdr)
print(temp_keyvalues)

temp_keyvalues = Rfits_header_to_keyvalues(temp_image$header)
print(temp_keyvalues)

temp_hdr = Rfits_keyvalues_to_hdr(temp_keyvalues)
print(temp_hdr)

temp_header = Rfits_keyvalues_to_header(temp_image$keyvalues, temp_image$keycomments,
  temp_image$comment)
print(temp_header)

temp_raw = Rfits_header_to_raw(temp_image$header)
print(temp_raw)

temp_header = Rfits_raw_to_header(temp_raw)
print(temp_header) #Notice the formatting includes blank spaces out to 80 characters

temp_keyvalues2 = Rfits_raw_to_keyvalues(temp_raw)
print(temp_keyvalues2)

identical(temp_keyvalues, temp_keyvalues2)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\concept{ header }% use one of  RShowDoc("KEYWORDS")

