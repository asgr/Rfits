\name{Rfits_header_convert}
\alias{Rfits_header_to_hdr}
\alias{Rfits_hdr_to_keyvalues}
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
  \item{remove_HIERARCH}{
Logical, should the leading 'HIERARCH' be removed for extended keyword names (longer than 8 characters)?  
}
}
\details{
Utility converstion functions, should you be stuck with an older format header.
}
\note{
\option{header} also supports the extended name format, where the first 8 letters have to be "HIERARCH", which is common from ESO facilities. When using this format, keynames can be longer than the FITS standard 8 letters.
}
\value{
\code{Rfits_header_to_hdr} return a \option{hdr} format header. See Examples.

\code{Rfits_hdr_to_keyvalues} return a \option{keyvalues} format header that is the standard in \code{Rfits}. See Examples.
}
\author{
Aaron Robotham
}

\seealso{
\code{\link{Rfits_read_header}}, \code{\link{Rfits_write_header}}
}
\examples{
#Read full image
file_image=system.file('extdata', 'image.fits', package = "Rfits")
temp_image=Rfits_read_image(file_image)

print(temp_image$header)

temp_hdr=Rfits_header_to_hdr(temp_image$header)
print(temp_hdr)

temp_keyvalues=Rfits_hdr_to_keyvalues(temp_hdr)
print(temp_keyvalues)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\concept{header }% use one of  RShowDoc("KEYWORDS")
