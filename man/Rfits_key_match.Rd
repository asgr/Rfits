\name{Rfits_key_match}
\alias{Rfits_key_match}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compare Keyvalues
}
\description{
Compare keyvalues to see if the header looks the same (or almost the same). This can be useful for checking if headers have the same WCS.
}
\usage{
Rfits_key_match(keyvalues_test, keyvalues_ref, check = "both", ignore = "NULL",
  verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{keyvalues_test}{
List; standard \code{Rfits} list format key values header to be tested.
}
  \item{keyvalues_ref}{
List; standard \code{Rfits} list format key values header to reference.
}
  \item{check}{
Character scalar/vector; if scalar then the test type: 'both' means only keywords that are in common will be tested; 'all' means the superset of keywords will be tested; 'test' means those present in \option{keyvalues_test} will be tested;  'ref' means those present in \option{keyvalues_ref} will be tested. If vector then the keywords specified by \option{check} will be tested.
}
  \item{ignore}{
Character scalar; keywords to ignore during testing.
}
  \item{verbose}{
Logical; should the maching and differing keywords be printed to the console?
}
}
\details{
This is different to just running \code{\link{all.equal}} on \option{keyvalues_test} and \option{keyvalues_ref} since it is tolerant to keyvalues being ordered differently. Possessing the same contents but in a different order would return TRUE in \code{Rfits_key_match} but not \code{all.equal}. The more forgiving test used here is more useful when e.g. wanting to check if the WCS is effectively the same, since the ordering of the terms is not important.
}
\value{
Logical scalar; returns whether tall keyvalues as specified by the \option{check} are TRUE.
}
\author{
Aaron Robotham
}

\seealso{
\code{\link{Rfits_header}}
}
\examples{
file_image = system.file('extdata', 'image.fits', package = "Rfits")
keyvalues_ref = Rfits_read_header(file_image)$keyvalues
keyvalues_test = keyvalues_ref
keyvalues_test$NAXIS1 = 400
keyvalues_test$NEW = 'check'

# Will fail because of differing NAXIS1:
Rfits_key_match(keyvalues_test, keyvalues_ref, check='both', verbose=TRUE)

# Will fail because of differing NAXIS1 and NEW:
Rfits_key_match(keyvalues_test, keyvalues_ref, check='test', verbose=TRUE)

# Will pass because we ignore NAXIS1:
Rfits_key_match(keyvalues_test, keyvalues_ref, check='both', ignore='NAXIS1', 
  verbose=TRUE)

# Will pass because we only check NAXIS2, CRVAL1, CRVAL2: 
Rfits_key_match(keyvalues_test, keyvalues_ref, check=c('NAXIS2', 'CRVAL1', 'CRVAL2'), 
  verbose=TRUE)
}
\concept{ compare }
\concept{ header }
