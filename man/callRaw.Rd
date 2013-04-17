\name{callRaw}
\alias{callRaw}
\title{Issue Any API Call Without Checks}
\description{
Processes input minimally to allow any API call, returning minimally processed results
}
\usage{callRaw(call, file = NULL)}
\arguments{
  \item{call}{properly-formatted API call (character)}
  \item{file}{name of file for received data (character)}
}
\details{
Facilitates free-form API calls.  Guidance regarding API syntax is available from the API page, linked below. The functionality of \code{callRaw} is limited to:  prefixing the API server name, appending the current authentication key (if set), and writing results to a file (if specified).

Note: neither passing multiple calls/filenames, nor receiving filestreams is yet worked out.

For retrieval of private data, an authentication key must be set.
}
%\value{...}
\author{Daniel T. Braithwaite}
\seealso{
\url{http://api.metagenomics.anl.gov},
\code{\link{Authentication}}, \code{\link{Identifiers}},
\code{\link{mGet}}, \code{\link{Collections}}
}
\examples{
callRaw ("project/92")

t <- tempfile()
callRaw ("library/mgl53826", file = t)
unlink (t)
}
