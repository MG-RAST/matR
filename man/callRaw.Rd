\name{callRaw}
\alias{callRaw}
\title{Issue Unsupervised API Calls}
\description{
Processes input minimally to allow arbitrary API calls, returning minimally processed results
}
\usage{callRaw(call, file = NULL)}
\arguments{
  \item{call}{...}
  \item{file}{...}
}
\details{
The purpose of \code{callRaw} is to facilitate free-form API calls.  Guidance regarding API syntax is available from the API page, linked below. The functionality of \code{callRaw} is limited to:  prefixing the API server name, appending the current authentication key (if set), and writing results to a file (if specified).

Note: neither passing multiple calls/filenames, nor receiving filestreams is yet worked out.

For retrieval of private data, an authentication key must be set.
}
\value{
...
}
\author{Daniel T. Braithwaite}
\seealso{
\url{http://api.metagenomics.anl.gov/api2.cgi},
\code{\link{Authentication}}, \code{\link{Identifiers}},
\code{\link{callRaw}}, \code{\link{mGet}}, \code{\link{Collections}}
}
\examples{
callRaw ("project/92")

t <- tempfile()
callRaw ("library/mgl53826", file = t)
unlink (t)
}
