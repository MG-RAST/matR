\name{callRaw}
\alias{callRaw}
\title{Issue Unsupervised API Calls}
\description{
Performs minimal preprocessing of input to allow arbitrary API calls, returning minimally processed results.
}
\usage{callRaw(call, toFile = NULL)}
\arguments{
  \item{call}{character vector used for API call(s)}
  \item{toFile}{must be length one or equal in length to call}
}
\details{
Currently \code{matR} provides four layers of access to \code{MG-RAST}, represented by the functions \code{callRaw}, \code{mGet}, \code{matrixView}, and \code{collection}.

The purpose of \code{callRaw} is to facilitate free-form API calls.  Guidance regarding API syntax is available from the API page, linked below. The functionality of \code{callRaw} is limited to:  prefixing the API server name, appending the current authentication key (if set), iterating through multiple API calls (when \code{call} has length greater than one), and writing results to file (if so specified).

[Note: neither passing multiple filenames, nor receiving filestreams is sufficiently worked out!\cr
...\code{toFile} must be \code{NULL}, length one, or equal in length to \code{call}.  In the first case, the result(s) of the API call(s) are returned (as a list, if \code{call} has length greater than one).  In the second case, the results of all calls are appended successively to the indicated file.  In the last cases, calls are written one per file.]

Remember to set an authentication key when attempting access to private data.
}
\value{
A character vector: if files are written, their names, and otherwise, the result(s) of the API call(s).
}
\author{Daniel T. Braithwaite}
\seealso{
\url{http://api.metagenomics.anl.gov},
\code{\link{Authentication}}, \code{\link{Identifiers}},
\code{\link{callRaw}}, \code{\link{mGet}}, \code{\link{matrixView}}, \code{\link{Collections}}
}
\examples{
callRaw ("project/92")

t <- tempfile()
callRaw ("library/mgl53826", toFile = t)
unlink (t)

callRaw ("sequences/4441602.3?md5s=5c6cdf00b3b2509879f412d55582af1a")

\dontrun{
callRaw ("reads/mgm4447971.3")
callRaw ("reads/mgm4447971.3", toFile = "foo")

p <- c (10, 11, 12)
callRaw (paste ("project/", p, sep = ""), toFile = paste ("project", p, sep = ""))
}
}