\name{.mCallRaw}
\alias{callRaw}
\alias{.mCallRaw}
\alias{mCallRaw}
\title{Issue Unsupervised API Calls}
\description{Performs minimal pre-processing of input to issue arbitrary API calls, and returns minimally processed results.}
\usage{.mCallRaw(call, toFile = NULL)}
\arguments{
  \item{call}{character vector used as API call(s)}
  \item{toFile}{must be length one, or equal in length to call}
}
\details{
Currently \code{matR} provides four layers of access to \code{MG-RAST}, represented by the functions \code{callRaw}, \code{mGet}, \code{matrixView}, and \code{collection}.

The purpose of \code{callRaw} is to facilitate free-form API calls.  Guidance regarding API syntax is available from \url{http://api.metagenomics.anl.gov}. The functionality of \code{callRaw} is limited to:  prefixing the API server name, affixing the current authentication key (if set), iterating through multiple API calls (when \code{call} has length greater than one), and writing results to file (if so specified).

\code{toFile} must be \code{NULL}, length one, or equal in length to \code{call}.  In the first case, the result(s) of the API call(s) are returned (as a list, if \code{call} has length greater than one).  In the second case, the results of all calls are appended successively to the indicated file.  In the last cases, calls are written one per file.

reconciliation between filestream and toFile?  THIS IS NOT SUFFICIENT CLEAR NOR WORKED OUT.
}
\value{
The names of files written, as a character vector, if files are
....
Returns the result(s) of the API call(s) if \code{toFile} is \code{NULL}, as a list if call has length greater than one.
Returns \code{toFile} otherwise.
}
\author{
Daniel T. Braithwaite
}
\note{
Remember to set an authentication key when attempting access to private data.
}
\seealso{callRaw, mGet, matrixView, Collections
Authentication,
Resources and Identifiers}
\examples{
callRaw ("project/92")

callRaw ("sequences/4441602.3?md5s=5c6cdf00b3b2509879f412d55582af1a")

\dontrun{
callRaw ("reads/mgm4447971.3")
callRaw ("reads/mgm4447971.3", toFile = "foo")
}

t <- tempfile()
callRaw ("library/mgl53826", toFile = t)
unlink (t)

p <- c (10, 11, 12)
callRaw (paste ("project/", p, sep = ""), toFile = paste ("project", p, sep = ""))
}