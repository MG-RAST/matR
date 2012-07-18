\name{.mCallRaw}
\alias{.mCallRaw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Issue Raw API Calls}
\description{Provides convenient API access to construct calls directly }
\usage{.mCallRaw(call, toFile = NULL)}
\arguments{
  \item{call}{character vector.  if greater than length one, a call is issued for each element}
  \item{toFile}{if not length zero, recycled to length of "call"}
}
\details{
\code{matR} provides four layers of access to \code{MG-RAST}, represented by the functions \code{callRaw}, \code{mGet}, \code{matrixView}, and \code{collection}.

purpose is to allow user to construct an ad-hoc API call 
adds server and authentication information
dumps to file if toFile is specified
(what is content is filestream?)
}
\value{
whatever is returned by the API
}
\author{
Daniel T. Braithwiate
}
\note{
Remember to set an authentication key when attempting access to private data.
}
\seealso{callRaw, mGet, matrixView, Collections}
\examples{
mCallRaw ("project/92")

t <- tempfile()
mCallRaw ("library/mgs?;messystuff=blah;.d", toFile = t)
unlink (t)

example of downloading filestream
}