\name{DirectAPIAccess}
\alias{Direct API Access}
\alias{.mCallRaw}
\title{Call the Kbase MG-RAST API Directly}
\description{
	Facility to call the Kbase MG-RAST API with exactly specified parameters.
}
\usage{
	.mCallRaw(call, toFile = NULL)
}
\arguments{
  \item{call}{...}
  \item{toFile}{...}
}
\details{
	A call to the MG-RAST API is issued with the parameter string exactly as specified, prepended with
	the name of the API server currently in use, and appended with the current auth code.
}
\value{
	Whatever is returned by the API, minimally processed.
}
\references{\url{http://api.metagenomics.anl.gov}}
\author{Daniel Braithwaite}
\seealso{
	\code{\link{mGet}},
%	\code{\link{.mServers}}, \code{link{.mGetServer}}, \code{\link{.mSetServer}}, 
	\code{\link{Kbase Authentication}}
}
\examples{
## .mCallRaw ("project/92")
}