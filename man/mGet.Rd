\name{mGet}
\alias{mGet}

\title{Access API Resources by ID}
\description{Retrieves API resources with some helpful structure and processing}
\usage{
	mGet(resource = "matrix", x, with = NULL, ..., parse = TRUE, enClass = FALSE, file = NULL)
}
\arguments{
	\item{resource}{name of resource for call (length-one character)}
	\item{x}{resource IDs for call (character)}
	\item{with}{parameters for call (named list)}
	\item{...}{additional parameters}
	\item{parse}{parse result as JSON (logical)}
	\item{enClass}{convert to a structured object where possible (logical)}
	\item{file}{file name for retrieved resource (character)}
}
\details{
\code{mGet} provides structured access to API resources in an \code{R}-friendly manner.  
Most functionality of the API documented at \url{http://api.metagenomics.anl.gov} is available through this interface.

Retrieved \code{resource}s may be files or data, and may or may not be transmitted in \code{JSON}.
API resources implemented by \code{mGet} require an identifier (\code{x}) appropriate to the kind of resource requested.

In some calls, \code{x} can contain multiple values.  Various formats for \code{x} are understood.  
See \link{Identifiers} for more details.

Authentication is required to access to private data.
}
\value{Various, according to the call issued.}
\author{Daniel Braithwaite}
\seealso{
\code{\link{callRaw}}, 
\code{\link{Collections}},
\code{\link{Authentication}}
}
\examples{
\dontrun{
M <- mGet( "metagenome", "4447971.3" )
P <- mGet( "project", "mgp92")
L <- mGet( "library", 3482.4 )
S <- mGet( "sample", 441 )

t <- tempfile()
mGet ("metagenome", guts, toFile = t)

ab <- mGet ("abundance", "4443360.3;4443361.3;4443362.3;4443363.3;4443364.3;4443365.3;4443366.3;4443367.3;4443368.3")

sub <- mGet ("subset", "4447971.3", org="Abiotrophia\%20defectiva\%20ATCC\%2049176")
seq <- mGet ("sequences", "4441602.3", md5="5c6cdf00b3b2509879f412d55582af1a")
mGet ("reads", "4441602.3")
mGet ("reads", "4441602.3", toFile = "someReads")
M1 <- mGet ("abundance", c(4443360.3,4443361.3,4443362.3), namespace="SEED")
M2 <- mGet ("abundance", "4443360.3;4443361.3;4443362.3", namespace="SEED", toFile="t")
M3 <- mGet ("abundance", "4443360.3;4443361.3;4443362.3", namespace="SEED", param="format/plain")
unlink (t)
}
}
