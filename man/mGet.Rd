\name{mGet}
\alias{mGet}
\title{Access API Resources by ID}
\description{Presents a consistent interface for structured retrieval of API resources by ID and qualifying parameters, and for translation into objects natural to R.}
\usage{
	mGet(resource =  c ("project", "sample", "library", "metagenome", "subset", "sequenceSet",
    "sequences", "reads", "abundance"), ID, namespace = NULL, annoType = NULL, seqType = NULL, org = NULL, func = NULL, md5 = NULL, param = NULL, parse = TRUE, enClass = FALSE, toFile = NULL)
}
\arguments{
	\item{resource}{...}
	\item{ID}{one or multiple IDs appropriate to the resource requested}
	\item{namespace}{...}
	\item{annoType}{...}
	\item{seqType}{...}
	\item{org}{...}
	\item{func}{...}
	\item{md5}{...}
	\item{param}{...}
  \item{parse}{attempt to parse return object(s) as JSON?}
  \item{enClass}{attempt to give return object(s) a matR S4 class?}
	\item{toFile}{...}
}
\details{
Currently \code{matR} provides four layers of access to \code{MG-RAST}, represented by the functions \code{callRaw}, \code{mGet}, \code{matrixView}, and \code{collection}.

The purpose of \code{mGet} is to provide structured access to API resources in an \code{R}-friendly manner.  Most functionality of the API documented at \url{http://api.metagenomics.anl.gov} is reliably implemented; familiarity with that API is necessary to use \code{mGet} effectively.

\code{resource}s returned may be files, textual, numerical, or other.  Some are, and some are not, transmitted in \code{JSON}.  All API resources implemented by \code{mGet} require an \code{ID} appropriate to the kind of resource requested.

In many calls, \code{ID} is permitted to contain multiple values.  Many intuitive formats for \code{ID} are understood.  See \link{Resources and Identifiers} for more details.  \code{resource}, also, may have multiple values, and is recyled to match the length of \code{ID} except in the case of "abundance."

Not all other parameters (\code{namespace}, \code{org}, \code{func}, and \code{md5}) are relevant to all resources.  Also, multiple values are (or are not) allowed, according to the values of \code{resource} and \code{ID}.  \code{mGet} checks reasonably thoroughly that the API call being attempted actually makes sense.

The results of the API call may be directed to file(s) with the \code{toFile} parameter.  Single or multiple filenames may be specified.  
In many cases, it is ok to call with multiple IDs and a single filename, and the result is that the downloaded resources
will be concatenated into a single file.  However, if the \code{resource} actually is a filestream (for instance, \code{sequenceSet} and \code{reads}), it is
always written to disk, and default filenames are used if \code{toFile} is not specified.  If \code{toFile} \emph{is}
specified for such resources, one filename per ID is required.  In the case of \code{sequenceSet}, the filename is used exactly.
In the case of \code{reads}, several files are associated to each ID, so each specified filename is used as a prefix, and an
appropriate extension is appended.

ALSO HERE I NEED A CLEARER VISION OF RECONCILIATION OF FILENAMES PROVIDED AND FILESTREAMS RECEIVED
}
\note{
Remember to set an authentication key when attempting access to private data.
}
\value{
An object of the type requested, or a list of such objects if multiple IDs are specified.
However, if the \code{toFile} parameter was specified, its value is the return value.
}
\author{Daniel Braithwaite}
\seealso{
callRaw, mGet, matrixView, Collections,
\code{\link{Authentication}}
Resources and Identifiers
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
