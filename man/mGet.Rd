\name{mGet}
\alias{mGet}
\title{Structured Access to API Resources by ID}
\description{...}
\usage{
	mGet(resource, ID, namespace = NULL, annoType = NULL, seqType = NULL, org = NULL, func = NULL, md5 = NULL, param = NULL, parse = TRUE, enClass = FALSE, toFile = NULL)
}
\arguments{
	\item{resource}{
One of \code{"project"}, \code{"sample"}, \code{"library"}, \code{"metagenome"}, \code{"subset"}, \code{"sequenceSet"}, \code{"sequences"}, \code{"reads"}, \code{"abundance"}}
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
\code{matR} provides four layers of access to \code{MG-RAST}, represented by the functions \code{callRaw}, \code{mGet}, \code{matrixView}, and \code{collection}.

The purpose of \code{mGet} is to provide structured access to API resources in an \code{R}-friendly manner.  Using this function effectively requires familiarity with the API documented at \link{http://api.metagenomics.anl.gov}.  \code{resource}s returned may be files, textual, numerical, or other.  Some are, and some are not, transmitted in \code{JSON}.  All API resources implemented by {mGet} require an \code{ID} appropriate to the kind of resource requested.

\code{ID} is permitted to contain multiple values in many calls.  Many intuitive formats for \code{ID} will work.  See \link{Identifiers} for more details.

Depending on the \code{resource} requested, multiple values may be (similarly) specified in 
the \code{namespace}, \code{org}, \code{func}, \code{md5}, and \code{toFile} parameters.

The results of the call may be directed to file(s) with the \code{toFile} parameter.  Single or multiple filenames may be specified.  
In many cases, it is ok to call with multiple IDs and a single filename, and the result is that the downloaded resources
will be concatenated into a single file.  If \code{resource} is \code{sequenceSet} or \code{reads}, however, the result is
\emph{always} written to disk, and default filenames are used if \code{toFile} is not specified.  If \code{toFile} \emph{is}
specified for these resources, one filename per ID is required.  In the case of \code{sequenceSet}, the filename is used exactly.
In the case of \code{reads}, several files are associated to each ID, so each specified filename is used as a prefix, and an
appropriate extension is appended.
}
\note{
Remember to set an authentication key when attempting access to private data.
}
\value{
An object of the type requested, or a list of such objects if multiple IDs are specified.  
However, if the \code{toFile} parameter was specified, its value is the return value.
}
\author{Daniel Braithwaite}
\seealso{callRaw, mGet, matrixView, Collections,
\code{\link{Authentication}}
Resources and Identifiers
}
\examples{
allProjectIDs <- mListAll("project")

M <- mGet( "metagenome", "4447971.3" )
print (M)
PList <- mGet( "project", allProjectIDs [1:10])
P <- PList [[1]]
print (P)
% L <- mGet( "library", 3482.4 )
S <- mGet( "sample", 441 )

% mGet ("metagenome", mListAll( "metagenome" ), toFile = "metagenomeIDs" )
% mGet ("project", allProjectIDs, toFile = paste ("project.", allProjectIDs, sep = ""))

% mGet ("abundance", "4443360.3;4443361.3;4443362.3;4443363.3;4443364.3;4443365.3;4443366.3")
ab <- mGet ("abundance", "4443360.3;4443361.3;4443362.3;4443363.3;4443364.3;4443365.3;4443366.3;4443367.3;4443368.3")
print (ab)
% mDist (M)
% mPCO (M)

% sub <- mGet( "subset", "4447971.3", org="Abiotrophia\%20defectiva\%20ATCC\%2049176" )
% seq <- mGet( "sequences", "4441602.3", md5="5c6cdf00b3b2509879f412d55582af1a" )
% M1 <- mGet( "abundance", c(4443360.3,4443361.3,4443362.3), namespace="SEED" )
% M2 <- mGet( "abundance", "4443360.3;4443361.3;4443362.3", namespace="SEED", toFile="myfile" )
% M3 <- mGet( "abundance", "4443360.3;4443361.3;4443362.3", namespace="SEED", param="format/plain" )
}
