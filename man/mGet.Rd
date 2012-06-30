\name{mGet}
\alias{mGet}
\title{
Retrieve Resources from the MG-RAST API
}
\description{...}
\usage{
	mGet(resource, ID, namespace = NULL, annoType = NULL, seqType = NULL, org = NULL, func = NULL, md5 = NULL, param = NULL, parse = TRUE, enClass = FALSE, toFile = NULL)
}
\arguments{
	\item{resource}{
One of \code{"project"}, \code{"sample"}, \code{"library"}, \code{"metagenome"}, \code{"subset"}, \code{"sequenceSet"}, \code{"sequences"}, \code{"reads"}, \code{"abundance"}}
	\item{ID}{One or multiple ID(s), appropriate to the resource requested}
	\item{namespace}{...}
	\item{annoType}{...}
	\item{seqType}{...}
	\item{org}{...}
	\item{func}{...}
	\item{md5}{...}
	\item{param}{...}
  \item{parse}{Should JSON be parsed?}
  \item{enClass}{Should an attempt be made to put the returned object in a formal class?}
	\item{toFile}{...}
}
\details{
This function retrieves output from the MG-RAST pipeline, of different kinds according to specified parameters.  
The \code{resource} parameter determines what kind of data is retrieved.  Some resources are primarily 
textual objects, others are numerical objects, and others are files.  An \code{ID} is always required, appropriate to the kind of 
resource requested.

\code{ID} may contain multiple values in most calls.  Many reasonable formats for \code{ID} will work, such as:\cr
\code{123}\cr
\code{"123"}\cr
\code{c(123,456)}\cr
\code{c("123","456")}\cr
\code{"123;456;789"}\cr
\code{c(123,"456;789")}\cr
Depending on the \code{resource} requested, multiple values may be (similarly) specified in 
the \code{namespace}, \code{org}, \code{func}, \code{md5}, and \code{toFile} parameters.

Output may be directed to disk with the \code{toFile} parameter.  Single or multiple filenames may be specified.  
In many cases, it is ok to call with multiple IDs and a single filename, and the result is that the downloaded resources
will be concatenated into a single file.  If \code{resource} is
\code{sequenceSet} or \code{reads}, however, the result is \emph{always} written to disk, and default filenames are used if \code{toFile} is
not specified.  If \code{toFile} \emph{is} specified for these resources, one filename per ID is required.  In the case of \code{sequenceSet},
the filename is used exactly.  In the case of \code{reads}, several files are associated to each ID, so each
specified filename is used as a prefix, and an appropriate extension is appended.

If \code{mGet} does not function properly, you might be requesting private data with an invalid authorization key.
See \code{Authentication}.
}
\value{
An object of the type requested, or a list of such objects if multiple IDs are specified.  
However, if the \code{toFile} parameter was specified, its value is the return value.
}
\author{Daniel Braithwaite}
\seealso{
\code{\link{Authentication}}
	}
\examples{
## Not run:
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
## End(Not run)
}
