\name{kbGet}
\alias{Data Retrieval from MG-RAST}
\alias{kbGet}
\title{
Retrieve Resources from the Kbase MG-RAST Server
}
\description{
}
\usage{
kbGet(ID, resource, namespace = NULL, annoType = NULL, seqType = NULL, org = NULL, func = NULL, md5 = NULL, param = NULL, toFile = NULL)
}
\arguments{
  \item{ID}{
An ID appropriate to the resource requested}
  \item{resource}{
One of \code{"project"}, \code{"sample"}, \code{"library"}, \code{"metagenome"}, \code{"subset"}, \code{"sequenceSet"}, \code{"sequences"}, \code{"reads"}, \code{"abundance"}
}
  \item{namespace}{
%%     ~~Describe \code{namespace} here~~
}
  \item{annoType}{
%%     ~~Describe \code{annoType} here~~
}
  \item{seqType}{
%%     ~~Describe \code{seqType} here~~
}
  \item{org}{
%%     ~~Describe \code{org} here~~
}
  \item{func}{
%%     ~~Describe \code{func} here~~
}
  \item{md5}{
%%     ~~Describe \code{md5} here~~
}
  \item{param}{
%%     ~~Describe \code{param} here~~
}
  \item{toFile}{
%%     ~~Describe \code{toFile} here~~
}
}
\details{
Where appropriate, parameters may hold (indifferently) either numeric or character values.  Also, a single parameter is often allowed to carry multiple values.
Multiple values may be passed within a vector or semicolon-separated character string.  Accordingly, these are all valid values for the \code{ID} parameter:\cr
\code{123}\cr
\code{"123"}\cr
\code{c(123,456)}\cr
\code{c("123","456")}\cr
\code{"123;456"}\cr
\code{c(123,"456;789")}\cr
Similar usage is allowed, depending on the \code{resource} requested, within the \code{namespace}, \code{org}, \code{func}, and \code{md5} parameters.

If \code{kbGet} does not function properly, the authorization key may need to be reset.  See \code{\link{Kbase Authorization}}.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Daniel Braithwaite}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{kbListAll}}, \code{\link{.kbCallRaw}}
}
\examples{
P <- kbGet( 92, "project" )
L <- kbGet( 3482.4, "library" )
S <- kbGet( 441, "sample" )
M <- kbGet( "4447971.3", "metagenome" )
sub <- kbGet( "4447971.3", "subset", org="Abiotrophia\%20defectiva\%20ATCC\%2049176" )
seq <- kbGet( "4441602.3", "sequences", md5="5c6cdf00b3b2509879f412d55582af1a" )
M1 <- kbGet( c(4443360.3,4443361.3,4443362.3), "abundance", namespace="SEED" )
M2 <- kbGet( "4443360.3;4443361.3;4443362.3", "abundance", namespace="SEED", toFile="myfile" )
M3 <- kbGet("4443360.3;4443361.3;4443362.3", "abundance", namespace="SEED", param="format/plain" )
}
