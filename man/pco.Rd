\name{pco}
\alias{pco}
\alias{Principal Coordinates Analysis}
\title{
Perform Principal Coordinates Analysis}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pco(x, method = "bray-curtis")        % add ... parameters and discuss graphical options
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
}
\details{
  This function wraps function \code{pco} from package \code{ecodist} in a form convenient for \code{matR}.
}
\value{
  Assuming the analyis is successful, an object of class \code{pco}.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\code{\link{pco-methods}}, \link{pco-class}}, \code{\link{render}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, method = "bray-curtis") 
{
    reqPack("ecodist")
    x <- as.matrix(x)
    D <- matR::mdist(x, method)
    P <- ecodist::pco(D)
    scaled <- P$values/sum(P$values)
    names(scaled) <- paste("PCO", 1:length(scaled), sep = "")
    dimnames(P$vectors)[[1]] <- dimnames(x)[[2]]
    list(values = scaled, vectors = P$vectors, distances = D)
  }
}
