\name{metadata}
\alias{metadata}
\alias{Metadata}
\title{Access Metadata of Objects}
\description{}
\usage{
metadata (x, ...)
}
\arguments{
  \item{x}{...}
  \item{...}{...}
}
\details{
Metadata in \code{matR} is }
\value{
	different according to object type
	for full details link to page 
  In each case, a metadata object of S4 class "rlist" that mostly amounts to a list of character vectors.
	For more about metadata, see the MG-RAST webpage linked below.
  }
\references{
\url{http://blog.metagenomics.anl.gov/metadata-in-mg-rast/}
}
\author{Daniel T. Braithwaite}
\seealso{
\code{\link{rlist-class}}, \code{\link{selection}}, \code{\link{metadata-methods}}
}
\examples{
\dontrun{
metadata ("4445567.3;4445568.3")
metadata (selection) 
metadata (collection)
}
}
