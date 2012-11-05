\name{MGRASTIdentifiers}
\alias{MGRASTIdentifiers}
\alias{MG-RAST Identifiers}
\alias{mProjects}
\alias{mSamples}
\alias{mMetagenomes}
\title{Listing Valid Resource IDs}
\description{
Each function simply returns a list of all valid IDs in MG-RAST for the resource of the specified kind.}
\usage{
mProjects()
mSamples()
mMetagenomes()
}
\details{...}
\value{A character vector.}
\references{\url{http://api.metagenomics.anl.gov}}
\author{Daniel T. Braithwaite}
\seealso{\code{\link{Abundance Retrieval}}, \code{\link{Metadata Retrieval}}, \code{\link{mGet}}}
\examples{
mProjects()
}
