\name{MetadataRetrieval}
\alias{Metadata Retrieval}
\alias{MetadataRetrieval}
\alias{mProjectMeta}
\alias{mSampleMeta}
\alias{mMetagenomeMeta}
\title{Retrieve Specified Metadata from the Kbase MG-RAST Server}
\description{...}
\usage{
mProjectMeta (projectIDs)
mSampleMeta (sampleIDs)
mMetagenomeMeta (metagenomeIDs)
}
\arguments{
\item{projectIDs}{...}
\item{sampleIDs}{...}
\item{metagenomeIDs}{...}
}
\details{...}
\value{In each case, an S4 class (with a few appropriate methods defined) that mostly amounts to a list of character vectors.}
\references{
\url{http://metagenomics.anl.gov}
}
\author{Daniel T. Braithwaite}
\seealso{\code{\link{MG-RAST Identifiers}}, \code{\link{mGet}}}
\examples{
mProjectMeta ("mgp24")
summary (mSampleMeta ("mgs17404"))
}