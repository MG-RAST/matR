\name{MGRASTQueries}
\alias{Querying MG-RAST}
\alias{mListAll}
\alias{mSearchMetagenomes}
\alias{mAnnotationLookup}
\title{Find IDs for Kbase MG-RAST Resources; Look Up Annotations}
\description{...}
\usage{
mListAll (resource)
mSearchMetagenomes (resource, attribute = NULL, value = NULL)
mAnnotationLookup (md5, namespace)
}
\arguments{
	\item{resource}{One of \code{"project"}, \code{"sample"}, \code{"library"}, \code{"annotation"}, \code{"metagenome"}}
	\item{md5}{...}
	\item{namespace}{...}
	\item{attribute}{...}
	\item{value}{...}
	}
\details{...}
\value{...}
\references{
\url{http://blog.metagenomics.anl.gov/howto/m5nr-\%E2\%80\%94-the-m5-non-redundant-protein-database/}
}
\author{Daniel Braithwaite}
\seealso{\code{\link{mGet}}}
\examples{
## mListAll ("project")
}
