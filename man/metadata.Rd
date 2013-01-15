\name{metadata}
\alias{metadata}
\title{Access Metadata of Collection}
\description{Accesses metadata of an existing collection, or of specified metagenomes}
\usage{metadata(x, ...)}
\arguments{
  \item{x}{...}
  \item{\dots}{...}
}
\details{Metadata elements are accessed in various ways, by field name.  See below for examples.}
\value{...}
\references{
\url{http://blog.metagenomics.anl.gov/metadata-in-mg-rast/},\cr
\url{http://gensc.org/gc_wiki/index.php/MIxS}
}
\author{Daniel T. Braithwaite}
\seealso{
	\code{\link{Collections}},
	\code{\link{metadata-class}},
	\code{\link{metadata-methods}}
}
\examples{
# collections have metadata
mm <- metadata(Waters)
mm <- metadata(collection(waters))

# retrieve only metadata retrieved for a set of IDs
mm <- metadata(guts)

# single index (length one)
mm["0464"]

# single index (length one)
mm["body product"]

# two indices (each length one)
mm["latitude", "longitude"]

# same, but return value is formatted as data.frame
mm["latitude", "longitude", bygroup=TRUE]

# single index (length two)
mm[c("0464", "env package.data")]

# three indices (each length two)
mm[c("0464", "PI"), c("0464","seq "), c("0464","biome")]
}

