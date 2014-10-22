\name{distx}
\alias{distx}
\alias{distx.matrix}
\alias{distx.biom}

\title{
Calculate distances with optional grouping and other features
}

\description{
Calculate several distances and dissimilarities with optional grouping, 
by default columnwise and pairwise,
or from an optionally specified common point.
}

\usage{
distx(x, ...)

\method{distx}{matrix}(x, method=c("euclidean", "bray-curtis", "jaccard", "mahalanobis",
  "sorensen", "difference", "maximum", "manhattan", "canberra", "binary", "minkowski"), 
  groups=NULL, p=NULL, ..., bycol=TRUE)

\method{distx}{biom}(x, method="euclidean", groups=NULL, ..., bycol=TRUE)
}

\arguments{
  \item{x}{{a matrix-like object (\code{matrix} or \code{biom})}}
  \item{method}{name of distance or dissimilarity measure (\code{character})}
  \item{groups}{a grouping of columns/rows (\code{character} or \code{factor})}
  \item{p}{a single column/row (\code{numeric})}
  \item{\dots}{unused}
  \item{bycol}{compute columnwise rather than rowwise? (\code{logical})}
}

\details{
Complete technical documentation is forthcoming.  
For the current preliminary release, please refer
to the examples provided.
}

\value{
Complete technical documentation is forthcoming.  
For the current preliminary release, please refer
to the examples provided.
}

\author{Daniel T. Braithwaite and Kevin P. Keegan}

\seealso{
\code{stats::\link{dist}},
\code{ecodist::\link[ecodist]{distance}}
}

\examples{
####  Euclidean distance between samples based on raw counts
distx (xx1)

####  alternate dissimilarity measure
distx (xx1, method="bray-curtis")

####  distance in log-transformed data
distx (transform (xx2, t_Log))

####  mean pairwise distance between biomes
distx (xx3, groups="$$biome", method="bray-curtis")
}
