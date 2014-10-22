\name{BIOMsubset}
\alias{[.biom}

\title{
Take part of (subset) BIOM data
}

\description{
Take part of (subset) an object of class \code{biom} by removing rows, columns, or both.
}

\usage{
\method{[}{biom}(x, i, j, ...)
}

\arguments{
  \item{x}{an object (\code{biom})}
  \item{i}{row index (\code{integer}, \code{character}, or \code{logical})}
  \item{j}{column index (\code{integer}, \code{character}, or \code{logical})}
  \item{\dots}{unused}
}

\details{
Complete technical documentation is forthcoming.  
For the current preliminary release, please refer
to the examples provided.
}

\value{
A \code{biom} object, the specified subset of \code{x}.
}

\author{Daniel T. Braithwaite}

\seealso{
\code{BIOM.utils::\link{biom}},
\code{BIOM.utils::\link{dim.biom}}
}

\examples{
####  explicit subsetting
xx3 [ , 1:8]
xx4 [c ("Bacteria", "Eukaryota"), c ("mgm4575333.3", "mgm4575334.3", "mgm4575335.3")]

####  keep only metagenomes from one biome
xx3 [ , columns (xx3, "biome") == "Tundra biome"]

####  keep only rows matching a search term
xx1 [grepl ("Protein secretion system", rownames(xx1)), ]
}
