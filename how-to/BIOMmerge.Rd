\name{BIOMmerge}
\alias{merge.biom}

\title{
Merge BIOM data
}

\description{
Merge two objects of class \code{biom}, maintaining metadata and other class structure.
}

\usage{
\method{merge}{biom}(x, y, ...)
}

\arguments{
  \item{x}{an object (\code{biom})}
  \item{y}{an object (\code{biom})}
  \item{\dots}{unused}
}

\details{
Complete technical documentation is forthcoming.  
For the current preliminary release, please refer
to the examples provided.
}

\value{
A \code{biom} object resulting from merging \code{x} and \code{y}.
}

\author{Daniel T. Braithwaite}

\seealso{
\code{BIOM.utils::\link{biom}}
}

\examples{
####  merging requires only that all colnames be unique, so nonsense can be performed
merge (xx1, xx4)

####  a more likely example, based on applying different normalizations
aa <- transform (xx4 [,1:8], t_Threshold, t_Log)
bb <- transform (xx4 [,9:16], t_Threshold=list(entry.min=5), t_Log)
xx4_norm <- merge (aa, bb)
}
