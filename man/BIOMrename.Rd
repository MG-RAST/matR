\name{BIOMrename}
\alias{dimnames<-.biom}

\title{
Change row and column identifiers of BIOM data
}

\description{
Change the \code{dimnames} (BIOM row and column \code{ids}) of an object of class \code{biom}.
}

\usage{
\method{dimnames}{biom}(x) <- value
}

\arguments{
  \item{x}{an object (\code{biom})}
  \item{value}{new row and column identifiers (\code{list} of \code{character})}
}

\details{
Complete technical documentation is forthcoming.  
For the current preliminary release, please refer
to the examples provided.
}

\value{
The argument \code{x} with updated row and column \code{id}s (that is, \code{dimnames}).
}

\author{Daniel T. Braithwaite}

\seealso{
\code{BIOM.utils::\link{biom}},
\code{BIOM.utils::\link{dimnames.biom}}
}

\examples{
####  even if not particularly useful, this is allowed
yy <- xx4
dimnames (yy) <- list (letters [1:nrow(yy)], LETTERS [1:ncol(yy)])

####  more useful: renaming columns by codes taken from metadata
colnames (yy) <- columns (yy, "sample.data.sample_name")
}
