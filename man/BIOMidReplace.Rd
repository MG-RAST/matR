\name{BIOMidReplace}
\alias{dimnames<-.biom}

\title{
Change row and column identifiers of BIOM data
}

\description{
Change the "dimnames" (that is, the BIOM row and column "ids") of an object of class "biom".
}

\usage{
\method{dimnames}{biom}(x) <- value
}

\arguments{
  \item{x}{an object ("biom")}
  \item{value}{new row and column identifiers ("list" of "character")}
}

\details{
Complete technical documentation is forthcoming.  
For the current preliminary release, please refer
to the examples and vignettes provided.
}

\value{
The argument "x" with updated row and column "id"s (that is, "dimnames").
}

\author{Daniel T. Braithwaite}

\seealso{
\code{BIOM.utils::\link{biom}},
\code{BIOM.utils::\link{dimnames.biom}}
}

\examples{
#  even if not particularly useful, this is allowed
yy <- xx4
dimnames (yy) <- list (letters [1:nrow(yy)], LETTERS [1:nrow(yy)])

#  more useful: renaming columns by codes taken from metadata
colnames (yy) <- columns (yy, "sample.data.sample_name")
}
