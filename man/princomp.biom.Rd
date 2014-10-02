\name{princomp.biom}
\alias{princomp.biom}

\title{
Compute and plot principal coordinates of BIOM data
}

\description{
Compute principal coordinates of a "biom" object (columnwise), and plot selected coordinates.
}

\usage{
\method{princomp}{biom}(x, method="euclidean", dim=1:3, ..., 
	map=NULL, rows=TRUE, columns=TRUE, rerender=NULL)
}

\arguments{
  \item{x}{an object ("biom")}
  \item{method}{name of distance or dissimilarity measure ("character")}
  \item{dim}{which principal coordinates to plot ("integer")}
  \item{\dots}{arguments to "scatterplot3d()", "points()", or "text()"}
  \item{map}{assignment of "par" variables to metadata fields ("character")}
  \item{rows}{subselection of rows ("integer", "character" or "logical")}
  \item{columns}{subselection of columns ("integer", "character" or "logical")}
  \item{rerender}{previous computation to reuse in this call ("pco" or "dist")}
}

\details{
Complete technical documentation is forthcoming.  
For the current preliminary release, please refer
to the examples and vignettes provided.
}

\value{
Complete technical documentation is forthcoming.  
For the current preliminary release, please refer
to the examples and vignettes provided.
}

\author{Daniel T. Braithwaite and Kevin P. Keegan}

\seealso{
\code{ecodist::\link[ecodist]{pco}},
\code{scatterplot3d::\link[scatterplot3d]{scatterplot3d}},
\code{graphics::\link{points}},
\code{graphics::\link{text}},
\code{\link{distx}},
\code{BIOM.utils::\link{biom}}
}

\examples{
}
