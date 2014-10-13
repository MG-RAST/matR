\name{princomp.biom}
\alias{princomp}
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

\note{
The function "princomp()" is declared S3 generic in the base package "stats".
The method documented here does not apply any existing methods, however,
but rather relies on entirely different libraries for computation and graphical rendering.
(See below.)
}

\seealso{
\code{ecodist::\link[ecodist]{pco}},
\code{scatterplot3d::\link[scatterplot3d]{scatterplot3d}},
\code{graphics::\link{points}},
\code{graphics::\link{text}},
\code{\link{distx}},
\code{BIOM.utils::\link{biom}}
}

\examples{
#  quick two or three dimensional plots with choice of dissimilarity measure
princomp (xx1)
princomp (xx1, dim=2:3, method="bray-curtis")

#  graphical tweaks incorporating metadata
columns (xx1, "host_common_name|samp_store_temp")
princomp (xx1, dim=1:2, map=c(col="host_common_name", pch="samp_store_temp"),
	col=c(Mouse="brown", cow="red", "striped bass"="blue"),
	pch=c("-80"="+","NA"="*"), cex=2, label.pos=c(4,4,2,2,2,2,4), label.font=3)

#  transformed data, labeling from metadata, and modified perspective
columns (xx2, "material")
princomp (transform (xx2, t_Log), map=c(col="material"), labels="$$project.id", 
  angle=50, mar=c(1,1,0,0))
}
