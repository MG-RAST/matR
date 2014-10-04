\name{image.biom}
\alias{image.biom}

\title{
Display heatmap of BIOM data with optional dendrograms
}

\description{
Display heatmap of a "biom" object with optional row and column dendrograms.
}

\usage{
\method{image}{biom}(x, ..., map=NULL, rows=TRUE, columns=TRUE, rerender=NULL)
}

\arguments{
  \item{x}{an object ("biom")}
  \item{\dots}{arguments to "gplots::heatmap.2()"}
  \item{map}{assignment of "par" variables to metadata fields ("character")}
  \item{rows}{subselection of rows ("integer", "character" or "logical")}
  \item{columns}{subselection of columns ("integer", "character" or "logical")}
  \item{rerender}{previous computation to reuse in this call ("heatmap", "dclust", "list" or "dist")}
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
\code{gplots::\link[gplots]{heatmap.2}},
\code{BIOM.utils::\link{biom}}
}

\examples{
}
