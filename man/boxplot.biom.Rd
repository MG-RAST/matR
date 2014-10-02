\name{boxplot.biom}
\alias{boxplot.biom}

\title{
Summarize BIOM data in boxplots
}

\description{
Summarize distribution of a "biom" object in columnwise boxplots.
}

\usage{
\method{boxplot}{biom}(x, y=NULL, ..., map=NULL, columns=TRUE)
}

\arguments{
  \item{x}{an object ("biom")}
  \item{y}{optionally, a second object for comparison ("biom")}
  \item{\dots}{arguments to "graphics::boxplot()"}
  \item{map}{assignment of "par" variables to metadata fields ("character")}
  \item{columns}{subselection of columns ("integer", "character" or "logical")}
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
\code{graphics::\link{boxplot}},
\code{BIOM.utils::\link{biom}}
}

\examples{
}
