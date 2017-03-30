\name{image.biom}
\alias{image}
\alias{image.biom}

\title{
Display heatmap of BIOM data with optional dendrograms
}

\description{
Display heatmap of a \code{biom} object with optional row and column dendrograms.
}

\usage{
\method{image}{biom}(x, ..., map=NULL, rows=TRUE, columns=TRUE, rerender=NULL)
}

\arguments{
  \item{x}{an object (\code{biom})}
  \item{\dots}{arguments to \code{gplots::heatmap.2()}}
  \item{map}{assignment of \code{par} variables to metadata (\code{character})}
  \item{rows}{subselection of rows (\code{integer}, \code{character}, or \code{logical})}
  \item{columns}{subselection of columns (\code{integer}, \code{character}, or \code{logical})}
  \item{rerender}{previous computation to reuse in this call (\code{heatmap}, \code{dclust}, \code{list}, or \code{dist})}
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

\note{
The function \code{image()} is declared S3 generic in the base package \code{graphics}.
The method documented here does not apply any existing methods, however,
but rather relies on entirely different libraries for computation and graphical rendering.
See reference below.
}

\author{Daniel T. Braithwaite and Kevin P. Keegan}

\seealso{
\code{gplots::\link[gplots]{heatmap.2}},
\code{BIOM.utils::\link{biom}}
}

\examples{
####  simple heatmap; using log transformation makes interesting things more apparent
image (xx2)
xx2t <- transform (xx2, t_Log)
image (xx2t, labCol="$$project.id")

####  clustering analysis restricted to Archaea
image (xx2t, labCol="$$project.id", rows=rows(xx2t,"taxonomy1")=="Archaea")

####  clustering analysis restricted by significance test p values
p <- rowstats (xx2t, test="t-test-unpaired", groups="$$material") $ p.value
p [is.na(p)] <- p [is.nan(p)] <- FALSE
image (xx2t [rows = p < 0.05, ], labCol="$$material")
}
