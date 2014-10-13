\name{image.biom}
\alias{image}
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

\note{
The function "image()" is declared S3 generic in the base package "graphics".
The method documented here does not apply any existing methods, however,
but rather relies on entirely different libraries for computation and graphical rendering.
(See below.)
}

\author{Daniel T. Braithwaite and Kevin P. Keegan}

\seealso{
\code{gplots::\link[gplots]{heatmap.2}},
\code{BIOM.utils::\link{biom}}
}

\examples{
#  simple heatmap; using log transformation makes interesting things more apparent
image (xx2)
xx2t <- transform (xx2, t_Log)
image (xx2t, labCol="$$project.id")

#  clustering analysis restricted to Archaea
image (xx2t, labCol="$$project.id", rows=rows(xx2t,"taxonomy1")=="Archaea")

#  clustering analysis restricted by significance test p values
p <- rowstats (xx2t, test="t-test-unpaired", groups="$$material") $ p.value
p [is.na(p)] <- p [is.nan(p)] <- FALSE
image (xx2t [rows = p < 0.05, ], labCol="$$material")
}
