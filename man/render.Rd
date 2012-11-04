\name{render}
\alias{render}
\alias{Visualizations}
\alias{visualizations}
\alias{Visualization}
\alias{visualization}
\title{Produces Default Graphical Representation of an Analysis}
\description{
  A generic function to put analyses into graphical form.
  Current methods reproduce visualizations also implemented on the \code{MG-RAST} analysis webpages.
}
\usage{
render (x, ...)
}
\arguments{
  \item{x}{...}
  \item{...}{...}
}
\details{
Applied to a collection object, produces boxplots.  Applied to a heatmap, produces a heatmap.  Applied to a PCoA, produces a PCoA plot.  [Give details of graphical parameters.]
}
\value{
\code{NULL}, or the filename, if the visualization is written to a file.
}
\author{Daniel T. Braithwaite}
\seealso{
\code{\link{collection}}, \code{\link{pco}}, \code{\link{heatmap}}, \code{\link{render-methods}}, \code{\link{mconfig}}}
\examples{
render (collection (c (alpha = "4441872.3", bravo = "4442034.3", charlie = "4448888.3")))

aa <- collection (guts)
render (aa)
render (heatmap (aa))
render (pco (aa))
}
