\name{render}
\alias{render}
\alias{Visualization}

\title{Produce Graphic of Computed Analysis}
\description{
Produces default visualizations of analysis products.  Under construction.  Methods are intended for:  collection, pco, parcoord, heatmap, sigtest.
}
\usage{
render(x, ...)
}
\arguments{
  \item{x}{renderable object (see above)}
  \item{\dots}{...}
}
%\details{...}
%\value{...}
\author{Daniel T. Braithwaite}
\seealso{
\code{\link{Collections}}
}
\examples{
render (Whalebone)

# look at raw counts (instead of normed)
render (Marine, view = "raw")

# apply standard graphical parameters
render (Waters, las = 2, cex.axis = 0.6, main = "summary of annotation\nfrequencies")
}
