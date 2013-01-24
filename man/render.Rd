\name{render}
\alias{render}
\alias{Visualization}
\title{Visualize Collections and Analyses}
\description{Produces default visualizations of metegenome collections and certain analyses}
\usage{
render(x, ...)
}
\arguments{
  \item{x}{...}
  \item{\dots}{...}
}
\details{...}
\value{...}
\author{Daniel T. Braithwaite}
\seealso{
\code{\link{Collections}}
}
\examples{
render (Whalebone)

# look at raw counts (instead of normed)
render (Marine, view = "count")

# apply standard graphical parameters
render (Waters, las = 2, cex.axis = 0.6, main = "summary of annotation\nfrequencies")
}
