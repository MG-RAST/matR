\name{heatmap}
\alias{heatmap}
\title{Heatmap of Collection}
\description{Creates heatmap of the specified view of a collection.  This function wraps and customizes \code{heatmap.2()} from package \code{gplots}.}
\usage{heatmap(x, ...)}
\arguments{
  \item{x}{collection}
  \item{\dots}{further arguments as indicated below}
}
%\details{...}
%\value{...}
\author{Kevin P. Keegan, Daniel T. Braithwaite}
\seealso{\code{\link{heatmap-methods}}}
\examples{
# simple use
heatmap (Waters)

# map a different view
Waters$lev2 <- c (entry = "normed", level = "level2")
heatmap (Waters, view = "lev2")

# map only selected rows, including a few graphical parameters (see ?gplots::heatmap.2)
groups (Waters) <- c (rep (1, 15), rep (2, 9))
results <- sigtest (Waters, view = "lev2", test = "Kruskal-Wallis")
heatmap (Waters, view = "lev2", rows = (results$p.value < 0.05), main = "Comparison of Water Samples", labCol = 1:24)
}
