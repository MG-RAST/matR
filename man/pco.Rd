\name{pco}
\alias{pco}
\title{Perform Principal Coordinates Analysis of Collection}
\description{Performs PCoA analysis, returning the results and plotting in two or three dimensions}
\usage{pco(x, ...)}
\arguments{
  \item{x}{...}
  \item{\dots}{...}
}
\details{
  This function wraps function \code{pco} from package \code{ecodist}.
}
\value{...}
\author{Kevin P. Keegan, Daniel T. Braithwaite}
\seealso{
\code{\link{pco-methods}}, \code{\link{pco-class}}, \code{\link{render}}
}
\examples{
# basic plot
pco (Coral)

# color and label points according to a grouping
groups (Waters) <- c (rep (1, 15), rep (2, 9))
pco (Waters, comp = c (1,2), main = "fresh vs. spring water samples", col = c (rep ("blue", 15), rep ("red", 9)))

# plot three components
# note parameter "color" here; "col" above
col <- factor (metadata (Hospital) ["ventilation_type"])
levels (col) <- c ("red", "blue", "orange")
col <- as.character (col)
pco (Hospital, comp = c (1,2,3), color = col, labels = "")
}
