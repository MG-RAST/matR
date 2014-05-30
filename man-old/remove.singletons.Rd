\name{remove.singletons}
\alias{remove.singletons}

\title{Scrub Low-Abundance Entries from Matrix}
\description{Replaces NA and single counts (or entries below a threshold) with zero, and removes rows of zeros (or rows with sum below a threshold).}
\usage{remove.singletons(x, lim.entry = 1, lim.row = 1, ...)}
\arguments{
  \item{x}{matrix}
  \item{lim.entry}{entry threshold for replacement with zero (numeric)}
  \item{lim.row}{row sum threshold for row removal (numeric)}
  \item{...}{currently unused}
}
%\details{...}
\value{The scrubbed matrix.}
\author{Kevin P. Keegan, Daniel T. Braithwaite}
\examples{
##
}
