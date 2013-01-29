\name{randomize}
\alias{randomize}
\title{
Create and Summarize Random Permutations of a Given Matrix
}
\description{
Applies a given function to random permutations of a given matrix
}
\usage{
randomize(x, ntimes = 1, method = c("sample", "dataset", "complete"), seed = NULL, FUN = identity, ...)
}
\arguments{
  \item{x}{...}
  \item{ntimes}{...}
  \item{method}{...}
  \item{seed}{...}
  \item{FUN}{...}
  \item{\dots}{...}
}
\details{...}
\value{The form of results depends on the return value of the supplied summarizing function.  By  default, the random permutation matrices themselves are returned as a list, but a function should be supplied in place of the identity.}
\author{Kevin P. Keegan, Daniel T. Braithwaite}
\seealso{...}
\examples{
#
}
