\name{randomize}
\alias{randomize}

\title{Apply Function to Matrix Permutations for Randomization Tests}
\description{
Applies given function repeatedly to random permutation of a given matrix, providing a general-purpose routine to facilitate randomization tests.
}
\usage{
randomize(x, ntimes = 1, method = c("sample", "dataset", "complete"), seed = NULL, FUN = identity, ...)
}
\arguments{
  \item{x}{matrix}
  \item{ntimes}{how many permutations (numeric)}
  \item{method}{type of permutation (character)}
  \item{seed}{seed for random number generation (numeric)}
  \item{FUN}{should accept single matrix argument and return summary value (function)}
  \item{\dots}{currently unused}
}
%\details{...}
\value{The form of results depends on the return value of the supplied summarizing function.  By  default, the random permutation matrices themselves are returned as a list, but a function should be supplied in place of the identity.}
\author{Kevin P. Keegan, Daniel T. Braithwaite}
%\seealso{...}
\examples{
##
}
