\name{mconfig}
\alias{mconfig}
\alias{Configuration}
\alias{configuration}
\alias{mconfig$par}
\alias{mconfig$exp}
\alias{mconfig$imp}
\title{Get or Set Session Configuration}
\description{
  \code{mconfig} is a data object created in each session in the \code{matR} package environment to hold configuration information for that session.
}
\usage{
`mconfig$par` (...)
`mconfig$exp` (...)
`mconfig$imp` (...)
`mconfig$path` (s = NULL)
}
\arguments{
  \item{...}{...}
  \item{s}{...}
}
\details{
  \code{mconfig$par()}, \code{mconfig$exp()}, and \code{mconfig$imp()} get or set session configuration options for (respectively) graphical outputs, exports to files, and imports from files.
  See \code{Visualization} and \code{Import and Export} for details.
  
  \code{mconfig$path(s)} sets a filesystem path that will be prepended to all filenames used during the session.  The current path is returned by \code{mconfig$path()}.

  Authentication information is also mediated by \code{mconfig}.  See \code{Authentication}.
}
\value{
  \code{mconfig$path()} returns the current filesystem path as a length one character vector.  All other functions documented here return \code{NULL}.
}
\references{
}
\author{Daniel Braithwaite}
\seealso{\code{\link{Authentication}}, \code{\link{Visualization}}, \code{\link{Import and Export}}}
\examples{
}
