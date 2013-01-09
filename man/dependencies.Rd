\name{dependencies}
\alias{dependencies}
\alias{Dependencies}
\title{Install Package Dependencies of matR}
\description{Attempts to install certain add-on packages (some of which are essential)}
\usage{dependencies(prompt = TRUE)}
\arguments{
  \item{prompt}{...}
}
\details{
This routine attempts to install, from repositories chosen by the user, the packages listed in the \code{Suggests:} field of the \code{matR} \code{DESCRIPTION} file.  
This is recommended.
Functionality in \code{matR} is modular wherever possible, meaning that during interactive sessions add-on packages are not loaded until strictly necessary.  
Consequently, it is possible to work without the full set of suggested packages.  
On the other hand, \code{matR} readily incorporates further add-on packages as needed for specific analyses.}
\references{\code{\link{matR-package}}, \code{\link{hazPackages}}}
\author{Daniel T. Braithwaite}
\examples{
\dontrun{
dependencies()
}
}
