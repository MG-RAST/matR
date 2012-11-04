\name{Dependencies}
\alias{Dependencies}
\alias{dependencies}
\title{Install Package Dependencies of matR}
\description{
Attempts to install suggested add-on packages.  Installation of \code{matR} requires no packages beyond base \code{R}, but little can be accomplished without the suggested packages.
}
\usage{
dependencies()
}
\details{
This routine attempts to install from repositories chosen by the user the packages listed in the \code{Suggests:} field of the \code{matR} \code{DESCRIPTION} file.  Functionality in \code{matR} is modular wherever possible, meaning that during interactive sessions add-on packages are not loaded until strictly necessary.  Consequently, it is possible to work without the full set of suggested packages.  On the other hand, \code{matR} readily incorporates further add-on packages as needed for specific analyses.}
\references{\code{\link{matR-package}}}
\author{Daniel T. Braithwaite}
