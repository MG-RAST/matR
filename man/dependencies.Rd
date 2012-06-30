\name{dependencies}
\alias{dependencies}
\alias{Dependencies}
\title{Install Package Dependencies of matR}
\description{
	Attempts to install add-on packages to support \code{matR}.  
  Strictly speaking \code{matR} has no dependencies since installation requires no packages beyond base \code{R}.  
  However, little can be accomplished without some suggested packages.}
\usage{
  dependencies()
}
\details{This routine attempts to install from repositories chosen by the user the packages listed in the \code{Suggests:} field of the \code{matR} \code{DESCRIPTION} file.  Functionality in \code{matR} is modular wherever possible, meaning that during interactive sessions add-on packages are not loaded until strictly necessary.  Consequently, it is possible to work without the full set of suggested packages.  On the other hand, \code{matR} readily incorporates further add-on packages as needed for specific analyses.}
\value{\code{NULL}}
\references{
\url{}
}
\author{Daniel Braithwaite}
%\seealso{\code{\link{mGet}}}
\examples{
}
