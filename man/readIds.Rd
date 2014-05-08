\name{readIds}
\alias{readIds}

\title{Read Metagenome IDs from File}
\description{
Reads IDs from a text file and returns as a character vector; a convenience function.
IDs are expected in the first column.
A second column (if present) is used to specify the names attribute.
The third and any additional columns are ignored. 
}
\usage{
readIds(file, ...)
}
\arguments{
  \item{file}{name of file (character)}
  \item{\dots}{currently unused}
}
%\details{...}
%\value{...}
\author{Daniel T. Braithwaite}
\seealso{\code{\link{Identifiers}}}
\examples{
##
}
