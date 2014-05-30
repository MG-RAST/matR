\name{asFile}
\alias{asFile}
\title{Write Metagenome and Analysis Objects to File}
\description{
	Exports data.  Methods are implemented or in progress for:  collection, matrix, biom, boxplot, pco, parcoord, heatmap, sigtest.  Text-format output is the intention here, but where a method is yet undefined, \code{save()} is used as the default, which writes a binary object image.
}
\usage{asFile (x, file, ...)}
\arguments{
	\item{x}{object to write (any)}
	\item{file}{name of file (character)}
	\item{...}{currently unused}
}
%\details{...}
%\value{...}
\author{Daniel T. Braithwaite}
\seealso{\code{\link{asFile-methods}}, \code{\link{msession}}, \code{\link{save}}}
\examples{
##
}
