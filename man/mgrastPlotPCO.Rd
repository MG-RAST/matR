\name{mgrastPlotPCO}
\alias{Analysis Tools}
\alias{mgrastPlotPCO}
\title{
mgrastPlotPCO
}
\description{
Performs a PCoA analysis on a taxon abundance matrix, using the selected distance metric.  Output always produces a \samp{.PCoA} file that has the normalized eigenvalues (top n lines) and eigenvectors (bottom n x m matrix, n lines) where n is the number of variables (e.g. subsystems), and m the number of samples. You can also choose to produce a \samp{.DIST} file that contain the distance matrix used to generate the PCoA.
}
\usage{
mgrastPlotPCO(file_in, input_dir = "./", output_PCoA_dir = "./", print_dist = FALSE, output_DIST_dir = "./", dist_method = "bray-curtis", headers = FALSE)
}
\arguments{
  \item{file_in}{
input filename
}
  \item{input_dir}{
directory path of input file
}
  \item{output_PCoA_dir}{
directory path for \samp{.PCoA} output file
}
  \item{print_dist}{
whether to print the \samp{.DIST} output file 
}
  \item{output_DIST_dir}{
directory path for \samp{.DIST} output file
}
  \item{dist_method}{
distance / dissimilarity metric to apply
}
  \item{headers}{
whether to print headers in \samp{.PCoA} output file
}
}
\value{
Returns 0 on success.\cr
Returns -1 when cranky.
}
\references{
\href{http://en.wikipedia.org/wiki/Principal_component_analysis}{Principal component analysis}
}
\author{
Kevin Keegan, Daniel Braithwaite
}
\note{
A pirate's favorite programming language!
}
\seealso{
\code{\link{sample_matrix}}
}
\examples{
#No examples for you!
}
\keyword{ PCA }
\keyword{ PCoA }
\keyword{ similarity }
\keyword{ dissimilarity }
\keyword{ distance }

