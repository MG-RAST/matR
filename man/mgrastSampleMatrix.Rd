\name{sample_matrix}
\alias{sample_matrix}
\title{
Calculate permutations and derived stats abundance count data
}
\description{
This function calculates permutations and derived stats abundance count data.
}
\usage{
sample_matrix(file_name, num_perm = 1, perm_type = "sample_rand", write_files = FALSE, perm_dir = "./permutations/", verbose = FALSE, debug = FALSE)
}
\arguments{
  \item{file_name}{ Required name or path/name of file to process
}
  \item{num_perm}{
Number of permutations to perform
}
  \item{perm_type}{
Type of permutations: either \code{"sample_rand"}, \code{"dataset_rand"}, or \code{"complete_rand"}
}
  \item{write_files}{
Whether to write a file for each permutation / iteration
}
  \item{perm_dir}{
Directory for permutation files
}
  \item{verbose}{
Whether to print verbose output to \code{stdout}
}
  \item{debug}{
Whether to print debug output to \code{stdout}
}
}
\details{
\code{"sample_rand"} shuffles values among fields within a sample / column (dataset distribution is maintained, as is the distribution for each sample / column); \code{"dataset_rand"} shuffles fields within the entire data set (maintaining total counts and the original data set distribution); \code{"complete_rand"} randomly distributes sum of counts about an equally sized matrix (maintaining total counts, but not distribution).
}
\value{
Returns zero on success.
}
\author{
Kevin Keegan
}
\seealso{
\code{\link{MGRAST_plot_pco}}
}
\examples{
#No examples for you!
}
