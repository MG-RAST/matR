\name{mListAllIds}
\alias{mListAllIds}
\title{List All Ids for a Specified Resource Type}
\description{
Convenience function to list all valid identifiers of accessible resources of a specified type.
}
\usage{
mListAllIds(resource = c ("project", "sample", "library", "annotation", "metagenome"))
}
\arguments{
  \item{resource}{...}
}
\details{
Note that an authentication key is required to access private resources.
}
\value{A character vector.}
\author{Daniel T. Braithwaite}
\seealso{\code{\link{Identifiers}},\code{\link{Authentication}},\code{link{mGet}}}
\examples{
mListAllIds("project")
mListAllIds("sample")
mListAllIds("metagenome")
}
