\name{mListAllIds}
\alias{mListAllIds}

\title{List All IDs for a Specified Resource Type}
\description{Lists all valid identifiers of accessible resources of a specified type}
\usage{mListAllIds(resource = c ("project", "sample", "library", "annotation", "metagenome"))}
\arguments{
  \item{resource}{kind of resource (character)}
}
\details{
Note that an authentication is required to access private resources.
}
\value{A character vector.}
\author{Daniel T. Braithwaite}
\seealso{\code{\link{Identifiers}},\code{\link{Authentication}},\code{\link{mGet}}}
\examples{
mListAllIds("project")
mListAllIds("sample")
mListAllIds("metagenome")
}
