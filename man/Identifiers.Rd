\name{Identifiers}
\alias{Identifiers}
\alias{scrubIds}
\alias{scrapeResources}

\title{Process Resource Identifiers}
\description{Formats IDs properly.  Returns resource types of given IDs.}
\usage{
scrubIds(ids, resources = c ("project", "library", "sample", "metagenome"))
scrapeResources(ids)
}
\arguments{
  \item{ids}{string(s) specifying resource ID(s) (character)}
  \item{resources}{string(s) specifying resource type(s) (character)}
}
\details{
These functions provide consistency in handling IDs to identify metagenomes and related resources.

\code{scrubIds} accepts IDs in various formats and returns a character vector of the same IDs properly formatted. \code{resources} should be length one, specifying one resource type for all IDs, or equal in length to \code{ids}, specifying the type of each.  \code{scrapeResources} returns the resource types corresponding to IDs given in proper format as a character vector.
}
\note{
IDs given as numeric rather than character usually work, but use caution with metagenome IDs.  Since they usually include a decimal point, problems can arise because as numerics they are float rather than integer.
}
\value{A character vector.}
\author{Daniel T. Braithwaite}
\seealso{
\url{http://api.metagenomics.anl.gov},
\code{\link{mGet}}, \code{\link{Collections}}}
\examples{
x <- c ("4445567.3", "4445568.3")
y <- "4445569.3;4445570.3"
z <- "4445571.3 mgp101 4445572.3"
A <- scrubIds ( c (x,y,z))
B <- scrubIds ("mgp101 mgp102 mgp103 mgp104 mgp105")
C <- scrubIds ("101;102;103", res = "project")
D <- scrubIds (c (22817, 22818, 22819), res = "samp")

scrapeResources (A)
scrapeResources (B)
scrapeResources (C)
scrapeResources (D)
l <- "mgl54248 mgl54250 mgl54252 mgl54254"
scrapeResources (l)
scrapeResources (scrubIds (l))
}
