\name{Identifiers}
\alias{Identifiers}
\alias{Resources}
\alias{Resources and Identifiers}
\alias{scrubIds}
\alias{scrapeResources}
\alias{Identifying Resources}
\title{String Processing for Resource Identifiers}
\description{
Formats casually formatted ids properly.  Returns resource types of given ids.
}
\usage{
scrubIds(ids, resources = c ("project", "library", "sample", "metagenome"))
scrapeResources(ids)
}
\arguments{
  \item{ids}{...}
  \item{resources}{...}
}
\details{
The format of resource identifiers is not specified exactly by the API or \code{MG-RAST}, so these functions exist to provide consistency and convenience.  (Typically, high-level functions that expect identifiers apply these functions to their argument internally as necessary.)

\code{scrubIds} accepts ids in various formats and returns a character vector of the same ids properly formatted. \code{resources} should be length one, specifying one resource type for all ids, or equal in length to \code{ids}, specifying the type of each.  \code{scrapeResources} returns the resource types corresponding to ids given in proper format as a character vector.
}
\note{
Ids given as numeric rather than character usually work, but use caution with metagenome ids.  Since they usually include a decimal point, problems can arise because as numerics they are float rather than integer.
}
\value{A character vector.}
\author{Daniel T. Braithwaite}
\seealso{
\url{http://api.metagenomics.anl.gov},
\code{\link{mGet}}, \code{\link{Selections}}, \code{\link{Collections}}}
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
