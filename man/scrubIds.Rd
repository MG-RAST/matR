\name{Identifiers}
\alias{Identifiers}
\alias{Resources}
\alias{scrubIds}
\alias{scrapeRes}
\alias{Identifying Resources}
\title{Process Strings that Identify Resources}
\description{

}
\usage{
scrubIds(ids, res = "metagenome")
scrapeRes(ids)
}
\arguments{
  \item{ids}{...}
  \item{res}{...}
}
\details{
Most functions that expect identifiers use these
but the user may occasionally find them useful
scrubIds "properly formats" id specs from out of loose format into a char vector
note: scrapeRes expects "properly formatted" id vector
}
\warning{mgm ids as numbers are problematic because of ".3"}
\value{A character vector.}
\author{Daniel T. Braithwaite}
\seealso{mGet, Collections}
\examples{
x <- c ("4445567.3", "4445568.3")
y <- "4445569.3;4445570.3"
z <- "4445571.3 mgp101 4445572.3"
A <- scrubIds ( c (x,y,z))
B <- scrubIds ("mgp101 mgp102 mgp103 mgp104 mgp105")
C <- scrubIds ("101;102;103", res = "project")
D <- scrubIds (c (22817, 22818, 22819), res = "samp")

scrapeRes (A)
scrapeRes (B)
scrapeRes (C)
scrapeRes (D)
l <- "mgl54248 mgl54250 mgl54252 mgl54254"
scrapeRes (l)
scrapeRes (scrubIds (l))
}
