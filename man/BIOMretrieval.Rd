\name{BIOMretrieval}
\alias{biomRequest}
\alias{biom.environment}

\title{
Get annotation information of samples as BIOM data
}

\description{
Retrieve annotation pipeline information (such as abundance profiles) 
for specified metagenomes and projects into an object of class \code{biom}.
}

\usage{
biomRequest(x, request=c("function", "organism", "feature"), ..., 
  block, wait=TRUE, quiet=FALSE, file, outfile)

\method{biom}{environment}(x, wait=TRUE, ..., quiet=FALSE)
}

\arguments{
  \item{x}{a set of metagenomes and/or projects (\code{character}) or a request ticket (\code{environment})}
  \item{request}{choice of annotation type (string)}
  \item{\dots}{arguments specifying or qualifying the data desired (\code{biomRequest()} only)}
  \item{block}{number of metagenomes per API call (\code{integer})}
  \item{wait}{return only when data is complete? (\code{logical})}
  \item{quiet}{suppress messages and warnings? (\code{logical})}
  \item{file}{file containing a set of metagenomes or projects (string)}
  \item{outfile}{file to save the retrieved data (string)}
}

\details{
Complete technical documentation is forthcoming.  
For the current preliminary release, please refer
to the examples provided.
}

\value{
For \code{biomRequest()}, an \code{environment} if \code{wait=FALSE}.
Otherwise and for \code{biom.environment()}, a \code{biom} object.
}

\author{Daniel T. Braithwaite}

\seealso{
\code{BIOM.utils::\link{biom}},
\code{MGRASTer::\link{call.MGRAST}}
}

\examples{
####  several files demonstrate valid formats for ID input
demoSets()

\dontrun{
ff <- demoSets()

####  simple retrieval of annotation data
yy <- biomRequest (file=ff[1])
head (rows (yy))

####  many arguments can modify what is retrieved
yy <- biomRequest (file=ff[1], group_level="level1")
rownames (yy)

####  taxonomic annotations
yy <- biomRequest (file=ff[4], request="organism", group_level="phylum", source="Greengenes")

####  IDs can be given directly, while output can be to a file
biomRequest ("mgp9", request="function", outfile="mgp9.biom")
biomRequest ("mgm4441619.3 mgm4441620.3 mgm4441656.4", 
  request="function", outfile="mgp9.biom")

####  place an asynchronous request...
yy <- biomRequest ("mgp9", wait=FALSE)
####  ...and receive the data when convenient
yy <- biom (yy)
}

####  full detail for available options
doc.MGRAST (3, head=c("matrix","function","parameters","options"))
doc.MGRAST (3, head=c("matrix","organism","parameters","options"))
doc.MGRAST (3, head=c("matrix","feature","parameters","options"))
}
