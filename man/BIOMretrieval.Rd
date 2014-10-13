\name{BIOMretrieval}
\alias{biomRequest}
\alias{biom.environment}

\title{
Get annotation information of samples as BIOM data
}

\description{
Retrieve annotation pipeline information (such as abundance profiles) 
for specified samples or projects into an object of class "biom".
}

\usage{
biomRequest(x, request=c("function", "organism", "feature"), ..., 
	block, wait=TRUE, quiet=FALSE, file=NULL, outfile=NULL)

\method{biom}{environment}(x, wait=TRUE, ..., quiet=FALSE)
}

\arguments{
  \item{x}{a set of samples or projects; or a request ticket ("character" or "environment")}
  \item{request}{choice of annotation type (string)}
  \item{\dots}{arguments specifying or qualifying the data desired ("biomRequest" only)}
  \item{block}{number of samples per API call ("integer")}
  \item{wait}{return only when data is complete? ("logical")}
  \item{quiet}{suppress messages and warnings? ("logical")}
  \item{file}{file containing a set of samples or projects (string)}
  \item{outfile}{file to save the retrieved data (string)}
}

\details{
Complete technical documentation is forthcoming.  
For the current preliminary release, please refer
to the examples and vignettes provided.
}

\value{
For "biomRequest()", an "environment", if "wait" is "FALSE".
Otherwise, and for "biom.environment()", a "biom" object.
}

\author{Daniel T. Braithwaite}

\seealso{
\code{BIOM.utils::\link{biom}},
\code{MGRASTer::\link{call.MGRAST}}
}

\examples{
#  several files demonstrate valid formats for ID input
demoSets()

\dontrun{
ff <- demoSets()

#  simple retrieval of annotation data
yy <- biomRequest (file=ff[1])
head (rows (yy))

#  many arguments can modify what is retrieved
yy <- biomRequest (file=ff[1], group_level="level1")
rownames (yy)

#  taxonomic annotations
yy <- biomRequest (file=ff[4], request="organism", group_level="phylum", source="Greengenes")

#  IDs can be given directly, while output can be to a file
biomRequest ("mgp9", request="function", outfile="mgp9.biom")
biomRequest ("mgm4441619.3 mgm4441620.3 mgm4441656.4", 
  request="function", outfile="mgp9.biom")

#  place an asynchronous request...
yy <- biomRequest ("mgp9", wait=FALSE)
#  ...and receive the data when convenient
yy <- biom (yy)
}

#  full detail for available options
doc.MGRAST (3, head=c("matrix","function","parameters","options"))
doc.MGRAST (3, head=c("matrix","organism","parameters","options"))
doc.MGRAST (3, head=c("matrix","feature","parameters","options"))
}
