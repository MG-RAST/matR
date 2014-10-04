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
}
