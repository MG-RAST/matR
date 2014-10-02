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
biomRequest(IDs, request=c("function", "organism"), ..., 
	blocking, wait=TRUE, quiet=FALSE, file=NULL, outfile=NULL)

\method{biom}{environment}(x, wait=TRUE, ..., quiet=FALSE)
}

\arguments{
  \item{IDs}{IDs for the samples or projects of interest ("character")}
  \item{request}{choice of functional or taxonomic annotations (string)}
  \item{\dots}{arguments specifying or qualifying the data desired ("biomRequest" only)}
  \item{blocking}{number of samples per API call ("integer")}
  \item{wait}{return only when data is complete? ("logical")}
  \item{quiet}{suppress messages and warnings? ("logical")}
  \item{file}{file containing sample or project IDs (string)}
  \item{outfile}{file to save the retrieved data (string)}
  \item{x}{an object returned by "biomRequest"}
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
