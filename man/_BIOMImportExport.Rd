\name{BIOMImportExport}
\alias{BIOM Import and Export}
\alias{fromBIOM}
\alias{toBIOM}
\alias{RBIOM}
\title{Import and Export BIOM Objects}
\description{
Functions and class and method definitions to implement import and export of JSON-based Biological Observation Matrix (BIOM) objects.
Importing lightly encapsulates the BIOM object's numerical data and metadata into the S4 RBIOM class.
Exporting an RBIOM object creates a valid JSON BIOM object.
}
\usage{
fromBIOM (x, keepSparse = TRUE)
toBIOM (x, type, ID = NULL, comment = "", asSparse = TRUE, toFile = NULL)
}
\arguments{
	\item{x}{...}
	\item{type}{...}
	\item{ID}{...}
	\item{comment}{...}
	\item{asSparse}{...}
	\item{toFile}{...}
	\item{keepSparse}{...}
}
\details{
The object created by \code{fromBIOM} is of class \code{RBIOM}, which encapsulates metadata and
can be used as a \code{matrix} or \code{sparseMatrix}, as the case may be.  
\code{toBIOM} builds a correct BIOM object and returns the filename, or if \code{toFile} is \code{NULL}, 
returns the object itself (as text).
}
\value{
The \code{RBIOM} or \code{JSON} object created.
}
\references{
\url{http://www.biom-format.org/}\cr
\url{http://www.json.org}
}
\author{Daniel Braithwaite}
\seealso{\code{\link{mGet}}}
\examples{
## ...
}
