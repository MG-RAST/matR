\name{BIOM Import and Export}
\alias{fromBIOM}
\alias{toBIOM}
\alias{BIOM}
\alias{BIOM Import and Export}
\alias{Import and Export}
\title{
Import and Export BIOM Objects
}
\description{
Easy import and export of objects in the JSON-based Biological Observation Matrix (BIOM) format.  Imports lightly encapsulate the BIOM object's numerical data and metadata. Exports write a valid BIOM object.
}
\usage{
fromBIOM(file, keepSparse = TRUE)
toBIOM(obj, type, ID = NULL, comment = "", asSparse = TRUE, toFile = NULL)
}
\arguments{
  \item{obj}{
 \code{matrix}, \code{sparseMatrix}, or \code{data.frame}
}
  \item{type}{
Character: one of \code{"OTU table"}, \code{"Pathway table"}, \code{"Function table"},\code{"Ortholog table"},\code{"Gene table"},\code{"Metabolite table"},\code{"Taxon table"}
}
  \item{ID}{
Optional, character
}
  \item{comment}{
Optional, character
}
  \item{asSparse}{
Logical: whether to create a \code{\"sparse\"} or \code{\"dense\"} \code{BIOM} object
}
  \item{toFile}{
Destination file
}
  \item{file}{
Character:  JSON content, name of source file, or URL
}
  \item{keepSparse}{
Logical: if \code{TRUE} and the \code{BIOM} object is sparse, a \code{sparseMatrix} will be created, otherwise an ordinary \code{matrix}
}
}
\details{
The object created by \code{fromBIOM} is of class \code{kb}, which encapsulates metadata but can be used as a \code{matrix} (or \code{sparseMatrix}, as the case may be).  
\code{toBIOM} writes a correct BIOM object and returns the filename, or if \code{toFile} is \code{NULL}, returns the object itself as text.
}
\value{
%%  ~Describe the value returned
}
\references{
\url{http://www.biom-format.org/}
}
\author{
Daniel Braithwaite
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{kbGet}}
}
\examples{
}
\keyword{ BIOM }
\keyword{ QIIME }
\keyword{ biom }
\keyword{ qiime }
