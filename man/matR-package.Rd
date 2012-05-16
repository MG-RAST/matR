\name{matR-package}
\alias{matR-package}
\alias{matR}
\alias{Kbase}
\alias{kbase}
\alias{mgrast}
\alias{mg-rast}
\alias{MGRAST}
\alias{MG-RAST}
\docType{package}
\title{
Metagenomics Analysis Tools via Kbase MG-RAST Server
}
\description{
This package provides facilities to query and analyze data from the Kbase MG-RAST metagenome analysis pipeline.  
Access to private data is authenticated.
Data is encapsulated with metadata in a lightweight object.
Import and export from BIOM format is enabled.
A suite of statistical tools is provided.
The lightweight \code{RBIOM} class is defined as a close translation into R of the BIOM standard JSON format.  
It inherits from both \code{Matrix} and \code{sparseMatrix}, constructs from a JSON object or provided fields, 
and writes to JSON as well as printing prettily.
Graphics are implemented with a Cairo device, and can be created interactively or saved in various formats.
The three package components --- download access to MG-RAST, statistical analysis tools, and visualization tools --- are modular.
We import freely from other contributed R packages, but most such dependencies are optional.
}
\details{
\tabular{ll}{
Package: \tab matR\cr
Type: \tab Package\cr
Version: \tab 0.9\cr
Date: \tab 2012-02-27\cr
Depends: \tab R (>= 2.14.2), methods\cr
License: \tab Unlimited\cr
LazyLoad: \tab yes\cr
}
}
\author{
Daniel Braithwaite, Kevin Keegan}
\references{
\url{http://metagenomics.anl.gov}\cr
\url{http://www.biom-format.org}\cr
\url{http://www.cairographics.org}\cr
}
\seealso{
\code{\link{Metadata Retrieval}}\cr
\code{\link{Abundance Retrieval}}\cr
\code{\link{MG-RAST Identifiers}}\cr
\code{\link{Kbase Authentication}}\cr
\code{\link{Querying MG-RAST}}\cr
\code{\link{BIOM Import and Export}}\cr
%\code{\link{Analysis Tools}}\cr
\code{\link{Direct API Access}}\cr
\code{\link{Example Data}}
}
\keyword{ package }
