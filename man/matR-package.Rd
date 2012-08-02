\name{matR-package}
\alias{matR-package}
\alias{matR}
\alias{mgrast}
\alias{mg-rast}
\alias{MGRAST}
\alias{MG-RAST}
\docType{package}
\title{
Metagenomics Analysis Tools for R
}
\description{
%This package provides facilities to query and analyze data from the MG-RAST pipeline.  
%Access to private data is authenticated.
%Data is encapsulated with metadata in a lightweight object.
%Import and export from BIOM format is enabled.
%A suite of statistical tools is provided.
%The lightweight \code{RBIOM} class is defined as a close translation into R of the BIOM standard JSON format.  
%It inherits from both \code{Matrix} and \code{sparseMatrix}, constructs from a JSON object or provided fields, 
%and writes to JSON as well as printing prettily.
%Graphics are implemented with a Cairo device, and can be created interactively or saved in various formats.
%The three package components --- download access to MG-RAST, statistical analysis tools, and visualization tools --- are modular.
%We import freely from other contributed R packages but individual dependencies are optional.
}
\details{
\tabular{ll}{
Package: \tab matR\cr
Type: \tab Package\cr
Version: \tab 0.9.7\cr
Date: \tab 2012-02-27\cr
Depends: \tab R (>= 2.14.2), methods\cr
License: \tab file LICENSE\cr
LazyLoad: \tab yes\cr
}
}
\author{
Daniel T. Braithwaite, Kevin P. Keegan}
\references{
\url{http://metagenomics.anl.gov}\cr
}
\seealso{
The short list:
\code{\link{Authentication}}\cr
\code{\link{Collections}}\cr
\code{\link{Demos}}\cr
\cr
Plus...\cr
\code{\link{Analysis, Heatmap Dendrogram}}\cr
\code{\link{Analysis, Principal Coordinates}}\cr
\code{\link{Analysis, Q value}}\cr
\code{\link{Analysis, Normalization}}\cr
\code{\link{Analysis, Boxplots}}\cr
\code{\link{Configuration}}\cr                % mconfig
\code{\link{Dependencies}}\cr
\code{\link{Example Data}}\cr
\code{\link{Resources and Identifiers}}\cr
\code{\link{Import and Export}}\cr            % asFile (plus imp, exp)
\code{\link{Metadata}}\cr
\code{\link{Visualization}}\cr                % render (plus par)
\code{\link{Other Packages}}\cr
%\code{\link{Querying MG-RAST}}\cr
}
\keyword{ package }
