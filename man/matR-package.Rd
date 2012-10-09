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
\author{Daniel T. Braithwaite, Kevin P. Keegan}
\references{
\url{http://metagenomics.anl.gov}\cr
\url{http://www.kbase.us}\cr
}
\seealso{
The short list:\cr
\code{\link{Authentication}}\cr
\code{\link{Collections}}\cr
\code{\link{Demos}}\cr
\code{\link{Dependencies}}\cr

And:\cr
\code{\link{Analyses}}\cr                     % analysis superclass - heatmap, pcoa, normalization, qvalue
\code{\link{Configuration}}\cr                % mconfig
\code{\link{Example Data}}\cr
\code{\link{Resources and Identifiers}}\cr    % Identifiers
\code{\link{Import and Export}}\cr            % asFile (plus imp, exp)
\code{\link{Metadata}}\cr
\code{\link{Recommended Packages}}\cr
\code{\link{Visualizations}}\cr               % render (plus par)
%\code{\link{Querying MG-RAST}}\cr
}
\keyword{ package }
