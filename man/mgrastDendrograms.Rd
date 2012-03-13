\name{mgrastDendrograms}
\alias{Analysis Tools}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Dendogram analysis
}
\description{
     This script will perform dendrogram analysis (x and y) of input data
     using the selected distance/dissimilarity  metric and the selected
     clustering method.
}
\usage{
mgrastDendrograms(file_in, file_out_column = "col_clust", file_out_row = "row_clust", dist_method = "euclidean", clust_method = "ward", produce_figures = FALSE, col_dendrogram_width = 950, col_dendrogram_height = 500, row_dendrogram_width = 950, row_dendrogram_height = 500, output_files_prefix = "my_dendrograms")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file_in}{
%%     ~~Describe \code{file_in} here~~
}
  \item{file_out_column}{
%%     ~~Describe \code{file_out_column} here~~
}
  \item{file_out_row}{
%%     ~~Describe \code{file_out_row} here~~
}
  \item{dist_method}{
%%     ~~Describe \code{dist_method} here~~
}
  \item{clust_method}{
%%     ~~Describe \code{clust_method} here~~
}
  \item{produce_figures}{
%%     ~~Describe \code{produce_figures} here~~
}
  \item{col_dendrogram_width}{
%%     ~~Describe \code{col_dendrogram_width} here~~
}
  \item{col_dendrogram_height}{
%%     ~~Describe \code{col_dendrogram_height} here~~
}
  \item{row_dendrogram_width}{
%%     ~~Describe \code{row_dendrogram_width} here~~
}
  \item{row_dendrogram_height}{
%%     ~~Describe \code{row_dendrogram_height} here~~
}
  \item{output_files_prefix}{
%%     ~~Describe \code{output_files_prefix} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (file_in, file_out_column = "col_clust", file_out_row = "row_clust", 
    dist_method = "euclidean", clust_method = "ward", produce_figures = FALSE, 
    col_dendrogram_width = 950, col_dendrogram_height = 500, 
    row_dendrogram_width = 950, row_dendrogram_height = 500, 
    output_files_prefix = "my_dendrograms") 
{
    suppressPackageStartupMessages(library(ecodist))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
