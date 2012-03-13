\name{mgrastPlotPCA}
\alias{mgrastPlotPCA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Visualize Principal Component Analysis
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mgrastPlotPCA(file_in, file_out = "my_pca", num_PCs = 2, produce_fig = FALSE, PC1 = "PC1", PC2 = "PC2", image_out = "my_pca", image_title = image_out, figure_width = 950, figure_height = 950, points_color = "red", figure_res = NA, lab_cex = 1, axis_cex = 1, points_text_cex = 0.8)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file_in}{
%%     ~~Describe \code{file_in} here~~
}
  \item{file_out}{
%%     ~~Describe \code{file_out} here~~
}
  \item{num_PCs}{
%%     ~~Describe \code{num_PCs} here~~
}
  \item{produce_fig}{
%%     ~~Describe \code{produce_fig} here~~
}
  \item{PC1}{
%%     ~~Describe \code{PC1} here~~
}
  \item{PC2}{
%%     ~~Describe \code{PC2} here~~
}
  \item{image_out}{
%%     ~~Describe \code{image_out} here~~
}
  \item{image_title}{
%%     ~~Describe \code{image_title} here~~
}
  \item{figure_width}{
%%     ~~Describe \code{figure_width} here~~
}
  \item{figure_height}{
%%     ~~Describe \code{figure_height} here~~
}
  \item{points_color}{
%%     ~~Describe \code{points_color} here~~
}
  \item{figure_res}{
%%     ~~Describe \code{figure_res} here~~
}
  \item{lab_cex}{
%%     ~~Describe \code{lab_cex} here~~
}
  \item{axis_cex}{
%%     ~~Describe \code{axis_cex} here~~
}
  \item{points_text_cex}{
%%     ~~Describe \code{points_text_cex} here~~
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
function (file_in, file_out = "my_pca", num_PCs = 2, produce_fig = FALSE, 
    PC1 = "PC1", PC2 = "PC2", image_out = "my_pca", image_title = image_out, 
    figure_width = 950, figure_height = 950, points_color = "red", 
    figure_res = NA, lab_cex = 1, axis_cex = 1, points_text_cex = 0.8) 
{
    suppressPackageStartupMessages(library(Cairo))
    suppressPackageStartupMessages(library(pcaMethods))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
