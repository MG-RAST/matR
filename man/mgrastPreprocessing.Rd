\name{mgrastPreprocessing}
\alias{mgrastPreprocessing}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Does preprocessing
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mgrastPreprocessing(file_in, file_out = "preprocessed_data", produce_fig = FALSE, image_out = "my_boxplots", raw_data_boxplot_title = "raw data", centered_data_boxplot_title = "log2(x+1) & centered per sample, scaled 0 to 1 over all samples", figure_width = 950, figure_height = 1000, figure_res = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file_in}{
%%     ~~Describe \code{file_in} here~~
}
  \item{file_out}{
%%     ~~Describe \code{file_out} here~~
}
  \item{produce_fig}{
%%     ~~Describe \code{produce_fig} here~~
}
  \item{image_out}{
%%     ~~Describe \code{image_out} here~~
}
  \item{raw_data_boxplot_title}{
%%     ~~Describe \code{raw_data_boxplot_title} here~~
}
  \item{centered_data_boxplot_title}{
%%     ~~Describe \code{centered_data_boxplot_title} here~~
}
  \item{figure_width}{
%%     ~~Describe \code{figure_width} here~~
}
  \item{figure_height}{
%%     ~~Describe \code{figure_height} here~~
}
  \item{figure_res}{
%%     ~~Describe \code{figure_res} here~~
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
function (file_in, file_out = "preprocessed_data", produce_fig = FALSE, 
    image_out = "my_boxplots", raw_data_boxplot_title = "raw data", 
    centered_data_boxplot_title = "log2(x+1) & centered per sample, scaled 0 to 1 over all samples", 
    figure_width = 950, figure_height = 1000, figure_res = NA) 
{
    suppressPackageStartupMessages(library(Cairo))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
