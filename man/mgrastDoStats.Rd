\name{mgrastDoStats}
\alias{mgrastDoStats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Does stats
}
\description{
Does stats, really
}
\usage{
mgrastDoStats(data_file, groups_file, data_type = c("raw", "normalized"), sig_test = c("t-test-paired", "Wilcoxon-paired", "t-test-un-paired", "Mann-Whitney_un-paired-Wilcoxon", "ANOVA-one-way", "Kruskal-Wallis"), file_out = "my_stats")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_file}{
%%     ~~Describe \code{data_file} here~~
}
  \item{groups_file}{
%%     ~~Describe \code{groups_file} here~~
}
  \item{data_type}{
%%     ~~Describe \code{data_type} here~~
}
  \item{sig_test}{
%%     ~~Describe \code{sig_test} here~~
}
  \item{file_out}{
%%     ~~Describe \code{file_out} here~~
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
function (data_file, groups_file, data_type = c("raw", "normalized"), 
    sig_test = c("t-test-paired", "Wilcoxon-paired", "t-test-un-paired", 
        "Mann-Whitney_un-paired-Wilcoxon", "ANOVA-one-way", "Kruskal-Wallis"), 
    file_out = "my_stats") 
{
    library(stats)
    library(nlme)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
