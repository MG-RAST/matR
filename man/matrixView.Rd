\name{matrixView}
\alias{matrixView}
\title{
Retrieve the Matrix Indicated by a View
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
matrixView(ids, v)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ids}{
%%     ~~Describe \code{ids} here~~
}
  \item{v}{
%%     ~~Describe \code{v} here~~
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
\link{Collections}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (ids, v) 
{
    s <- paste("format/plain", "/result_column/", switch(v@of, 
        count = "abundance", normed = "abundance", evalue = "evalue", 
        length = "length", percentid = "identity"), "/type/", 
        v@annotation, "/group_level/", v@level, "/source/", v@source, 
        sep = "")
    x <- mGet("abundance", scrubIds(ids), param = s, enClass = FALSE)
    if (v@of == "normed") 
        Matrix::Matrix(normalize(x))
    else x
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
