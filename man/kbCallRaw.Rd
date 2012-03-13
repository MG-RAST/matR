\name{.kbCallRaw}
\alias{Direct API Calls}
\alias{.kbCallRaw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calling the MG-RAST API Directly
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
.kbCallRaw(call, toFile = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{call}{
%%     ~~Describe \code{call} here~~
}
  \item{toFile}{
%%     ~~Describe \code{toFile} here~~
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
function (call, toFile = NULL) 
{
    if (0 == length(grep("?", call, fixed = TRUE))) 
        conj = "?"
    else conj = "&"
    urlStr <- paste(.kbGetServer(), call, conj, "auth=", .kbGetAuth(), 
        sep = "")
    message("KbaseKit: .kbCallRaw: ", urlStr)
    if (is.null(toFile)) 
        readLines(urlStr, warn = FALSE)
    else download.file(urlStr, toFile, quiet = TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
