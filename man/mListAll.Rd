\name{mListAll}
\alias{mListAll}
\title{
List All IDs for a Specified Resource
}
\description{
Convenience function to list all valid IDs for a given resource
}
\usage{
mListAll(resource)
}
\arguments{
  \item{resource}{...}
}
\details{
}
\value{
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
function (resource) 
{
    reqPack("RJSONIO")
    if (!oneofmust(resource, "project", "sample", "library", 
        "annotation", "metagenome")) 
        return()
    x <- fromJSON(.mCallRaw(resource), simplify = TRUE, asText = TRUE)
    x
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
