\name{kbListAll}
\alias{kbListAll}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
List MG-RAST ID's of All Resources of One Type}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
kbListAll(resource)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resource}{
One of \code{"project"}, \code{"sample"}, \code{"library"}, \code{"annotation"}, \code{"metagenome"}
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
\author{Daniel Braithwaite}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{kbGet}}, \code{\link{kbSearchMetagenomes}}
}
\examples{
kbListAll("project")
}
