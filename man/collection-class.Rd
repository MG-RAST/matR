\name{collection-class}
\Rdversion{1.1}
\docType{class}
\alias{collection-class}
\alias{[[,collection-method}
\alias{$,collection-method}
\alias{initialize,collection-method}
\alias{print,collection-method}
\alias{show,collection-method}
\alias{summary,collection-method}

\title{Class \code{"collection"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("collection", sel, data, views, ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{sel}:}{Object of class \code{"selection"} ~~ }
    \item{\code{data}:}{Object of class \code{"list"} ~~ }
    \item{\code{views}:}{Object of class \code{"list"} ~~ }
    \item{\code{view}:}{Object of class \code{"function"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{[[}{\code{signature(x = "collection")}: ... }
    \item{$}{\code{signature(x = "collection")}: ... }
    \item{initialize}{\code{signature(.Object = "collection")}: ... }
    \item{metadata}{\code{signature(x = "collection")}: ... }
    \item{print}{\code{signature(x = "collection")}: ... }
    \item{render}{\code{signature(x = "collection")}: ... }
    \item{show}{\code{signature(object = "collection")}: ... }
    \item{summary}{\code{signature(object = "collection")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("collection")
}
\keyword{classes}
