\name{selection-class}
\Rdversion{1.1}
\docType{class}
\alias{selection-class}
\alias{initialize,selection-method}
\alias{print,selection-method}
\alias{show,selection-method}
\alias{summary,selection-method}

\title{Class \code{"selection"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("selection", ids, resources, tagging)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{ids}:}{Object of class \code{"character"} ~~ }
    \item{\code{resources}:}{Object of class \code{"character"} ~~ }
    \item{\code{sel}:}{Object of class \code{"character"} ~~ }
    \item{\code{tagging}:}{Object of class \code{"character"} ~~ }
    \item{\code{metadata}:}{Object of class \code{"rlist"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{collection}{\code{signature(sel = "selection")}: ... }
    \item{initialize}{\code{signature(.Object = "selection")}: ... }
    \item{metadata}{\code{signature(x = "selection")}: ... }
    \item{print}{\code{signature(x = "selection")}: ... }
    \item{show}{\code{signature(object = "selection")}: ... }
    \item{summary}{\code{signature(object = "selection")}: ... }
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
showClass("selection")
}
\keyword{classes}
