\name{rlist-class}
\Rdversion{1.1}
\docType{class}
\alias{rlist-class}
\alias{[[,rlist-method}
\alias{$,rlist-method}
\alias{initialize,rlist-method}
\alias{print,rlist-method}
\alias{show,rlist-method}
\alias{summary,rlist-method}

\title{Class \code{"rlist"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("rlist", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"} ~~ }
    \item{\code{names}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{namedList}"}, directly.
Class \code{"\linkS4class{list}"}, by class "namedList", distance 2.
Class \code{"\linkS4class{vector}"}, by class "namedList", distance 3.
}
\section{Methods}{
  \describe{
    \item{[[}{\code{signature(x = "rlist")}: ... }
    \item{$}{\code{signature(x = "rlist")}: ... }
    \item{asFile}{\code{signature(x = "rlist")}: ... }
    \item{initialize}{\code{signature(.Object = "rlist")}: ... }
    \item{print}{\code{signature(x = "rlist")}: ... }
    \item{show}{\code{signature(object = "rlist")}: ... }
    \item{summary}{\code{signature(object = "rlist")}: ... }
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
showClass("rlist")
}
\keyword{classes}
