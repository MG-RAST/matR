\name{sigtest-class}
\Rdversion{1.1}
\docType{class}
\alias{sigtest-class}
\alias{print,sigtest-method}
\alias{show,sigtest-method}
\alias{summary,sigtest-method}

\title{Class \code{"sigtest"}}
\description{...}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("sigtest", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{list}"}, from data part.
Class \code{"\linkS4class{analysis}"}, directly.
Class \code{"\linkS4class{vector}"}, by class "list", distance 2.
}
\section{Methods}{
  \describe{
    \item{print}{\code{signature(x = "sigtest")}: ... }
    \item{show}{\code{signature(object = "sigtest")}: ... }
    \item{summary}{\code{signature(object = "sigtest")}: ... }
	 }
}
\seealso{\code{\link{sigtest}}}
\examples{
showClass("sigtest")
}
\keyword{classes}
