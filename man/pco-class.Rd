\name{pco-class}
\Rdversion{1.1}
\docType{class}
\alias{pco-class}
\alias{print,pco-method}
\alias{render,pco-method}
\alias{show,pco-method}
\alias{summary,pco-method}

\title{Class \code{"pco"}}
\description{...}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("pco", ...)}.
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
    \item{print}{\code{signature(x = "pco")}: ... }
    \item{render}{\code{signature(x = "pco")}: ... }
    \item{show}{\code{signature(object = "pco")}: ... }
    \item{summary}{\code{signature(object = "pco")}: ... }
	 }
}
\seealso{\code{\link{pco}}}
\examples{
showClass("pco")
}
\keyword{classes}
