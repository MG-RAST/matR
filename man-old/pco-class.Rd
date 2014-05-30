\name{pco-class}
\Rdversion{1.1}
\docType{class}
\alias{pco-class}
\alias{print,pco-method}
\alias{render,pco-method}
\alias{show,pco-method}
\alias{summary,pco-method}

\title{class \code{"pco"}}
\description{Container for results of a principal coordinates analysis}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"}}
  }
}
\section{Extends}{
Class \code{"\linkS4class{list}"}, from data part.\cr
Class \code{"\linkS4class{analysis}"}, directly.\cr
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
\keyword{classes}
