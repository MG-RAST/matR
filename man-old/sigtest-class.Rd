\name{sigtest-class}
\Rdversion{1.1}
\docType{class}
\alias{sigtest-class}
\alias{print,sigtest-method}
\alias{show,sigtest-method}
\alias{summary,sigtest-method}

\title{class \code{"sigtest"}}
\description{Container for results of annotation significance testing}
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
    \item{print}{\code{signature(x = "sigtest")}: ... }
    \item{show}{\code{signature(object = "sigtest")}: ... }
    \item{summary}{\code{signature(object = "sigtest")}: ... }
	 }
}
\seealso{\code{\link{sigtest}}}
\keyword{classes}
