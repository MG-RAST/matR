\name{selection-class}
\Rdversion{1.1}
\docType{class}
\alias{selection-class}
\alias{[,selection,ANY,ANY,ANY-method}
\alias{groups,selection-method}
\alias{groups<-,selection-method}
\alias{metadata,selection-method}
\alias{names,selection-method}
\alias{names<-,selection-method}
\alias{print,selection-method}
\alias{samples,selection-method}
\alias{selection,selection-method}
\alias{show,selection-method}
\alias{summary,selection-method}

\title{Class \code{"selection"}}
\description{...}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("selection", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{ids}:}{Object of class \code{"character"} ~~ }
    \item{\code{groups}:}{Object of class \code{"factor"} ~~ }
    \item{\code{metadata}:}{Object of class \code{"metadata"} ~~ }
    \item{\code{ids.spec}:}{Object of class \code{"character"} ~~ }
    \item{\code{resource.spec}:}{Object of class \code{"character"} ~~ }
    \item{\code{metadata.extent}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "selection", i = "ANY", j = "ANY", drop = "ANY")}: ... }
    \item{collection}{\code{signature(x = "selection")}: ... }
    \item{groups}{\code{signature(x = "selection")}: ... }
    \item{groups<-}{\code{signature(x = "selection")}: ... }
    \item{metadata}{\code{signature(x = "selection")}: ... }
    \item{names}{\code{signature(x = "selection")}: ... }
    \item{names<-}{\code{signature(x = "selection")}: ... }
    \item{print}{\code{signature(x = "selection")}: ... }
    \item{samples}{\code{signature(x = "selection")}: ... }
    \item{selection}{\code{signature(x = "selection")}: ... }
    \item{show}{\code{signature(object = "selection")}: ... }
    \item{summary}{\code{signature(object = "selection")}: ... }
	 }
}
\seealso{\code{\link{selection-methods}}}
\examples{
showClass("selection")
}
\keyword{classes}
