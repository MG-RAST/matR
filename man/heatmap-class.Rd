\name{heatmap-class}
\Rdversion{1.1}
\docType{class}
\alias{heatmap-class}
\alias{render,heatmap-method}

\title{Class \code{"heatmap"}}
\description{...}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("heatmap", ...)}.
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
    \item{render}{\code{signature(x = "heatmap")}: ... }
	 }
}
\seealso{\code{\link{heatmap}}}
\examples{
showClass("heatmap")
}
\keyword{classes}
