\name{collection-class}
\Rdversion{1.1}
\docType{class}
\alias{collection-class}
\alias{$,collection-method}
\alias{$<-,collection-method}
\alias{[,collection,ANY,ANY,ANY-method}
\alias{[[,collection-method}
\alias{[[<-,collection,ANY,missing,character-method}
\alias{asFile,collection-method}
\alias{dist,collection-method}
\alias{groups,collection-method}
\alias{groups<-,collection-method}
\alias{heatmap,collection-method}
\alias{metadata,collection-method}
\alias{names,collection-method}
\alias{names<-,collection-method}
\alias{print,collection-method}
\alias{render,collection-method}
\alias{rownames,collection-method}
\alias{samples,collection-method}
\alias{show,collection-method}
\alias{summary,collection-method}
\alias{viewnames,collection-method}
\alias{viewnames<-,collection-method}
\alias{views,collection-method}

\title{Class \code{"collection"}}
\description{...}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("collection", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{views}:}{Object of class \code{"list"} ~~ }
    \item{\code{sel}:}{Object of class \code{"selection"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{$}{\code{signature(x = "collection")}: ... }
    \item{$<-}{\code{signature(x = "collection")}: ... }
    \item{[}{\code{signature(x = "collection", i = "ANY", j = "ANY", drop = "ANY")}: ... }
    \item{[[}{\code{signature(x = "collection")}: ... }
    \item{[[<-}{\code{signature(x = "collection", i = "ANY", j = "missing", value = "character")}: ... }
    \item{asFile}{\code{signature(x = "collection")}: ... }
    \item{dist}{\code{signature(x = "collection")}: ... }
    \item{groups}{\code{signature(x = "collection")}: ... }
    \item{groups<-}{\code{signature(x = "collection")}: ... }
    \item{heatmap}{\code{signature(x = "collection")}: ... }
    \item{metadata}{\code{signature(x = "collection")}: ... }
    \item{names}{\code{signature(x = "collection")}: ... }
    \item{names<-}{\code{signature(x = "collection")}: ... }
    \item{pco}{\code{signature(x = "collection")}: ... }
    \item{print}{\code{signature(x = "collection")}: ... }
    \item{render}{\code{signature(x = "collection")}: ... }
    \item{rownames}{\code{signature(x = "collection")}: ... }
    \item{samples}{\code{signature(x = "collection")}: ... }
    \item{selection}{\code{signature(x = "collection")}: ... }
    \item{show}{\code{signature(object = "collection")}: ... }
    \item{sigtest}{\code{signature(x = "collection")}: ... }
    \item{summary}{\code{signature(object = "collection")}: ... }
    \item{viewnames}{\code{signature(x = "collection")}: ... }
    \item{viewnames<-}{\code{signature(x = "collection")}: ... }
    \item{views}{\code{signature(x = "collection")}: ... }
	 }
}
\seealso{\code{\link{Collections}}}
\examples{
showClass("collection")
}
\keyword{classes}
