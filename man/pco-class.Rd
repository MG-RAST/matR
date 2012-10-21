\name{pco-class}
\Rdversion{1.1}
\docType{class}
\alias{pco-class}
\alias{print,pco-method}
\alias{show,pco-method}
\alias{summary,pco-method}

\title{Class \code{"pco"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("mpco", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{values}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{vectors}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{par}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{print}{\code{signature(x = "pco")}: ... }
    \item{show}{\code{signature(object = "pco")}: ... }
    \item{summary}{\code{signature(object = "pco")}: ... }
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
showClass("pco")
}
\keyword{classes}
