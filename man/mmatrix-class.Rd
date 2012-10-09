\name{mmatrix-class}
\Rdversion{1.1}
\docType{class}
\alias{mmatrix-class}
\alias{initialize,mmatrix-method}
\alias{print,mmatrix-method}
\alias{show,mmatrix-method}
\alias{summary,mmatrix-method}

\title{Class \code{"mmatrix"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("mmatrix", data, metadata, hierarchy)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"Matrix"} ~~ }
    \item{\code{metadata}:}{Object of class \code{"rlist"} ~~ }
    \item{\code{hierarchy}:}{Object of class \code{"character"} ~~ }
%    \item{\code{Dim}:}{Object of class \code{"integer", from class "Matrix"} ~~ }
%    \item{\code{Dimnames}:}{Object of class \code{"list", from class "Matrix"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{Matrix}"}, directly, with explicit coerce.
Class \code{"\linkS4class{matrix}"}, directly, with explicit coerce.
Class \code{"\linkS4class{array}"}, by class "matrix", distance 2, with explicit coerce.
Class \code{"\linkS4class{structure}"}, by class "matrix", distance 3, with explicit coerce.
Class \code{"\linkS4class{vector}"}, by class "matrix", distance 4, with explicit coerce.
}
\section{Methods}{
  \describe{
    \item{asFile}{\code{signature(x = "mmatrix")}: ... }
    \item{initialize}{\code{signature(.Object = "mmatrix")}: ... }
    \item{print}{\code{signature(x = "mmatrix")}: ... }
    \item{show}{\code{signature(object = "mmatrix")}: ... }
    \item{summary}{\code{signature(object = "mmatrix")}: ... }
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
showClass("mmatrix")
}
\keyword{classes}
