\name{Collections}
\alias{Collections}
\alias{collections}
\alias{Collection}
\alias{collection}
\alias{Selections}
\alias{selections}
\alias{Selection}
\alias{selection}
\alias{Views}
\alias{views}
\alias{View}
\alias{view}
\title{Building Metagenome Collections for Analysis with Selections and Views}
\description{The basic component of analysis in \code{matR} is a matrix with columns labeled by metagenome ID, 
but there are various possibilities for the matrix content, and analyses often examine several such matrices 
together.  Such a group of related matrices is called a \code{collection}.
}
\usage{
selection (ids, resources = "metagenome", tagging = "asis")
view (of = "count",
  			annotation = "function",
				level = if (!is.na (pmatch (annotation, "function"))) "level3" else "species",
				source = if (!is.na (pmatch (annotation, "function"))) "Subsystems" else "m5rna")
collection (sel, ...)
}
\arguments{
  \item{ids}{...}
  \item{resources}{...}
  \item{tagging}{...}
  \item{of}{...}
  \item{annotation}{...}
  \item{level}{...}
  \item{source}{...}
  \item{sel}{...}
  \item{...}{...}
}
\details{
\code{matR} provides four layers of access to \code{MG-RAST}, represented by the functions \code{callRaw}, \code{mGet}, \code{matrixView}, and \code{collection}.

  Construction of \code{selection}, \code{view}, and \code{collection} objects is flexible and can be simple or customized.  In principal, a \code{collection} is defined by a \code{selection} and one or more named \code{view}s.  However, various shortcuts are available.  For a quick start, see the examples below.

  
}
\value{
  \code{selection} returns an object of \code{selection} class, \code{view} returns an object of \code{view} class, and \code{collection} returns an object of \code{collection} class.
}
\author{Daniel T. Braithwaite}
\note{Remember to set an authentication key when attempting access to private data.}
\seealso{\code{\link{matR Parameters}}, \code{\link{Authentication}}, \code{Standard Views}, \code{\link{selection-class}}, \code{link{view-class}}, \code{\link{collection-class}}, \code{\link{collection-methods}}
callRaw, mGet, matrixView, Collections}
\examples{

}
