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
together.  Such a group of related matrices is called a \code{collection}.}
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
  \item{...}{named views}
}
\details{
Currently \code{matR} provides four layers of access to \code{MG-RAST}, represented by the functions \code{callRaw}, \code{mGet}, \code{matrixView}, and \code{collection}.  

The \code{collection} function offers high-level access, structured to facilitate analysis.  Construction of \code{collection} and related objects is flexible and can be simple or customized.  In principle, a \code{collection} is defined by a \code{selection} and one or more named \code{view}s.  However, various shortcuts are available.  Details follow, but for a quick start, see the examples below.

A \code{selection} designates a specific set of metagenomes and holds associated metadata.  It holds no annotation or analysis information.  The elements of a \code{selection} are specified by project, sample, library, and/or metagenome ID.  Thus, for instance, it is possible to make a \code{selection} consisting of all the metagenomes of one project, plus a single metagenome from a different project, with only two IDs.  Since metadata for related metagenomes can be very repetitive, the extent of metadata held by a selection is adjustable with the \code{tagging} parameter.  Experimentation is the best way to see the results of different values.  Note that to study metadata of a set of metagenomes, it is enough to construct a \code{selection} object.

A \code{view} expresses the kind of content held by a metagenome matrix, but it is an abstraction not associated to any particular matrix, nor any particular metagenome.  The parameters to the 

\code{selection}s and \code{view}s are useful primarily to construct \code{collection}s, but like any object they can be saved and copied, which may prove useful.

A \code{colletion} holds a selection object, one or more named \code{view}s, and metagenome matrices in those views.
A collection is rarely built to hold a single view, but rather a list of multiple views on the same selection.
  
}
\value{
  \code{selection} returns an object of \code{selection} class, \code{view} returns an object of \code{view} class, and \code{collection} returns an object of \code{collection} class.
}
\author{Daniel T. Braithwaite}
\note{Remember to set an authentication key when attempting access to private data.}
\seealso{\code{\link{matR Parameters}}, \code{\link{Authentication}}, \code{Standard Views}, \code{\link{selection-class}}, \code{\link{view-class}}, \code{\link{collection-class}}, \code{\link{collection-methods}}
\code{\link{callRaw}}, \code{\link{mGet}}, \code{\link{matrixView}}}
\examples{
\dontrun{
cc <- collection("4443360.3;4443361.3;4443362.3;4443363.3;4443364.3;4443365.3;4443366.3;4443367.3;4443368.3")

metadata (selection (...))

selection ("mgp101 mgp102")

v <- view ()
sel <- selection ()
dd <- collection (sel, v)

ee <- collection (selection (), n = view (), m = view ())
metadata (ee)
ee$n
ee$m

ww <- list (func = view (), org = view(), onlygg= view())
ff <- collection ("...", ww)
ff$onlygg
}
}
