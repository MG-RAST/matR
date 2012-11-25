\name{Wrappers}
\alias{Wrappers}
\alias{matrixView}
\alias{orgMatrix}
\alias{orgMatrixEvalue}
\alias{orgMatrixLength}
\alias{orgMatrixPercentID}
\alias{funcMatrix}
\alias{funcMatrixEvalue}
\alias{funcMatrixLength}
\alias{funcMatrixPercentID}

\title{Retrieve a Specified Metagenome Matrix}
\description{.....Construct an Analysis View of Matrix (Annotations by IDs) ...with a wrapper-style interface}
\usage{
matrixView(ids, v)

orgMatrix(ids, level = "species", source = "m5rna", noMeta = FALSE)
orgMatrixEvalue(ids, level = "species", source = "m5rna", noMeta = TRUE)
orgMatrixLength(ids, level = "species", source = "m5rna", noMeta = TRUE)
orgMatrixPercentID(ids, level = "species", source = "m5rna", noMeta = TRUE)

funcMatrix(ids, level = "level3", source = "Subsystems", noMeta = FALSE)
funcMatrixEvalue(ids, level = "level3", source = "Subsystems", noMeta = TRUE)
funcMatrixLength(ids, level = "level3", source = "Subsystems", noMeta = TRUE)
funcMatrixPercentID(ids, level = "level3", source = "Subsystems", noMeta = TRUE)
}
\arguments{
  \item{v}{an object of class view}
  \item{ids}{...}
  \item{level}{...}
  \item{source}{...}
  \item{noMeta}{...}
}
\details{
Currently \code{matR} provides four layers of access to \code{MG-RAST}, represented by the functions \code{callRaw}, \code{mGet}, \code{matrixView}, and \code{collection}.

These functions are simple wrappers for appropriate calls to \code{mGet}, intended for convenience.  \code{matrixView} uses the \code{view} class to provide a higher level of generality than the others, which are named to indicate their respective function.

}
\value{An object of class \code{Matrix} (from the \code{Matrix} package).}
\author{Daniel T. Braithwaite}
\note{
For some applications, it may be necessary to convert the returned \code{Matrix} explicitly with \code{as.matrix}.  See the examples.
Remember to set an authentication key when attempting access to private data.
}
\seealso{callRaw, mGet, matrixView, Collections, Views}
\examples{
\dontrun{
aa <- orgMatrix ("4441679.3;4441680.3;4441682.3;4441695.3;4441696.3;4440463.3;4440464.3")
print (aa)
bb <- orgMatrixEvalue ("4441679.3;4441680.3;4441682.3;4441695.3;4441696.3;4440463.3;4440464.3")
print (bb)
cc <- orgMatrixLength ("4441679.3;4441680.3;4441682.3;4441695.3;4441696.3;4440463.3;4440464.3", level = "phylum")
print (cc)
dd <- list (
	m5rna = orgMatrix ("4441679.3;4441680.3;4441682.3;4441695.3;4441696.3;4440463.3;4440464.3", source = "m5rna" ),
	RefSeq = orgMatrix ("4441679.3;4441680.3;4441682.3;4441695.3;4441696.3;4440463.3;4440464.3", source = "RefSeq" ),
	Greengenes = orgMatrix ("4441679.3;4441680.3;4441682.3;4441695.3;4441696.3;4440463.3;4440464.3", source = "Greengenes" ))
as.matrix (matrixView ("4441679.3 4441680.3", view (of = "drisee")))
}
}
