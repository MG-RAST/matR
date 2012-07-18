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

\title{Retrieve an Abundance Matrix with View Parameters}
\description{...}
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
  \item{ids}{...}
  \item{level}{...}
  \item{source}{...}
  \item{noMeta}{...}
}
\details{
\code{matR} provides four layers of access to \code{MG-RAST}, represented by the functions \code{callRaw}, \code{mGet}, \code{matrixView}, and \code{collection}.
}
\value{The returned item is of class \code{Matrix} (defined in package \code{Matrix}).}
\author{Daniel T. Braithwaite}
\note{These functions are simple wrappers for specific calls to \code{mGet}.
Remember to set an authentication key when attempting access to private data.
}
\seealso{\code{\link{mGet}}
callRaw, mGet, matrixView, Collections}
\examples{
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
}
