\name{AbundanceRetrieval}
\alias{Abundance Retrieval}
\alias{AbundanceRetrieval}
\alias{orgMatrix}
\alias{orgMatrixEvalue}
\alias{orgMatrixLength}
\alias{orgMatrixPercentID}
\alias{funcMatrix}
\alias{funcMatrixEvalue}
\alias{funcMatrixLength}
\alias{funcMatrixPercentID}

\title{Retrieve Metagenome Taxon and Function Abundances from the Kbase MG-RAST Server}
\description{...}
\usage{
orgMatrix(mgIDs, level = "species", source = "m5nr", noMeta = FALSE)
orgMatrixEvalue(mgIDs, level = "species", source = "m5nr", noMeta = TRUE)
orgMatrixLength(mgIDs, level = "species", source = "m5nr", noMeta = TRUE)
orgMatrixPercentID(mgIDs, level = "species", source = "m5nr", noMeta = TRUE)

funcMatrix(mgIDs, level = "level3", source = "Subsystem", noMeta = FALSE)
funcMatrixEvalue(mgIDs, level = "level3", source = "Subsystem", noMeta = TRUE)
funcMatrixLength(mgIDs, level = "level3", source = "Subsystem", noMeta = TRUE)
funcMatrixPercentID(mgIDs, level = "level3", source = "Subsystem", noMeta = TRUE)
}
\arguments{
  \item{mgIDs}{...}
  \item{level}{...}
  \item{source}{...}
  \item{noMeta}{...}
}
\details{...}
\value{The returned item is an S4 class that can be treated as a \code{sparseMatrix} (and therefore as a \code{matrix}).}
\author{Daniel T. Braithwaite}
\note{These functions are simple wrappers for specific calls to \code{mGet}.}
\seealso{\code{\link{MG-RAST Identifiers}}, \code{\link{mGet}}}
\examples{
aa <- orgMatrix ("4441679.3;4441680.3;4441682.3;4441695.3;4441696.3;4440463.3;4440464.3")
print (aa)
bb <- orgMatrixEvalue ("4441679.3;4441680.3;4441682.3;4441695.3;4441696.3;4440463.3;4440464.3")
print (bb)
cc <- orgMatrixLength ("4441679.3;4441680.3;4441682.3;4441695.3;4441696.3;4440463.3;4440464.3", level = "phylum")
print (cc)
dd <- list (
	m5nr = orgMatrix ("4441679.3;4441680.3;4441682.3;4441695.3;4441696.3;4440463.3;4440464.3", source = "m5nr" ),
	RefSeq = orgMatrix ("4441679.3;4441680.3;4441682.3;4441695.3;4441696.3;4440463.3;4440464.3", source = "RefSeq" ),
	Greengenes = orgMatrix ("4441679.3;4441680.3;4441682.3;4441695.3;4441696.3;4440463.3;4440464.3", source = "Greengenes" ))
}
