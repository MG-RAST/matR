\name{metadata.character}
\alias{metadata}
\alias{metadata.character}

\title{
Get metadata of projects or metagenomes
}

\description{
Get metadata of specified projects or metagenomes, 
or (simply) look up corresponding IDs in an index of projects and metagenomes.
}

\usage{
\method{metadata}{character}(x, detail=NULL, ..., quiet=FALSE, file=NULL)
}

\arguments{
  \item{x}{IDs of projects or metagenomes ("character")}
  \item{detail}{level of metadata detail ("NULL", "TRUE", or "character")}
  \item{\dots}{arguments to "call.MGRAST()"}
  \item{quiet}{suppress messages and warnings? ("logical")}
  \item{file}{file containing project or metagenome IDs (string)}
}

\note{
Although associated to the same generic, "BIOM.utils::metadata.biom()" 
is conceptually distinct from the function documented here.
}

\details{
Complete technical documentation is forthcoming.  
For the current preliminary release, please refer
to the examples and vignettes provided.
}

\value{
A "list" (for projects) or named "vector" (for metagenomes) when "detail=NULL".
Otherwise, a "data.frame".
Complete technical documentation is forthcoming.  
For the current preliminary release, please refer
to the examples and vignettes provided.
}

\author{Daniel T. Braithwaite}

\seealso{
\code{\link{dir.MGRAST}},
\code{\link{search.MGRAST}},
\code{\link{biomRequest}},
\code{MGRASTer::\link{call.MGRAST}}
}

\examples{
##  three levels of detail for project metadata
xx <- "mgp21 mgp24 mgp30"
metadata (xx)
metadata (xx, detail=TRUE)
names (metadata (xx, detail="verbose"))

##  similarly (but not identically) for metagenome metadata
yy <- "mgm4440066.3 mgm4440062.3 mgm4440055.3 mgm4441681.3 mgm4440463.3 mgm4440464.3"
metadata (yy)
metadata (yy, detail=TRUE)
names (metadata (yy, detail="metadata"))

##  relevant documentation for underlying API calls
doc.MGRAST(3, head=c('project','instance','parameters','options'))
doc.MGRAST(3, head=c('metagenome','instance','parameters','options'))
}
