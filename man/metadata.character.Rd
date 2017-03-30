\name{metadata.character}
\alias{metadata}
\alias{metadata.character}

\title{
Get metadata of projects and metagenomes
}

\description{
Get metadata of projects or metagenomes specified by ID,
or simply look up correspondence of project and metagenome IDs.
}

\usage{
\method{metadata}{character}(x, detail=NULL, ..., quiet=TRUE, file)
}

\arguments{
  \item{x}{IDs of projects or metagenomes (\code{character})}
  \item{detail}{level of metadata detail (\code{NULL}, \code{TRUE}, or \code{character})}
  \item{\dots}{arguments to \code{call.MGRAST()}}
  \item{quiet}{suppress messages and warnings? (\code{logical})}
  \item{file}{file containing project or metagenome IDs (string)}
}

\details{
Complete technical documentation is forthcoming.  
For the current preliminary release, please refer
to the examples provided.
}

\value{
A \code{list} (for projects) or \code{vector} (for metagenomes) when \code{detail=NULL}.
Otherwise, a \code{data.frame}.
}

\note{
The function \code{metadata()} is declared S3 generic in required package \code{BIOM.utils},
and a method is defined there for class \code{biom}.  
Although here is documented another method of the same function,
the actual functionality is unrelated.
}

\author{Daniel T. Braithwaite}

\seealso{
\code{\link{dir.MGRAST}},
\code{\link{search.MGRAST}},
\code{\link{biomRequest}},
\code{MGRASTer::\link{call.MGRAST}}
}

\examples{
\dontrun{
####  three levels of detail for project metadata
xx <- "mgp21 mgp24 mgp30"
metadata (xx)
metadata (xx, detail=TRUE)
names (metadata (xx, detail="verbose"))

####  similar (but not identical) for metagenome metadata
yy <- "mgm4440066.3 mgm4440062.3 mgm4440055.3 mgm4441681.3 mgm4440463.3 mgm4440464.3"
metadata (yy)
metadata (yy, detail=TRUE)
names (metadata (yy, detail="metadata"))
}

####  relevant documentation for underlying API calls
doc.MGRAST (3, head=c('project','instance','parameters','options'))
doc.MGRAST (3, head=c('metagenome','instance','parameters','options'))
}
