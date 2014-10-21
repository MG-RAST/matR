\name{search.MGRAST}
\alias{search.MGRAST}

\title{
Find metagenomes matching specified criteria
}

\description{
Find metagenomes matching search terms
specified for metadata, annotations, and/or md5s,
giving minimal or detailed metadata per metagenome.
}

\usage{
search.MGRAST(public=NULL, detail=NULL, match.all=TRUE, ..., quiet=TRUE)
}

\arguments{
  \item{public}{... (\code{logical})}
  \item{detail}{... (\code{logical} or \code{character})}
  \item{match.all}{... (\code{logical})}
  \item{\dots}{arguments to \code{call.MGRAST()}}
  \item{quiet}{suppress messages and warnings? (\code{logical})}
}

\details{
Complete technical documentation is forthcoming.  
For the current preliminary release, please refer
to the examples and vignettes provided.
}

\value{
Complete technical documentation is forthcoming.  
For the current preliminary release, please refer
to the examples and vignettes provided.
}

\author{Daniel T. Braithwaite}

\seealso{
\code{\link{dir.MGRAST}},
\code{\link{metadata.character}},
\code{MGRASTer::\link{call.MGRAST}}
}

\examples{
\dontrun{
}
#  relevant documentation for the underlying API call
doc.MGRAST (3, head=c('metagenome','query','parameters','options'))
}
