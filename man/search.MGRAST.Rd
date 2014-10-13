\name{search.MGRAST}
\alias{search.MGRAST}

\title{
Search metagenomes in MG-RAST
}

\description{
Find metagenomes in MG-RAST matching search terms
specified for metadata, annotations, and/or md5s,
giving minimal or detailed metadata per metagenome.
}

\usage{
search.MGRAST(..., quiet=FALSE)
}

\arguments{
  \item{\dots}{arguments to "call.MGRAST()"}
  \item{quiet}{suppress messages and warnings? ("logical")}
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


##  relevant documentation for the underlying API call
doc.MGRAST(3, head=c('metagenome','query','parameters','options'))
}
