\name{dir.MGRAST}
\alias{dir.MGRAST}

\title{
List directory of MG-RAST projects
}

\description{
Get full or partial directory listing of MG-RAST projects,
with minimal or detailed metadata per project.
}

\usage{
dir.MGRAST(from, to, length.out=0, ..., quiet=TRUE)
}

\arguments{
  \item{from}{starting index for returned results (numeric)}
  \item{to}{ending index for returned results (numeric)}
  \item{length.out}{number of results to return; default zero means all (numeric)}
  \item{\dots}{arguments to "call.MGRAST()"}
  \item{quiet}{suppress messages and warnings? ("logical")}
}

\details{
Complete technical documentation is forthcoming.  
For the current preliminary release, please refer
to the examples and vignettes provided.
}

\value{
A "data.frame".
Complete technical documentation is forthcoming.  
For the current preliminary release, please refer
to the examples and vignettes provided.
}

\author{Daniel T. Braithwaite}

\seealso{
\code{\link{search.MGRAST}},
\code{\link{metadata.character}},
\code{MGRASTer::\link{call.MGRAST}}
}

\examples{
##  names of all public projects
dir.MGRAST()$name

##  ids of all public projects
rownames(dir.MGRAST())

##  investigators who have contributed public projects
unique(dir.MGRAST()$pi)

##  first 25 projects submitted to MG-RAST
dir.MGRAST(len=25, order="id")

##  detailed information about them
names (dir.MGRAST(len=25, order="id", verbosity="verbose"))

##  relevant documentation for the underlying API call
doc.MGRAST(3, head=c('project','query','parameters','options'))
}
