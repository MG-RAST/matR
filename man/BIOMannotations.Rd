\name{BIOMannotations}
\alias{rows}
\alias{rows<-}
\alias{columns}
\alias{columns<-}

\title{
Search and change row or column annotations of BIOM data
}

\description{
For an object of class "biom",
find row or column annotations (that is, within the BIOM "metadata") 
that match by name a given pattern,
or append new annotations.
}

\usage{
rows(x, pattern="*")
rows(x, name) <- value

columns(x, pattern="*")
columns(x, name) <- value
}

\arguments{
  \item{x}{an object ("biom")}
  \item{pattern}{literal string or regular expression identifying metadata field(s) by name ("character")}
  \item{name}{name for new metadata annotation ("character")}
  \item{value}{new metadata annotations, one per row (or column)}
}

\details{
Complete technical documentation is forthcoming.  
For the current preliminary release, please refer
to the examples and vignettes provided.
}

\value{
For "rows()" and "columns()", 
a "data.frame" consisting of the metadata fields of "x" that match "pattern".
For the replacement functions,
the object "x" with updated metadata.
}

\author{Daniel T. Braithwaite}

\seealso{
\code{BIOM.utils::\link{biom}}
}

\examples{
# ex with [,] to get single factor
}
