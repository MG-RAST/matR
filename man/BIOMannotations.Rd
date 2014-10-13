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
\code{\link{regex}}
}

\examples{
#  exact sampling locations returned in a data.frame
columns (xx3, "latitude|longitude")

#  project IDs and environmental package metadata -- note regex here and above
colnames (columns (xx2, "project\\.id|^env_package"))

#  row metadata makes annotation hierarchy levels available,
#  so typical row metadata has few components -- here just two
names (rows (xx1))
rows (xx1, "ontology1")

#  here, the rownames and the variable of the data.frame coincide
rows (xx1, "ontology2")

#  a data.frame is returned even in case of a single metadata field
is.data.frame (columns (xx1, "sample.data.biome"))

#  and variables are almost always coded as factors
is.factor (columns (xx1, "sample.data.biome") [[1]])
}
