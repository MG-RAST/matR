\name{BIOMannotations}
\alias{rows}
\alias{rows<-}
\alias{columns}
\alias{columns<-}

\title{
Search and change row or column annotations of BIOM data
}

\description{
For an object of class \code{biom},
find row or column annotations (BIOM \code{metadata}) 
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
  \item{x}{an object (\code{biom})}
  \item{pattern}{literal string or regular expression identifying metadata by name (\code{character})}
  \item{name}{name for new metadata annotation (\code{character})}
  \item{value}{new metadata, one value per row/column}
}

\details{
Complete technical documentation is forthcoming.  
For the current preliminary release, please refer
to the examples provided.
}

\value{
For \code{rows()} and \code{columns()}, 
a \code{data.frame} consisting of the metadata of \code{x} matching \code{pattern} by name.
For the replacement functions,
the object \code{x} with updated metadata.
}

\author{Daniel T. Braithwaite}

\seealso{
\code{BIOM.utils::\link{biom}},
\code{\link{regex}}
}

\examples{
####  exact sampling locations returned in a data.frame
columns (xx3, "latitude|longitude")

####  a data.frame is returned even in case of a single matching metadata field
is.data.frame (columns (xx1, "sample.data.biome"))

####  project IDs and environmental package metadata -- note regex here and above
colnames (columns (xx2, "project\\\.id|^env_package"))

####  row metadata makes annotation hierarchy levels available,
####  so typical row metadata has few components, and here just two
names (rows (xx1))
rows (xx1, "ontology1")

####  here, the rownames and the (single) variable of the data.frame coincide
rows (xx1, "ontology2")

####  variables are almost always coded as factors
is.factor (columns (xx1, "sample.data.biome") [[1]])
}
