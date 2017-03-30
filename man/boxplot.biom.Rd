\name{boxplot.biom}
\alias{boxplot}
\alias{boxplot.biom}

\title{
Summarize BIOM data in boxplots
}

\description{
Summarize distribution of a \code{biom} object in columnwise boxplots.
}

\usage{
\method{boxplot}{biom}(x, y=NULL, ..., map=NULL, columns=TRUE)
}

\arguments{
  \item{x}{an object (\code{biom})}
  \item{y}{optionally, a second object for comparison (\code{biom})}
  \item{\dots}{arguments to \code{graphics::boxplot()}}
  \item{map}{assignment of \code{par} variables to metadata fields (\code{character})}
  \item{columns}{subselection of columns (\code{integer}, \code{character}, or \code{logical})}
}

\details{
Complete technical documentation is forthcoming.  
For the current preliminary release, please refer
to the examples provided.
}

\value{
Complete technical documentation is forthcoming.  
For the current preliminary release, please refer
to the examples provided.
}

\author{Daniel T. Braithwaite and Kevin P. Keegan}

\seealso{
\code{graphics::\link{boxplot}},
\code{BIOM.utils::\link{biom}}
}

\examples{
####  simple use
xx2t <- transform (xx2, t_Log)
boxplot (xx2t, main="log transformed data", notch=FALSE)

####  plotting raw and normalized against each other
columns (xx2t, "material")
boxplot (xx2t, xx2, x.main="log of data", y.main="raw data", map=c(col="material"),
  col=c("freshwater"="darkorange", "hot spring"="slateblue", 
  "hot spring ; microbial mat"="chocolate4"), notch=FALSE)

####  label by metadata
columns (xx4, "sample_name")
boxplot (transform (xx4, t_Log), names="$$sample.data.sample_name", notch=FALSE)

####  two normalizations plotted against each other
xx2tt <- transform (xx2, t_Threshold=list(entry.min=5), t_Log)
boxplot (xx2t, xx2tt, notch=FALSE, x.main="log transformation", 
  y.main="low counts removed, then log transformation")
}
