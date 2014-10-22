\name{IDsets}
\alias{IDsets}
\alias{readSet}
\alias{expandSet}
\alias{scrapeSet}
\alias{scrubSet}

\title{
Handle sets of metagenome and project IDs
}

\description{
Utility functions to to establish a standard format
and handle sets of metagenome and project IDs,
possibly with metadata.
}

\usage{
readSet(file)
expandSet(x)
scrapeSet(x)
scrubSet(x, resources="metagenome")
}

\arguments{
  \item{file}{a filename (\code{character})}
  \item{x}{metagenome and project IDs, possibly with metadata (\code{character}, \code{numeric}, or \code{data.frame})}
  \item{resources}{corresponding resource designation(s) (\code{character})}
}

\details{Internal and undocumented at present.}
\value{Internal and undocumented at present. }
\author{Daniel T. Braithwaite}
