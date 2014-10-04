\name{transform.biom}
\alias{t_ColCenter}
\alias{t_ColScale}
\alias{t_DENorm}
\alias{t_Log}
\alias{t_NA2Zero}
\alias{t_Threshold}
\alias{transform.biom}

\title{
Apply mathematical transformations to BIOM data
}

\description{
Prepare an object of class "biom" for further analysis 
by applying selected transformations with specified parameters.
}

\usage{
\method{transform}{biom}(`_data`, ...)

t_ColCenter(x, ...)
t_ColScale(x, ...)
t_DENorm(x, DEparam, ...)
t_Log(x, ...)
t_NA2Zero(x, ...)
t_Threshold(x, entry.lim=1, row.lim=1, col.lim=1)
}

\arguments{
  \item{_data}{an object ("biom")}
  \item{x}{"matrix"}
  \item{DEparam}{arguments to differential expression normalization}
  \item{entry.lim}{make zero entries not exceeding this number ("numeric")}
  \item{row.lim}{remove rows with sum not exceeding this number ("numeric")}
  \item{col.lim}{remove columns with sum not exceeding this number ("numeric")}
  \item{\dots}{transformations to apply and parameters thereto (see below)}
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

\author{Daniel T. Braithwaite and Kevin P. Keegan}

\seealso{
\code{BIOM.utils::\link{biom}}
}

\examples{
}
