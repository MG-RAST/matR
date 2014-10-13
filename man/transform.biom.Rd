\name{transform.biom}
\alias{transform}
\alias{transform.biom}
\alias{t_ColCenter}
\alias{t_ColScale}
\alias{t_DENorm}
\alias{t_Log}
\alias{t_NA2Zero}
\alias{t_Threshold}


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
t_Threshold(x, entry.min=1, row.min=1, col.min=1)
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

\note{
The function "transform()" is an S3 generic in base R.
However, the method documented here does not apply any existing methods,
and offers rather different functionality.
}

\author{Daniel T. Braithwaite and Kevin P. Keegan}

\note{
The function "transform()" is declared S3 generic in the base package "stats".
The method documented here does not apply any existing methods, however,
but rather relies on entirely different libraries for computation and graphical rendering.
(See below.)
}

\seealso{
\code{BIOM.utils::\link{biom}},
\code{\link{transform}}
}

\examples{
#  simple log-transform
transform (xx1, t_Log)

#  additional filters
transform (xx1, NA2Zero, t_Threshold, t_Log)

#  what is lost with more stringent filtering of low-abundance annotations
yy <- transform (xx2, t_NA2Zero, t_Threshold, t_Log)
zz <- transform (xx2, t_NA2Zero, t_Threshold=list(entry.min=5, row.min=10), t_Log)
setdiff (rownames (yy), rownames (zz))

#  each sample centered around zero; scaling columnwise by standard deviation
transform (xx4, t_NA2Zero, t_Threshold, t_Log, t_ColCenter, t_ColScale)

#  defining a new transformation that indicates presence / absence
t_Indicator <- function (x, ...) { ifelse (x,1,0) }
transform (xx1, t_Threshold = list(entry.min=5), t_Indicator)
}
