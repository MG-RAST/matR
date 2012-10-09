\name{asFile}
\alias{asFile}
\alias{ImportExport}
\alias{Import and Export}
\alias{Import}
\alias{import}
\alias{Export}
\alias{export}
\title{Import and Export between Objects}
\description{
	\code{asFile} is a simple export routine to write several kinds of object to file.  
	It is a light wrapper for \code{write.table()} and \code{save()}, and accepts most of the same options (such as whether to write comma- or tab-separated output).
	Standing options can be set with \code{mconfig}. 
}
\usage{
asFile (x, fname, ...)
}
\arguments{
	\item{x}{currently this may be a \code{matrix}, \code{Matrix}, or \code{mmatrix}}
	\item{fname}{the name for the export file}
  \item{...}{addional parameters}
}
\details{
\code{fname} will be prepended with \code{mconfig$path()}.
Standing options may be set with \code{mconfig$exp(opt1=value1, opt2=value2, ...)}: details of specific imp and exp values.
}
\value{
The name of the file created, or an error.
}
\author{Daniel T. Braithwaite}
\seealso{\code{\link{asFile-methods}}, \code{\link{mconfig}}, \code{\link{write.table}}, \code{\link{save}}}
\examples{
## ...
}
