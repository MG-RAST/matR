\name{Authentication}
\alias{Authentication}
\alias{authentication}
\alias{setAuth}
\alias{getAuth}
\alias{mconfig$setAuth}
\alias{mconfig$getAuth}
\title{
	Get or Set Authentication Key
	}
\description{
	Set an authentication key for access to private data, or get the key currently in use.
	}
\usage{
	`mconfig$setAuth`()
	`mconfig$getAuth`()
	}
\details{
	\code{mconfig} is a data object created in each session in the \code{matR} package environment to hold configuration information for that session.

	\code{mconfig$setAuth()} prompts for and stores an authentication key.  

	\code{mconfig$getAuth()} returns the auth key currently in use.

	Note that public data is available without an auth key.  
  Keys are associated to specific \code{MG-RAST} accounts and may be obtained upon login from the \code{MG-RAST} account management webpage linked below.
  Keys are not stored between \code{R} sessions.
	}
\value{
  \code{mconfig$getAuth()} returns a length one character vector of the current auth key or \code{""} if none.  
  The value of \code{mconfig$setAuth()} is \code{NULL}.
}
\references{\url{http://metagenomics.anl.gov?page=AccountManagement}}
\author{Daniel Braithwaite}
\seealso{
	\code{\link{mconfig}}
	}
\examples{
\dontrun{
mconfig$setAuth()
mconfig$getAuth()
}
}
