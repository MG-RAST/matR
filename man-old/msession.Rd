\name{msession}
\alias{msession}
\alias{Authentication}
\title{Session Configuration Including Authentication}
\description{Accesses control details for the current session, including authentication, API server, and debugging info.}
%\usage{See examples below.}
%\details{...}
%\value{...}
\author{Daniel T. Braithwaite}
\seealso{\code{\link{Visualization}}, \code{\link{asFile}}}
\examples{
\dontrun{
# set and show current authentication key
msession$setAuth()
msession$setAuth(file="mykey.txt")
msession$getAuth()

# show available and current servers, and set
msession$servers()
msession$server()
msession$server("http://api.metagenomics.anl.gov")

# show recent API calls for debugging
msession$urls()
}
}
