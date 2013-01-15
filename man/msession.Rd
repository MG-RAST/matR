\name{msession}
\alias{msession}
\alias{Authentication}
\title{Get or Set Session Configuration}
\description{Reports or adjusts settings of the current session including authentication key}
\usage{...}
\details{...}
\value{...}
\author{Daniel T. Braithwaite}
\seealso{\code{\link{Visualization}}, \code{\link{asFile}}}
\examples{
# show current authentication key (if set)
msession$getAuth()

# show available servers
msession$servers()

# show current server
msession$server()

# show recent URL (for debugging)
msession$urls()

\dontrun{
# set authentication key
msession$setAuth()

# set server
msession$server("http://api.metagenomics.anl.gov")
}
}
