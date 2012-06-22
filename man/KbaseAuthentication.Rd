\name{KbaseAuthentication}
\alias{Kbase Authentication}
%\alias{.mSetAuth}
\alias{.mGetAuth}
\alias{authentication}
\title{
	Get and Set Kbase MG-RAST Authentication Key
	}
\description{
	Get and set the active authentication key for access to non-public MG-RAST resources.
	}
\usage{
%	.mSetAuth(key = NULL, save = FALSE)
	.mGetAuth()
	}
%\arguments{
%	\item{key}{...}
%	\item{save}{..}
%	}
\details{
	If \code{mGet} does not function properly, the authentication key may need to be reset.
	Auth keys are generated and retrieved from the user profile page of the MG-RAST website.
	}
\value{...}
\references{\url{http://metagenomics.anl.gov}}
\author{Daniel Braithwaite}
\section{Warning}{
	If your session is saved upon exit from R, the auth key is stored in .RData, which may be insecure.
	}
\seealso{
	\code{\link{mGet}}
	}
\examples{
## .mSetAuth( "HYnMCwTavrHNC9dWPC5GzmzSH" )
## .mGetAuth()
}
