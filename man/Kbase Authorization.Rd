\name{Kbase Authorization}
\alias{Authorization}
\alias{.kbSetAuth}
\alias{.kbGetAuth}
\alias{Kbase Authorization}
\title{
Get and Set MG-RAST Authorization Key
}
\description{
Get and set the authorization key for access to MG-RAST resources.
}
\usage{
.kbSetAuth(key = NULL, save = FALSE)
.kbGetAuth()
}
\arguments{
  \item{key}{
%%     ~~Describe \code{key} here~~
}
  \item{save}{
%%     ~~Describe \code{save} here~~
}
}
\details{
If \code{kbGet} does not function problem, the authorization key may need to be reset.}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Daniel Braithwaite
}
\note{
%%  ~~further notes~~
}
\section{Warning}{
If you save your session upon exit from R, the authorization key is stored in .RData, which may be insecure.
}
\seealso{
\code{\link{kbGet}}
}
\examples{
.kbSetAuth( "HYnMCwTavrHNC9dWPC5GzmzSH" )
.kbGetAuth()
}
