\name{ExampleData}
\alias{ExampleData}
\alias{Coral}
\alias{Guts}
\alias{Hospital}
\alias{Marine}
\alias{Mat}
\alias{Waters}
\alias{Whalebone}
\alias{guts}
\alias{waters}
\alias{id.ex}
\alias{mgm.ex}
\alias{mgp.ex}
\docType{data}
\title{Metagenome Collection Example Datasets}
\description{Various metagenome collections packaged as examples, useful for offline exploration and demonstration}
\usage{
Coral
Guts
Hospital
Marine
Mat
Waters
Whalebone
guts
waters
id.ex
mgm.ex
mgp.ex
}
\source{\url{http://metagenomics.anl.gov}}
\seealso{
	\code{\link{Collections}}
}
\examples{
samples(Hospital)
views(Coral)

pco(Guts)

heatmap(Waters)
}
\keyword{datasets}
