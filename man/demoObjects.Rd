\name{demoObjects}
\alias{demoObjects}
\alias{xx1}
\alias{xx2}
\alias{xx3}
\alias{xx4}
\alias{yy1}
\alias{yy2}
\alias{yy3}
\alias{yy4}

\docType{data}

\title{
BIOM annotation data for certain metagenomes and projects
}

\description{
Objects of class \code{biom} for demonstration purposes,
containing annotation data for certain sets of metagenomes.
}

\usage{
xx1
xx2
xx3
xx4
yy1
yy2
yy3
yy4
}

\details{
\code{xx1} to \code{xx4} are packaged as examples to help users get started.
They are objects of class \code{biom} that could be built with \code{biomRequest()}.
They can be built in an automated way by \code{buildDemoSets()}.


\code{yy1} to \code{yy4} correspond to the example lists \code{li1} to \code{li4} from \code{BIOM.utils}.
That is, \code{yy#} is equal to \code{biom(li#)}.  These contain (all but the last) vacuous data.
}

\seealso{
\code{\link{demoSets}},
\code{\link{buildDemoSets}}
}
