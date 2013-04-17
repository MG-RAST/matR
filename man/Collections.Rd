\name{Collections}
\alias{Collections}
\alias{collection}
\alias{samples}
\alias{groups}
\alias{groups<-}
\alias{views}
\alias{viewnames}
\alias{viewnames<-}
\alias{rownames}
\title{About Metagenome Collections}
\description{The basic object of analysis in \code{matR} is a matrix, with columns labeled by metagenome ID and rows by annotation.
There are various possibilities for the matrix content (constituting different \code{views}), and analyses may examine several such matrices 
together.  A group of such matrices referring to the same metagenomes is called a \code{collection}.}
\usage{
collection (x, ...)
samples (x, ...)
groups (x, ...)
groups (x) <- value
views (x, ...)
viewnames (x, ...)
viewnames (x) <- value
rownames (x, ...)
}
\arguments{
  \item{x}{collection or metagenome IDs}
  \item{value}{replacement value}
  \item{...}{currently unused}
}
\note{Private data requires an authentication key.}
%\value{...}
\author{Daniel T. Braithwaite}
\seealso{
	\code{\link{Authentication}}, 
	\code{\link{default.views}}, 
	\code{\link{collection-methods}},
	\code{\link{coerce-methods}}}
\examples{
# make collection from single string of IDs directly:
cc <- collection ("4441679.3 4441680.3 4441682.3 4441695.3 4441696.3 4440463.3 4440464.3")

# or from a character vector:
cc <- collection (guts)

# or with custom views:
dd <- collection (guts, fl1 = c (level = "level1"), fl2 = c (level = "level2"), fl3 = c (level = "level3"))

# basic ways of looking at a collection:
samples (dd)
views (dd)
viewnames (dd)
rownames (dd)
metadata (dd)

# groups can be assigned and are used for various analyses:
groups (dd) <- c (1, 1, 1, 2, 2, 3, 3)

# reuse views from one collection to make another:
vl <- views (dd)
ee <- collection (waters, as.list (vl))

# make collection from file of IDs:
t <- tempfile ()
write.table (guts, file = t)
ff <- collection (file = t)
unlink (t)
}
