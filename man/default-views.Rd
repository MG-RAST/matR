\name{default-views}
\alias{default.views}
\alias{view.params}
\docType{data}
\title{Data Attributes of Metagenome Collections}
\description{Data objects provided to explain possible views of a metagenome collection}
\usage{
default.views
view.params
}
\details{\code{view.params} shows parameters and allowed values for specifying views of a collection.  \code{default.views} shows the named views included by default, when a collection is created with no views explicitly specified.}
\examples{
default.views
default.views$raw

view.params
view.params$level$organism
}
\keyword{datasets}

%view.descriptions <- list (
%	entry = list (
%		count = "raw abundance counts",
%		normed = "log-normalized abundance counts, centered and scaled to [0,1]",
%		no.singletons = "raw abundance counts, with counts of 1 replaced by zero",
%		no.singletons.normed = "log-normalized abundance counts with counts of 1 replaced by zero, centered and scaled to [0,1]",
%		evalue = "",
%		length = "",
%		percentid = ""),
%	annot = list (
%		`function` = "functional annotations",
%		taxon = "taxonomic annotations"),
%	level = list (
%		level1 = "annotations at function level 1",
%		level2 = "annotations at function level 2",
%		level3 = "annotations at function level 3",
%		`function` = "annotations at function level"),
%	`source` = list ())