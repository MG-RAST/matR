\name{rowstats}
\alias{rowstats}
\alias{rowstats.matrix}
\alias{rowstats.biom}

\title{
Apply selected significance test
}

\description{
Apply selected significance test across rows to grouped columns,
with optional q-value calculation.
}

\usage{
rowstats(x, ...)

\method{rowstats}{matrix}(x, groups, 
	test=c("Kruskal-Wallis", "t-test-paired", "Wilcoxon-paired", "t-test-unpaired",
		"Mann-Whitney-unpaired-Wilcoxon", "ANOVA-one-way"),
	fdr.level=NULL, qvalue=FALSE, ...)

\method{rowstats}{biom}(x, groups,
	test=c("Kruskal-Wallis", "t-test-paired", "Wilcoxon-paired", "t-test-unpaired",
		"Mann-Whitney-unpaired-Wilcoxon", "ANOVA-one-way"),
	fdr.level=NULL, qvalue=FALSE, ...)
}

\arguments{
  \item{x}{a matrix-like object ("matrix" or "biom")}
  \item{groups}{a grouping of columns ("character" or "factor")}
  \item{test}{name of statistical test ("character")}
  \item{fdr.level}{false-discovery rate parameter, passed to "qvalue()" ("numeric")}
  \item{qvalue}{perform qvalue calculation? ("logical")}
  \item{\dots}{unused}
}

\details{
Complete technical documentation is forthcoming.  
For the current preliminary release, please refer
to the examples and vignettes provided.
}

\value{
Complete technical documentation is forthcoming.  
For the current preliminary release, please refer
to the examples and vignettes provided.
}

\author{Kevin P. Keegan and Daniel T. Braithwaite}

\seealso{
\code{stats::\link{t.test}},
\code{stats::\link{wilcox.test}},
\code{stats::\link{kruskal.test}},
\code{stats::\link{anova}}
}

\examples{
#  Kruskal test applied, for the case of more than two metagenome groups
columns (xx1, "host_common_name")
str (rowstats (xx1, groups="$$host_common_name", test="Kruskal"))

#  force a desired grouping of metagenomes
gg <- columns (xx2, "material") [[1]]
gg
levels (gg) <- levels (gg) [c(1,2,2)]
str (rowstats (xx2, groups=gg, test="t-test-unpaired"))


}
