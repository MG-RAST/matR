\name{rowstats}
\alias{rowstats}
\alias{rowstats.matrix}
\alias{rowstats.biom}

\title{
Apply selected significance test across rows
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
  qvalue=FALSE, fdr.level=NULL, ...)

\method{rowstats}{biom}(x, groups, ...)
}

\arguments{
  \item{x}{a matrix-like object (\code{matrix} or \code{biom})}
  \item{groups}{a grouping of columns (\code{character} or \code{factor})}
  \item{test}{name of statistical test (\code{character})}
  \item{qvalue}{perform qvalue calculation? (\code{logical})}
  \item{fdr.level}{false-discovery rate parameter, passed to \code{qvalue()} (\code{numeric})}
  \item{\dots}{unused}
}

\details{
Complete technical documentation is forthcoming.  
For the current preliminary release, please refer
to the examples provided.
}

\value{
Complete technical documentation is forthcoming.  
For the current preliminary release, please refer
to the examples provided.
}

\author{Kevin P. Keegan and Daniel T. Braithwaite}

\seealso{
\code{stats::\link{t.test}},
\code{stats::\link{wilcox.test}},
\code{stats::\link{kruskal.test}},
\code{stats::\link{anova}}
}

\examples{
####  Kruskal test applied, for the case of more than two metagenome groups
columns (xx1, "host_common_name")
str (rowstats (xx1, groups="$$host_common_name", test="Kruskal"))

####  force a desired grouping of metagenomes
gg <- columns (xx2, "material") [[1]]
gg
levels (gg) <- levels (gg) [c(1,2,2)]
str (rowstats (xx2, groups=gg, test="t-test-unpaired"))
}
