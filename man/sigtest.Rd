\name{sigtest}
\alias{sigtest}
\title{Test Significance of Annotations}
\description{Applies specified significance test to annotations of a grouped collection}
\usage{sigtest(x, ...)}
\arguments{
  \item{x}{...}
  \item{\dots}{...}
}
\details{...}
\value{...}
\author{Kevin P. Keegan, Daniel T. Braithwaite}
\seealso{\code{\link{Collections}}}
\examples{
groups(Guts) <- c(1,1,1,2,2,2,2)
res <- sigtest(Guts, view = "normed", test = "t-test-un-paired")
res <- sigtest(Guts, "normed", "Mann-Whitney_un-paired-Wilcoxon")
res <- sigtest(Guts, "normed", "ANOVA-one-way")
res <- sigtest(Guts, "normed", "Kruskal-Wallis")

groups(Waters) <- c(rep(1,12), rep(2,12))
res <- sigtest(Waters, "normed", "t-test-paired")
res <- sigtest(Waters, "normed", "t-test-un")
res <- sigtest(Waters, "normed", "Mann-Whitney")
res <- sigtest(Waters, "normed", "Wilcoxon-paired")
res <- sigtest(Waters, "normed", "ANOVA")
res <- sigtest(Waters, test = "Kruskal")

names(res)
head(res$mean)
head(res$sd)
head(res$stat)
head(res$p.value)
}

