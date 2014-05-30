\name{sigtest}
\alias{sigtest}
\title{Significance Test of Collection Annotations Against Groupings}
\description{Applies specified significance test to annotations of a grouped collection}
\usage{sigtest(x, ...)}
\arguments{
  \item{x}{collection}
  \item{\dots}{further arguments as indicated below}
}
%\details{...}
\value{A list with components expressing the result of significance testing.}
\author{Kevin P. Keegan, Daniel T. Braithwaite}
\seealso{\code{\link{Collections}}, \code{\link{sigtest-methods}}}
\examples{
groups(Guts) <- c(1,1,1,2,2,2,2)
res <- sigtest(Guts, view = "nrm", test = "t-test-un-paired")
res <- sigtest(Guts, "nrm", "Mann-Whitney_un-paired-Wilcoxon")
res <- sigtest(Guts, "nrm", "ANOVA-one-way")
res <- sigtest(Guts, "nrm", "Kruskal-Wallis")

groups(Waters) <- c(rep(1,12), rep(2,12))
res <- sigtest(Waters, "nrm", "t-test-paired")
res <- sigtest(Waters, "nrm", "t-test-un")
res <- sigtest(Waters, "nrm", "Mann-Whitney")
res <- sigtest(Waters, "nrm", "Wilcoxon-paired")
res <- sigtest(Waters, "nrm", "ANOVA")
res <- sigtest(Waters, test = "Kruskal")

names(res)
head(res$mean)
head(res$sd)
head(res$stat)
head(res$p.value)
}

