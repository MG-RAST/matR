\name{matR_preprocessing}
\alias{matR_preprocessing}
\title{Preprocessing of metagenomic abundance count data}
\description{This function enables a variety of preprocessing procedures that can be applied to (raw) abundance count data. These include filtering of singletons/low abundance count data, multiple methods to normalize the data (supported by accessory pacakges like \code{DESeq} and \code{preprocessCore}) and the ability to apply simple linear scaling to the entire data set. This function can also be used to produce boxplots of the sampled prior to and after normalization}
\usage{heatmap(data_in, ...)}
\arguments{
  \item{data_in}{no default: name of tab delimited file or R matrix that contains the raw abundance count data}
  \item{data_type}{default="file": c("file", "r_matrix") Data type for data_in}
  \item{output_file}{default="default": name for flat file output, automatically generated for "default"}
  \item{removeSg}{default=TRUE: boolean that indicates if singleton/low abundance filtering will be used}
  \item{removeSg_valueMin}{default=2: lowest retained value per matrix cell (lower values converted to 0)  }
  \item{removeSg_rowMin}{default=4: lowest retained sum (after removeSg_valueMin) per matrix row (rows with lower values are removed)}
  \item{log_transform}{default=FALSE: boolean indicating if data should be log transformed log2(x+1)}
  \item{norm_method}{default="DESeq": c("standardize", "quantile", "DESeq", none) indicates the normalization procedure that will be applied to the data}
  \item{}{}

  \item{\dots}{further arguments as indicated below}
}
%\details{...}
%\value{...}
\author{Kevin P. Keegan, Daniel T. Braithwaite}
\seealso{\code{\link{heatmap-methods}}}
\examples{
# simple use
heatmap (Waters)

# map a different view
Waters$lev2 <- c (entry = "normed", level = "level2")
heatmap (Waters, view = "lev2")

# map only selected rows, including a few graphical parameters (see ?gplots::heatmap.2)
groups (Waters) <- c (rep (1, 15), rep (2, 9))
results <- sigtest (Waters, view = "lev2", test = "Kruskal-Wallis")
heatmap (Waters, view = "lev2", rows = (results$p.value < 0.05), main = "Comparison of Water Samples", labCol = 1:24)
}
