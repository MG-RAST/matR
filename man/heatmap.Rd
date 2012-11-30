\name{heatmap}
\alias{heatmap}
\title{
Create Heatmap with Dendrograms}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
heatmap(x, figure_type = "jpg", image_out = "HD.jpg", image_title = image_out, heat_color1 = "red", heat_color2 = "green", palette_n = 12, labRow = NULL, labCol = NULL, hclustfun_method = "ward", figure_width = 1000, figure_height = 1000, figure_res = NA, Rowv = TRUE, Colv = if (symm) "Rowv" else TRUE, distfun = stats::dist, hclustfun = hclust, dendrogram = "both", symm = FALSE, scale = "none", na.rm = TRUE, revC = identical(Colv, "Rowv"), add.expr, breaks, symbreaks = min(x < 0, na.rm = TRUE) || scale != "none", col = "heat.colors", colsep, rowsep, sepcolor = "white", sepwidth = c(0.05, 0.05), cellnote, notecex = 1, notecol = "cyan", na.color = par("bg"), trace = "none", tracecol = "cyan", hline = median(breaks), vline = median(breaks), linecol = tracecol, margins = c(5, 1), ColSideColors, RowSideColors, row_lab_mult = 2, col_lab_mult = 3, cexRow = row_lab_mult * (1/log10(nr)), cexCol = col_lab_mult * (1/log10(nc)), key = FALSE, keysize = 0.9, key_lines = 1, key_text = "Key (min to max)", key_text_cex = 0.5, key_xlabel = NULL, key_ylabel = NULL, density.info = c("histogram", "density", "none"), denscol = tracecol, symkey = min(x < 0, na.rm = TRUE) || symbreaks, densadj = 0.25, xlab = NULL, ylab = NULL, lmat = NULL, lhei = NULL, lwid = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{figure_type}{
%%     ~~Describe \code{figure_type} here~~
}
  \item{image_out}{
%%     ~~Describe \code{image_out} here~~
}
  \item{image_title}{
%%     ~~Describe \code{image_title} here~~
}
  \item{heat_color1}{
%%     ~~Describe \code{heat_color1} here~~
}
  \item{heat_color2}{
%%     ~~Describe \code{heat_color2} here~~
}
  \item{palette_n}{
%%     ~~Describe \code{palette_n} here~~
}
  \item{labRow}{
%%     ~~Describe \code{labRow} here~~
}
  \item{labCol}{
%%     ~~Describe \code{labCol} here~~
}
  \item{hclustfun_method}{
%%     ~~Describe \code{hclustfun_method} here~~
}
  \item{figure_width}{
%%     ~~Describe \code{figure_width} here~~
}
  \item{figure_height}{
%%     ~~Describe \code{figure_height} here~~
}
  \item{figure_res}{
%%     ~~Describe \code{figure_res} here~~
}
  \item{Rowv}{
%%     ~~Describe \code{Rowv} here~~
}
  \item{Colv}{
%%     ~~Describe \code{Colv} here~~
}
  \item{distfun}{
%%     ~~Describe \code{distfun} here~~
}
  \item{hclustfun}{
%%     ~~Describe \code{hclustfun} here~~
}
  \item{dendrogram}{
%%     ~~Describe \code{dendrogram} here~~
}
  \item{symm}{
%%     ~~Describe \code{symm} here~~
}
  \item{scale}{
%%     ~~Describe \code{scale} here~~
}
  \item{na.rm}{
%%     ~~Describe \code{na.rm} here~~
}
  \item{revC}{
%%     ~~Describe \code{revC} here~~
}
  \item{add.expr}{
%%     ~~Describe \code{add.expr} here~~
}
  \item{breaks}{
%%     ~~Describe \code{breaks} here~~
}
  \item{symbreaks}{
%%     ~~Describe \code{symbreaks} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{colsep}{
%%     ~~Describe \code{colsep} here~~
}
  \item{rowsep}{
%%     ~~Describe \code{rowsep} here~~
}
  \item{sepcolor}{
%%     ~~Describe \code{sepcolor} here~~
}
  \item{sepwidth}{
%%     ~~Describe \code{sepwidth} here~~
}
  \item{cellnote}{
%%     ~~Describe \code{cellnote} here~~
}
  \item{notecex}{
%%     ~~Describe \code{notecex} here~~
}
  \item{notecol}{
%%     ~~Describe \code{notecol} here~~
}
  \item{na.color}{
%%     ~~Describe \code{na.color} here~~
}
  \item{trace}{
%%     ~~Describe \code{trace} here~~
}
  \item{tracecol}{
%%     ~~Describe \code{tracecol} here~~
}
  \item{hline}{
%%     ~~Describe \code{hline} here~~
}
  \item{vline}{
%%     ~~Describe \code{vline} here~~
}
  \item{linecol}{
%%     ~~Describe \code{linecol} here~~
}
  \item{margins}{
%%     ~~Describe \code{margins} here~~
}
  \item{ColSideColors}{
%%     ~~Describe \code{ColSideColors} here~~
}
  \item{RowSideColors}{
%%     ~~Describe \code{RowSideColors} here~~
}
  \item{row_lab_mult}{
%%     ~~Describe \code{row_lab_mult} here~~
}
  \item{col_lab_mult}{
%%     ~~Describe \code{col_lab_mult} here~~
}
  \item{cexRow}{
%%     ~~Describe \code{cexRow} here~~
}
  \item{cexCol}{
%%     ~~Describe \code{cexCol} here~~
}
  \item{key}{
%%     ~~Describe \code{key} here~~
}
  \item{keysize}{
%%     ~~Describe \code{keysize} here~~
}
  \item{key_lines}{
%%     ~~Describe \code{key_lines} here~~
}
  \item{key_text}{
%%     ~~Describe \code{key_text} here~~
}
  \item{key_text_cex}{
%%     ~~Describe \code{key_text_cex} here~~
}
  \item{key_xlabel}{
%%     ~~Describe \code{key_xlabel} here~~
}
  \item{key_ylabel}{
%%     ~~Describe \code{key_ylabel} here~~
}
  \item{density.info}{
%%     ~~Describe \code{density.info} here~~
}
  \item{denscol}{
%%     ~~Describe \code{denscol} here~~
}
  \item{symkey}{
%%     ~~Describe \code{symkey} here~~
}
  \item{densadj}{
%%     ~~Describe \code{densadj} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{lmat}{
%%     ~~Describe \code{lmat} here~~
}
  \item{lhei}{
%%     ~~Describe \code{lhei} here~~
}
  \item{lwid}{
%%     ~~Describe \code{lwid} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
