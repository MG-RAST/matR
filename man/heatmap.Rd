\name{heatmap}
\alias{heatmap}
\alias{Heatmap Dendrogram Analysis}
\title{
Perform Heatmap Analysis with Dendrograms}
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, figure_type = "jpg", image_out = "HD.jpg", image_title = image_out, 
    heat_color1 = "red", heat_color2 = "green", palette_n = 12, 
    labRow = NULL, labCol = NULL, hclustfun_method = "ward", 
    figure_width = 1000, figure_height = 1000, figure_res = NA, 
    Rowv = TRUE, Colv = if (symm) "Rowv" else TRUE, distfun = stats::dist, 
    hclustfun = hclust, dendrogram = "both", symm = FALSE, scale = "none", 
    na.rm = TRUE, revC = identical(Colv, "Rowv"), add.expr, breaks, 
    symbreaks = min(x < 0, na.rm = TRUE) || scale != "none", 
    col = "heat.colors", colsep, rowsep, sepcolor = "white", 
    sepwidth = c(0.05, 0.05), cellnote, notecex = 1, notecol = "cyan", 
    na.color = par("bg"), trace = "none", tracecol = "cyan", 
    hline = median(breaks), vline = median(breaks), linecol = tracecol, 
    margins = c(5, 1), ColSideColors, RowSideColors, row_lab_mult = 2, 
    col_lab_mult = 3, cexRow = row_lab_mult * (1/log10(nr)), 
    cexCol = col_lab_mult * (1/log10(nc)), key = FALSE, keysize = 0.9, 
    key_lines = 1, key_text = "Key (min to max)", key_text_cex = 0.5, 
    key_xlabel = NULL, key_ylabel = NULL, density.info = c("histogram", 
        "density", "none"), denscol = tracecol, symkey = min(x < 
        0, na.rm = TRUE) || symbreaks, densadj = 0.25, xlab = NULL, 
    ylab = NULL, lmat = NULL, lhei = NULL, lwid = NULL, ...) 
{
    require(Cairo)
    require(gplots)
    require(matlab)
    x <- as.matrix(x)
    file_in <- tempfile()
    number_entries = (dim(x)[1])
    number_samples = (dim(x)[2])
    main = gsub(" ", "", paste(image_title, "::", hclustfun_method, 
        "_clustering"))
    main = gsub(" ", "", main)
    {
        ramp <- colorRamp(c(heat_color1, heat_color2))
        custom_palette <<- rgb(ramp(seq(0, 1, length = palette_n)), 
            max = 255)
    }
    col = custom_palette
    if (identical(figure_type, "png")) {
        CairoPNG(image_out, width = figure_width, height = figure_height, 
            pointsize = 12, res = fiure_res, units = "px")
    }
    if (identical(figure_type, "jpg")) {
        CairoJPEG(image_out, quality = 100, width = figure_width, 
            height = figure_height, res = figure_res, units = "px")
    }
    if (identical(figure_type, "pdf")) {
        CairoPDF(file = image_out, width = figure_width, height = figure_height, 
            res = fiure_res, units = "px")
    }
    if (identical(figure_type, "ps")) {
        CairoPS(file = image_out, width = figure_width, height = figure_height, 
            res = fiure_res, units = "px")
    }
    if (trace == "none") {
        key_lines = 0
    }
    scale01 <- function(x, low = min(x), high = max(x)) {
        x <- (x - low)/(high - low)
        x
    }
    retval <- list()
    scale <- if (symm && missing(scale)) 
        "none"
    else match.arg(scale)
    dendrogram <- match.arg(dendrogram)
    trace <- match.arg(trace)
    density.info <- match.arg(density.info)
    if (length(col) == 1 && is.character(col)) 
        col <- get(col, mode = "function")
    if (!missing(breaks) && (scale != "none")) 
        warning("Using scale=\"row\" or scale=\"column\" when breaks are", 
            "specified can produce unpredictable results.", "Please consider using only one or the other.")
    if (is.null(Rowv) || is.na(Rowv)) 
        Rowv <- FALSE
    if (is.null(Colv) || is.na(Colv)) 
        Colv <- FALSE
    else if (Colv == "Rowv" && !isTRUE(Rowv)) 
        Colv <- FALSE
    if (length(di <- dim(x)) != 2 || !is.numeric(x)) 
        stop("x must be a numeric matrix")
    nr <- di[1]
    nc <- di[2]
    if (nr <= 1 || nc <= 1) 
        stop("'x' must have at least 2 rows and 2 columns")
    if (!is.numeric(margins) || length(margins) != 2) 
        stop("'margins' must be a numeric vector of length 2")
    if (missing(cellnote)) 
        cellnote <- matrix("", ncol = ncol(x), nrow = nrow(x))
    if (!inherits(Rowv, "dendrogram")) {
        if (((!isTRUE(Rowv)) || (is.null(Rowv))) && (dendrogram \%in\% 
            c("both", "row"))) {
            if (is.logical(Colv) && (Colv)) 
                dendrogram <- "column"
            else dedrogram <- "none"
            warning("Discrepancy: Rowv is FALSE, while dendrogram is `", 
                dendrogram, "'. Omitting row dendogram.")
        }
    }
    if (!inherits(Colv, "dendrogram")) {
        if (((!isTRUE(Colv)) || (is.null(Colv))) && (dendrogram \%in\% 
            c("both", "column"))) {
            if (is.logical(Rowv) && (Rowv)) 
                dendrogram <- "row"
            else dendrogram <- "none"
            warning("Discrepancy: Colv is FALSE, while dendrogram is `", 
                dendrogram, "'. Omitting column dendogram.")
        }
    }
    if (inherits(Rowv, "dendrogram")) {
        ddr <- Rowv
        rowInd <- order.dendrogram(ddr)
    }
    else if (is.integer(Rowv)) {
        hcr <- hclustfun(distfun(x), method = hclustfun_method)
        ddr <- as.dendrogram(hcr)
        ddr <- reorder(ddr, Rowv)
        rowInd <- order.dendrogram(ddr)
        if (nr != length(rowInd)) 
            stop("row dendrogram ordering gave index of wrong length")
    }
    else if (isTRUE(Rowv)) {
        Rowv <- rowMeans(x, na.rm = na.rm)
        hcr <- hclustfun(distfun(x), method = hclustfun_method)
        ddr <- as.dendrogram(hcr)
        ddr <- reorder(ddr, Rowv)
        rowInd <- order.dendrogram(ddr)
        if (nr != length(rowInd)) 
            stop("row dendrogram ordering gave index of wrong length")
    }
    else {
        rowInd <- nr:1
    }
    if (inherits(Colv, "dendrogram")) {
        ddc <- Colv
        colInd <- order.dendrogram(ddc)
    }
    else if (identical(Colv, "Rowv")) {
        if (nr != nc) 
            stop("Colv = \"Rowv\" but nrow(x) != ncol(x)")
        if (exists("ddr")) {
            ddc <- ddr
            colInd <- order.dendrogram(ddc)
        }
        else colInd <- rowInd
    }
    else if (is.integer(Colv)) {
        hcc <- hclustfun(distfun(if (symm) 
            x
        else t(x)), method = hclustfun_method)
        ddc <- as.dendrogram(hcc)
        ddc <- reorder(ddc, Colv)
        colInd <- order.dendrogram(ddc)
        if (nc != length(colInd)) 
            stop("column dendrogram ordering gave index of wrong length")
    }
    else if (isTRUE(Colv)) {
        Colv <- colMeans(x, na.rm = na.rm)
        hcc <- hclustfun(distfun(if (symm) 
            x
        else t(x)), method = hclustfun_method)
        ddc <- as.dendrogram(hcc)
        ddc <- reorder(ddc, Colv)
        colInd <- order.dendrogram(ddc)
        if (nc != length(colInd)) 
            stop("column dendrogram ordering gave index of wrong length")
    }
    else {
        colInd <- 1:nc
    }
    retval$rowInd <- rowInd
    retval$colInd <- colInd
    retval$call <- match.call()
    x <- x[rowInd, colInd]
    x.unscaled <- x
    cellnote <- cellnote[rowInd, colInd]
    if (is.null(labRow)) 
        labRow <- if (is.null(rownames(x))) 
            (1:nr)[rowInd]
        else rownames(x)
    else labRow <- labRow[rowInd]
    if (is.null(labCol)) 
        labCol <- if (is.null(colnames(x))) 
            (1:nc)[colInd]
        else colnames(x)
    else labCol <- labCol[colInd]
    if (scale == "row") {
        retval$rowMeans <- rm <- rowMeans(x, na.rm = na.rm)
        x <- sweep(x, 1, rm)
        retval$rowSDs <- sx <- apply(x, 1, sd, na.rm = na.rm)
        x <- sweep(x, 1, sx, "/")
    }
    else if (scale == "column") {
        retval$colMeans <- rm <- colMeans(x, na.rm = na.rm)
        x <- sweep(x, 2, rm)
        retval$colSDs <- sx <- apply(x, 2, sd, na.rm = na.rm)
        x <- sweep(x, 2, sx, "/")
    }
    if (missing(breaks) || is.null(breaks) || length(breaks) < 
        1) {
        if (missing(col) || is.function(col)) 
            breaks <- 16
        else breaks <- length(col) + 1
    }
    if (length(breaks) == 1) {
        if (!symbreaks) 
            breaks <- seq(min(x, na.rm = na.rm), max(x, na.rm = na.rm), 
                length = breaks)
        else {
            extreme <- max(abs(x), na.rm = TRUE)
            breaks <- seq(-extreme, extreme, length = breaks)
        }
    }
    nbr <- length(breaks)
    ncol <- length(breaks) - 1
    if (class(col) == "function") 
        col <- col(ncol)
    min.breaks <- min(breaks)
    max.breaks <- max(breaks)
    x[x < min.breaks] <- min.breaks
    x[x > max.breaks] <- max.breaks
    if (missing(lhei) || is.null(lhei)) 
        lhei <- c(keysize, 4)
    if (missing(lwid) || is.null(lwid)) 
        lwid <- c(keysize, 4)
    if (missing(lmat) || is.null(lmat)) {
        lmat <- rbind(4:3, 2:1)
        if (!missing(ColSideColors)) {
            if (!is.character(ColSideColors) || length(ColSideColors) != 
                nc) 
                stop("'ColSideColors' must be a character vector of length ncol(x)")
            lmat <- rbind(lmat[1, ] + 1, c(NA, 1), lmat[2, ] + 
                1)
            lhei <- c(lhei[1], 0.2, lhei[2])
        }
        if (!missing(RowSideColors)) {
            if (!is.character(RowSideColors) || length(RowSideColors) != 
                nr) 
                stop("'RowSideColors' must be a character vector of length nrow(x)")
            lmat <- cbind(lmat[, 1] + 1, c(rep(NA, nrow(lmat) - 
                1), 1), lmat[, 2] + 1)
            lwid <- c(lwid[1], 0.2, lwid[2])
        }
        lmat[is.na(lmat)] <- 0
    }
    if (length(lhei) != nrow(lmat)) 
        stop("lhei must have length = nrow(lmat) = ", nrow(lmat))
    if (length(lwid) != ncol(lmat)) 
        stop("lwid must have length = ncol(lmat) =", ncol(lmat))
    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    layout(lmat, widths = lwid, heights = lhei, respect = FALSE)
    if (!missing(RowSideColors)) {
        par(mar = c(margins[1], 0, 0, 0.5))
        image(rbind(1:nr), col = RowSideColors[rowInd], axes = FALSE)
    }
    if (!missing(ColSideColors)) {
        par(mar = c(0.5, 0, 0, margins[2]))
        image(cbind(1:nc), col = ColSideColors[colInd], axes = FALSE)
    }
    par(mar = c(margins[1], 0, 0, margins[2]))
    x <- t(x)
    cellnote <- t(cellnote)
    if (revC) {
        iy <- nr:1
        if (exists("ddr")) 
            ddr <- rev(ddr)
        x <- x[, iy]
        cellnote <- cellnote[, iy]
    }
    else iy <- 1:nr
    image(1:nc, 1:nr, x, xlim = 0.5 + c(0, nc), ylim = 0.5 + 
        c(0, nr), axes = FALSE, xlab = "", ylab = "", col = col, 
        breaks = breaks, ...)
    retval$carpet <- x
    if (exists("ddr")) 
        retval$rowDendrogram <- ddr
    if (exists("ddc")) 
        retval$colDendrogram <- ddc
    retval$breaks <- breaks
    retval$col <- col
    if (!invalid(na.color) & any(is.na(x))) {
        mmat <- ifelse(is.na(x), 1, NA)
        image(1:nc, 1:nr, mmat, axes = FALSE, xlab = "", ylab = "", 
            col = na.color, add = TRUE)
    }
    axis(1, 1:nc, labels = labCol, las = 2, line = -0.5, tick = 0, 
        cex.axis = cexCol)
    if (!is.null(xlab)) 
        mtext(xlab, side = 1, line = margins[1] - 1.25)
    axis(4, iy, labels = labRow, las = 2, line = -0.5, tick = 0, 
        cex.axis = cexRow)
    if (!is.null(ylab)) 
        mtext(ylab, side = 4, line = margins[2] - 1.25)
    if (!missing(add.expr)) 
        eval(substitute(add.expr))
    if (!missing(colsep)) 
        for (csep in colsep) rect(xleft = csep + 0.5, ybottom = rep(0, 
            length(csep)), xright = csep + 0.5 + sepwidth[1], 
            ytop = rep(ncol(x) + 1, csep), lty = 1, lwd = 1, 
            col = sepcolor, border = sepcolor)
    if (!missing(rowsep)) 
        for (rsep in rowsep) rect(xleft = 0, ybottom = (ncol(x) + 
            1 - rsep) - 0.5, xright = nrow(x) + 1, ytop = (ncol(x) + 
            1 - rsep) - 0.5 - sepwidth[2], lty = 1, lwd = 1, 
            col = sepcolor, border = sepcolor)
    min.scale <- min(breaks)
    max.scale <- max(breaks)
    x.scaled <- scale01(t(x), min.scale, max.scale)
    if (trace \%in\% c("both", "column")) {
        retval$vline <- vline
        vline.vals <- scale01(vline, min.scale, max.scale)
        for (i in colInd) {
            if (!is.null(vline)) {
                abline(v = i - 0.5 + vline.vals, col = linecol, 
                  lty = 2)
            }
            xv <- rep(i, nrow(x.scaled)) + x.scaled[, i] - 0.5
            xv <- c(xv[1], xv)
            yv <- 1:length(xv) - 0.5
            lines(x = xv, y = yv, lwd = 1, col = tracecol, type = "s")
        }
    }
    if (trace \%in\% c("both", "row")) {
        retval$hline <- hline
        hline.vals <- scale01(hline, min.scale, max.scale)
        for (i in rowInd) {
            if (!is.null(hline)) {
                abline(h = i + hline, col = linecol, lty = 2)
            }
            yv <- rep(i, ncol(x.scaled)) + x.scaled[i, ] - 0.5
            yv <- rev(c(yv[1], yv))
            xv <- length(yv):1 - 0.5
            lines(x = xv, y = yv, lwd = 1, col = tracecol, type = "s")
        }
    }
    if (!missing(cellnote)) 
        text(x = c(row(cellnote)), y = c(col(cellnote)), labels = c(cellnote), 
            col = notecol, cex = notecex)
    par(mar = c(margins[1], 0, 0, 0))
    if (dendrogram \%in\% c("both", "row")) {
        plot(ddr, horiz = TRUE, axes = FALSE, yaxs = "i", leaflab = "none")
    }
    else plot.new()
    par(mar = c(0, 0, if (!is.null(main)) 5 else 0, margins[2]))
    if (dendrogram \%in\% c("both", "column")) {
        plot(ddc, axes = FALSE, xaxs = "i", leaflab = "none")
    }
    else plot.new()
    if (!is.null(main)) 
        title(main, cex.main = 0.9 * op[["cex.main"]])
    if (key) {
        par(mar = c(5, 4, 2, 1), cex = 0.75)
        tmpbreaks <- breaks
        if (symkey) {
            max.raw <- max(abs(c(x, breaks)), na.rm = TRUE)
            min.raw <- -max.raw
            tmpbreaks[1] <- -max(abs(x))
            tmpbreaks[length(tmpbreaks)] <- max(abs(x))
        }
        else {
            min.raw <- min(x, na.rm = TRUE)
            max.raw <- max(x, na.rm = TRUE)
        }
        z <- seq(min.raw, max.raw, length = length(col))
        image(z = matrix(z, ncol = 1), col = col, breaks = tmpbreaks, 
            xaxt = "n", yaxt = "n")
        par(usr = c(0, 1, 0, 1))
        lv <- pretty(breaks)
        xv <- scale01(as.numeric(lv), min.raw, max.raw)
        axis(1, at = xv, labels = lv)
        if (scale == "row") 
            mtext(side = 1, "Row Z-Score", line = 2)
        else if (scale == "column") 
            mtext(side = 1, "Column Z-Score", line = 2)
        else mtext(side = 1, key_xlabel, line = 2)
        if (density.info == "density") {
            dens <- density(x, adjust = densadj, na.rm = TRUE)
            omit <- dens$x < min(breaks) | dens$x > max(breaks)
            dens$x <- dens$x[-omit]
            dens$y <- dens$y[-omit]
            dens$x <- scale01(dens$x, min.raw, max.raw)
            if (key_lines > 0) {
                lines(dens$x, dens$y/max(dens$y) * 0.95, col = denscol, 
                  lwd = 1)
                axis(2, at = pretty(dens$y)/max(dens$y) * 0.95, 
                  pretty(dens$y))
            }
            title(key_text)
            par(cex = key_text_cex)
            mtext(side = 2, "Density", line = 2)
        }
        else if (density.info == "histogram") {
            h <- hist(x, plot = FALSE, breaks = breaks)
            hx <- scale01(breaks, min.raw, max.raw)
            hy <- c(h$counts, h$counts[length(h$counts)])
            if (key_lines > 0) {
                lines(hx, hy/max(hy) * 0.95, lwd = 1, type = "s", 
                  col = denscol)
                axis(2, at = pretty(hy)/max(hy) * 0.95, pretty(hy))
            }
            title(key_text)
            par(cex = key_text_cex)
            mtext(side = 2, key_ylabel, line = 2)
        }
        else title(key_text)
    }
    else plot.new()
    retval$colorTable <- data.frame(low = retval$breaks[-length(retval$breaks)], 
        high = retval$breaks[-1], color = retval$col)
    invisible(retval)
    dev.off()
    Row_labels_file = gsub(" ", "", paste(file_in, ".HD.Row_labels.txt"))
    for (i in (dim(data.matrix(labRow))[1]):1) {
        write(labRow[i], file = Row_labels_file, append = TRUE)
    }
    Col_labels_file = gsub(" ", "", paste(file_in, ".HD.Col_labels.txt"))
    for (j in 1:(dim(data.matrix(labCol))[1])) {
        write(labCol[j], file = Col_labels_file, append = TRUE)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
