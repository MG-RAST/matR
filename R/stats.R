
############################################
### STATISTICAL AND GRAPHICAL ROUTINES
############################################

# These functions operate on an abundance object.
# They (1) calculate and/or (2) produce images.
# They are designed to be convenient for both interactive 
# use and for scripting.
# Each accepts input in the form of either a filename or object.
# Each returns an object, displays to a graphics window, and/or writes
# to a file (in which case the filename(s) is returned).
# Graphics are written to file in .pdf, .png, .jpeg, or .ps formats,
# automatically detected by filename extension, default .png.
# width, height, etc are specified in pixels except in inches for .ps output.
# matrices or data.frames are written to file in a standard text format.
# Others objects are written as binary using "save".
# In each case, calling with no arguments in equivalent to 
# calling help for the function.

mPermutations <- function (x, ntimes = 1, type = "sample", toFile = NULL, verbose = FALSE) {
	M <- fsUnwrap (x)
	m <- nrow (M)
	n <- ncol (M)
	P <- matrix (nrow = m, ncol = n)
	rownames (P) <- rownames (M)
	colnames (P) <- colnames (M)

	totalSum <- base::sum (M)
	for (j in 1:ntimes) {
		switch (type,
# shuffle values within each sample;
# distributions are maintained within each sample.
			"sample" = { for (k in 1:n) P[,k] <- sample (M [,k]) },
# shuffle values across the whole matrix;
# distribution is maintained across the matrix, but not within samples.
			"dataset" = { P <- matrix (sample (as.vector (M)), nrow = m, ncol = n, dimnames = list (rownames (M), colnames (M))) },
# total sum of matrix entries is randomly redistributed; 
# should lose the sample and data set distributions.
			"complete" = {
				P <- 0
				redistrib <- table (sample ( 1:(m*n), size = totalSum, replace = TRUE ))
				P [as.numeric (names (redistrib))] <- redistrib
				dim (P) <- c (m, n) ; dimnames (P) <- list (rownames (M), colnames (M))
				}
			)
#		if (verbose) sum_rand_data = base::sum(rand_data); verbose_report(k, sum_data, sum_rand_data, rand_data)
#		if (! is.null (toFile)) write_files (perm_dir, file_name, rand_data, k)
#		fsWrap (P, toFile [j])
		}
# !!! incomplete thing: how to write to file, how to return multiple permutations?
	P
	}

mNormalize <- function (x, toFile = NULL) {
	M <- fsUnwrap (x)
	M [is.na (M)] <- 0
# log scale
	M <- log2 (M + 1)
# then scale by mean and standard deviation per sample
	mu <- colMeans (M)
	sigm <- unlist (sapply (as.data.frame (M), sd))
	M <- t ((t (M) - mu) / sigm)
# then scale to [0,1] across all samples
	shift <- min (M)
	scale <- max (M) - shift
	if (scale != 0) M <- (M - shift) / scale
	fsWrap (M, toFile)
	}

# check for valid significance test
# why "as.real"?
# is ok to run make_two_groups routine even for ANOVA and KW, which do not use it?
mStats <- function (x, groups,
					sig_test = c ("t-test-paired", "Wilcoxon-paired", "t-test-un-paired", "Mann-Whitney_un-paired-Wilcoxon", "ANOVA-one-way", "Kruskal-Wallis"),
					toFile = NULL) {
	suppressPackageStartupMessages (require (stats))

# if "groups" is length one, we assume it is the name 
# of a tab-separated file of group names
	if (length (groups) == 1) groups <- scan (file = groups, what = "character", nlines = 1, sep = "\t", quiet = TRUE)
	M <- fsUnwrap (x)

	m = nrow (M)
	n = ncol (M)
	ngroups <- nlevels (as.factor (groups))

	results <- matrix (0, m, ngroups + 2)
	rownames (results) <- rownames (M)
	colnames (results) <- c (paste ("group_(", levels (as.factor (groups)), ")_stddev", sep = ""),
								paste (sig_test, "_stat", sep = ""),
								paste (sig_test, "_p_value", sep = ""))
	for (j in 1:m) {
		oneRow <- data.frame (values = M [j,], group = groups, stringsAsFactors = TRUE)
		for (k in 1:ngroups) results [j,k] <- sd (subset (oneRow, group == levels( as.factor (groups)) [k] ) $ values)
		group1 <- subset (oneRow, subset = (group == levels (as.factor (groups)) [1])) $ values
		group2 <- subset (oneRow, subset = (group == levels (as.factor (groups)) [2])) $ values
		results [matrix ( c (j, ngroups + 1, j, ngroups + 2), nrow = 2, ncol = 2, byrow = TRUE) ] <-
			as.real (switch (sig_test,
				"t-test-un-paired" =
					t.test (group1, group2) [c ("statistic", "p.value")],
				"t-test-paired" =
					t.test (group1, group2, paired = TRUE) [c ("statistic", "p.value")],
				"Mann-Whitney_un-paired-Wilcoxon" =
					wilcox.test (group1, group2, exact = TRUE) [c ("statistic", "p.value")],
				"Wilcoxon-paired" =
					wilcox.test (group1, group2, exact = TRUE, paired = TRUE) [c ("statistic", "p.value")],
				"Kruskal-Wallis" =
					kruskal.test (oneRow$values, oneRow$group) [c ("statistic", "p.value")],
				"ANOVA-one-way" = {
					a <- anova (aov (values ~ group, data = oneRow))  [c ("F value", "Pr(>F)")]
					c (a ["F value"] [1,1], a ["Pr(>F)"] [1,1])
					}))

		}
	fsWrap (results, toFile)
	}

# this routine is modified to boxplot any number of samples for mutual comparison
# x is expected to be a series of abundance matrices, which will be plotted together
# just shows some boxplots with nice formatting
mBoxplot <- function (x, titles, imgFile = NULL, figure_width = 950, figure_height = 1000, figure_res = NA) {
	suppressPackageStartupMessages (require (Cairo))

	fsUnwrap (x)
	if (! is.null (imgFile)) Cairo (figure_width, figure_height, imgFile, grType (imgFile), pointsize = 12, res = figure_res , units = "px")
	else quartz ()
	split.screen (c (2,1))
	screen (1); boxplot (x, titles, las = 2)
#    screen (2); boxplot (log2_cent_data, main = centered_data_boxplot_title, las = 2)
	if (! is.null (imgFile)) dev.off ()
	}
	
# inputs: matrix
# returns: dist
# implements various distances.  transpose, since we go by columns.
mDist <- function (M, method = "bray-curtis") {
	suppressPackageStartupMessages (require (ecodist))
	if (any (method == c ("bray-curtis", "jaccard", "mahalanobis", "sorensen", "difference")))
		distance (t (M), method = method) 
	else dist (t (M), method = method)
	}

# inputs: filename or matrix
# returns: filename, or list of "values" (numeric), "vectors" (matrix), "dist" (dist)
# compute distance, scaled eigenvalues, and eigenvectors
# ... consider better way to combine?  c, cbind, data.matrix ...
mPCO <- function (x, method = "bray-curtis", toFile = NULL, distFile = NULL) {
	suppressPackageStartupMessages (require (ecodist))

	M <- fsUnwrap (x)
	D <- mDist (M, method)
	P <- pco (D)
	scaled <- P$values / sum (P$values)
	names (scaled) <- paste ("PCO", 1:length(scaled), sep = "")
# should both rows and columns be labeled, here?
	dimnames (P$vectors) [[1]] <- dimnames (M) [[2]]
	dimnames (P$vectors) [[2]] <- dimnames (M) [[2]]
	fsWrap (list (values = scaled, vectors = P$vectors, dist = D), toFile)
	}

# inputs: filename or matrix
# returns: filename or pcaRes
mPCA <- function (x, n, toFile = NULL) {
	suppressPackageStartupMessages (require (pcaMethods))

	M <- fsUnwrap (x)
	P <- pca (M, nPcs = n)
	fsWrap (P, toFile)
	}

# inputs: filename or matrix
# returns: filename or NULL
# if no output file specified, attempts to display image
# pch, integer values that indicate different point types (19 is a filled circle)
mPlotPCA <- function (x, n, toFile = NULL, imgFile = NULL) {
	suppressPackageStartupMessages (require (pcaMethods))
	suppressPackageStartupMessages (require (Cairo))

# in the original these are parameters to the function
	PC1="PC1"; PC2="PC2"; image_title = "Principal Component Analysis"; figure_width = 700; figure_height = 700
	points_color = "orange"; figure_res = NA; lab_cex= 1; axis_cex = 1; points_text_cex = .8

	M <- fsUnwrap (x)
	P <- mPCA (M, n)
	if (! is.null (imgFile)) Cairo (figure_width, figure_height, imgFile, grType (imgFile), pointsize = 12, res = figure_res, units = "px")
	else quartz ()

	plot (P@loadings [,PC1], P@loadings [,PC2], cex.axis = axis_cex, cex.lab = lab_cex,
		main = image_title, type = "p", col = points_color,
		xlab = paste (PC1, "R^2 =", round (P@R2 [PC1], 4)),
		ylab = paste (PC2, "R^2 =", round (P@R2 [PC2], 4)))
	points (P@loadings [,PC1], P@loadings [,PC2], col = points_color, pch = 19, cex = 2 )
	text (P@loadings [,PC1], P@loadings [,PC2], labels = rownames (P@loadings), cex = points_text_cex)

# need to concatenate two tables into single object
	if (! is.null (toFile)) {
		write.table (P@R2,	file = toFile, sep = "\t", col.names = FALSE, row.names = TRUE, append = FALSE)
#		write.table (P (my_pcaRes), file = toFile, sep = "\t", col.names = FALSE, row.names = TRUE, append = TRUE)
		}
	if (! is.null (imgFile)) dev.off (dev.cur ())
	}

mSuggestTest <- function (x, groups, dataType = "raw", paired = FALSE, toFile = NULL) {
	M <- fsUnwrap (x)
	n <- dim (M) [1]
# if "groups" is length one, we assume it is the name 
# of a tab-separated file of group names
	if (length (groups) == 1) groups <- scan (file = groups, what = "character", nlines = 1, sep = "\t", quiet = TRUE)
	ngroups <- nlevels (as.factor (groups))

	if (n != dim (M) [2] || n < 3 || ngroups < 2  || !oneof (dataType, "raw", "normalized")) {
		result <- list (test = "none", note = "no statistical test could be selected")
		if (is.null (toFile)) return (result)
		writeLines (c (result$test, result$note), toFile)
		return (toFile)
		}
    special <- 1 %in% table (groups) [1:2]

	criteria <- list ( type = c ("raw", "normalized"), ngroups = c ("two", "morethantwo"),
						paired = c ("paired", "unpaired"), special = c ("specialcase", "notspecialcase"))
	shape <- integer (length (criteria))
	for (j in 1:length (shape)) shape [j] <- length (criteria [[j]])
	tests <- array (dim = shape, dimnames = criteria)
	notes <- array (dim = shape, dimnames = criteria)

	tests ["normalized", "two", "paired", "specialcase"] <- "ANOVA_repeat_measures (not yet supported: you could try ANOVA-one-way, note that it assumes independent measures)"
	tests ["normalized", "two", "paired", "notspecialcase"] <- "t-test-paired"
	tests ["normalized", "two", "unpaired", "notspecialcase"] <- "t-test-un-paired"
	tests ["normalized", "two", "unpaired", "specialcase"] <- "ANOVA-one-way"
	tests ["normalized", "morethantwo", "paired",] <- "ANOVA-repeat-measures"
	tests ["normalized", "morethantwo", "unpaired",] <- "ANOVA-one-way"
	tests ["raw", "two", "paired","specialcase"] <- "WilcoMon-paired"
	tests ["raw", "two", "paired","notspecialcase"] <- "WilcoMon-paired"
	tests ["raw", "two", "unpaired","specialcase"] <- "Mann-Whitney_un-paired_WilcoMon"
	tests ["raw", "two", "unpaired","notspecialcase"] <- "Mann-Whitney_un-paired_WilcoMon"
	tests ["raw", "morethantwo", "paired",] <- "Friedman-test"
	tests ["raw", "morethantwo", "unpaired",] <- "Kruskal-Wallis"

	notes ["normalized", "two", "paired", "specialcase"] <- "one of the two groups has just a single measure.  t-test cannot be used because it requires at least 2 measures per group.  The suggested ANOVA \"can\" be used, but statistical power is likely to be very low." 
	notes ["normalized", "two", "paired", "notspecialcase"] <- "none"
	notes ["normalized", "two", "unpaired", "notspecialcase"] <- "none"
	notes ["normalized", "two", "unpaired", "specialcase"] <- "one of the two groups has just a single measure.  t-test cannot be used because it requires at least 2 measures per group.  The suggested ANOVA \"can\" be used, but statistical power is likely to be very low." 
	notes ["normalized", "morethantwo", "paired",] <- "not currently supported - you may be able to try ANOVA-one-way, note that it assumes independent measures"
	notes ["normalized", "morethantwo", "unpaired",] <- "none"
	notes ["raw", "two", "paired","specialcase"] <- "one of the two groups has just a single sample; statistical power is likley to be very low"
	notes ["raw", "two", "paired","notspecialcase"] <- "none"
	notes ["raw", "two", "unpaired","specialcase"] <- "one of the two groups has just a single sample; statistical power is likley to be very low"
	notes ["raw", "two", "unpaired","notspecialcase"] <- "this test is also known as the Mann-Whitney U test, the Mann-Whitney_WilcoMon test, or the WilcoMon rank-sum test"
	notes ["raw", "morethantwo", "paired",] <- "not yet supported: you could try an ANOVA-one-way on the normalized data, note that ANOVA-one-way assumes independent measures" 
	notes ["raw", "morethantwo", "unpaired",] <- "none"

	index <- matrix (c (dataType, if (ngroups == 2) "two" else "morethantwo", 
						if (paired) "paired" else "unpaired", if (special) "specialcase" else "notspecialcase"), 1, 4)
	result <- list (test = tests [index], note = notes [index])
	if (is.null (toFile)) return (result)
	writeLines (c (result$test, result$note), toFile)
	return (toFile)
	}

mDendrogram <- function (
	imgFile, toFile, 
	file_in, file_out_column = "col_clust", file_out_row    =  "row_clust",
	dist_method           = "euclidean", # ("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
	clust_method          = "ward",  # ("ward", "single", "complete", "average", "mcquitty", "median", "centroid")
	produce_figures       = FALSE,
	col_dendrogram_width  = 950, col_dendrogram_height = 500,
	row_dendrogram_width  = 950, row_dendrogram_height = 500,
	output_files_prefix   = "my_dendrograms") {
#	if (! is.null (imgFile)) Cairo (figure_width, figure_height, toFile, grType (imgFile), pointsize = 12, res = figure_res , units = "px")
#	else quartz ()
	if (! is.null (imgFile)) dev.off (dev.cur ())
	}

mHeatmap <- function (...) {
#	if (! is.null (imgFile)) Cairo (figure_width, figure_height, toFile, grType (imgFile), pointsize = 12, res = figure_res , units = "px")
#	else quartz ()
# will not change this routine much
#	if (! is.null (imgFile)) dev.off (dev.cur ())
	}


############################################
### backward-compatible wrappers for 
### existing production scripts
###
### these provide the stats functions prototyped 
### (almost) exactly as Kevin initially implemented
### them but now driven by the routines above
############################################

MGRAST_do_stats <- function (data_file, groups_file, data_type = c ("raw", "normalized"),
							sig_test = c ("t-test-paired", "Wilcoxon-paired", "t-test-un-paired", "Mann-Whitney_un-paired-Wilcoxon", "ANOVA-one-way", "Kruskal-Wallis"),
							file_out) {
	mStats (data_file, groups_file, sig_test, file_out)
	}

MGRAST_plot_pca <- function (file_in, file_out = "my_pca", num_PCs = 2, produce_fig = FALSE, PC1="PC1", PC2="PC2", image_out = "my_pca", 
							image_title = image_out, figure_width = 950, figure_height = 950, points_color = "red", 
							figure_res = NA, lab_cex = 1, axis_cex = 1, points_text_cex = .8) {
	if (! produce_fig) image_out <- NULL
	mPlotPCA (file_in, num_PCs, file_out, image_out)
	}

MGRAST_plot_pco <- function (file_in, file_out = "my_PCoA", dist_method = "bray-curtis", headers = 0) {
	P <- mPCO (file_in, dist_method)
	if (as.logical (headers)) write (file = file_out, paste (
			"# file_in    :\n", file_in,
			"# dist_method:\n", dist_method,
			"#________________________________\n",
			"# EIGEN VALUES (scaled 0 to 1) >\n",
			"#________________________________"))
	write.table (P$values, file = file_out, col.names = FALSE, row.names = TRUE, append = as.logical (headers), sep = "\t")
	if (as.logical (headers)) write (file = file_out, paste (
		"#________________________________\n",
		"# EIGEN VECTORS >\n",
		"#________________________________"), append = TRUE)
	write.table (P$vectors, file = file_out, col.names = FALSE, row.names = TRUE, append = TRUE, sep = "\t")
	}

MGRAST_preprocessing <- function (file_in, file_out = "preprocessed_data", produce_fig = FALSE, image_out = "my_boxplots",
	raw_data_boxplot_title = "raw data", centered_data_boxplot_title = "log2(x+1) & centered per sample, scaled 0 to 1 over all samples",
	figure_width = 950, figure_height = 1000, figure_res = NA) {
	mNormalize (file_in, file_out)
#	if (produce_fig) mBoxPlot (...)
	}

# verbose option not implemented
sample_matrix <- function (file_name, num_perm = 1, perm_type = "sample_rand", write_files = FALSE, perm_dir = "./permutations/", 
							verbose = FALSE, debug = FALSE ) {
	toFile <- if (write_files) paste (perm_dir, file_name, ".permutation.", sep = "") else NULL
	type <- strsplit (perm_type, "_", fixed = TRUE) [[1]] [1]
	for (j in 1:num_perm) P <- mPermutations (file_name, 1, type, if (is.null (toFile)) toFile else paste (toFile, j, sep = ""))
	}

# a defect here: the number of samples and of groups can 
# not be reported as previously, due to the restructuring
# of functions
MGRAST_suggest_test <- function (data_file, groups_file, data_type = c ("raw", "normalized"), paired = FALSE, 
								file_out = "suggest_stat_test.out") {
	result <- mSuggestTest (data_file, groups_file, data_type, paired, NULL)
	if (result$test == "none") writeLines (paste (
		"Minimum analysis requirements not met - no test could be selected:",
		"\ndata_type   =\t", data_type,
		"\npaired      =\t", paired,
		"\nnum_samples =\t", # ... missing here ...
		"\nnum_groups  =\t", # ... missing here ...
		"\ntest_notes  =\tnone",
		"\nSingle sample analyses are not supported.",
		"\nThe minimum analysis requirements are:",
		"\n     (1) at least two groups of samples(metagenomes)",
		"\n     (2) at least one group with two or more samples.", sep = ""), file_out)
	else writeLines (paste (result$test,
		"\ndata-type         =\t", data_type,
		"\npaired            =\t", paired,
		"\nnum-samples       =\t", # ... missing here ...
		"\nnum-groups        =\t", # ... missing here ...
		"\ntest_notes        =\t", result$notes, sep = ""), file_out)
	}

MGRAST_dendrograms <- function (
	file_in,
	file_out_column = "col_clust", file_out_row = "row_clust",
	dist_method = "euclidean",
	clust_method = "ward",
	produce_figures = FALSE,
	col_dendrogram_width = 950, col_dendrogram_height = 500, row_dendrogram_width  = 950, row_dendrogram_height = 500,
	output_files_prefix = "my_dendrograms") {
	}

heatmap_dendrogram <- function (file_in,
# file_out,
	figure_type   = "png",                              # c("jpg" "pdf" "ps" or "png") # added this as an input argument 8-10-10
	image_out = gsub(" ", "", paste(file_in, ".HD.", figure_type)),

	image_title = image_out, # image_out
# removed "x" from comments to get clean build
 x,                                             # x = input_object that contains the data                 
# bells and whistles ...
	heat_color1="red",                             # two colors for the the gradient that will be created for the heatmap
	heat_color2="green",
	palette_n=12,
# key = FALSE,
	labRow = NULL,                                      # Kevin 1-27-10 - Dendrogram row labels (NA to remove)
	labCol = NULL,                                      # Kevin 1-27-10 - Dendrogram column labels (NA to remove)
# par (las=2 (labels perp to axis)
	hclustfun_method = "ward",            # hclustfun_method = c("ward", "single", "complete", "average", "mcquitty", "median" or "centroid")
# figure output parameters (units vary depending on selected figure_type (bleow)
	figure_width  = 1000,                               # usually pixels, inches if eps is selected; png is default
	figure_height = 1000,                               # usually pixels, inches if eps is selected; png is default
	figure_res    = NA,                                 # usually pixels, inches if eps is selected; png is default      
# figure_type   = "png",                              # c("jpg" "pdf" "ps" or "png") # added this as an input argument 8-10-10
# dendrogram control
	Rowv = TRUE,                                        # <--- Kevin 1-27-10 - FALSE, data are not hclust sorted by row
	Colv = if (symm) "Rowv" else TRUE,                  # <--- Kevin 1-27-10 - FALSE data are not hclust sorted by column
	distfun = dist,
	hclustfun = hclust,                                 # <------ Kevin 2-8-10 - forces "complete" method # made variable directly below 2-24-10
# hclustfun_method = "complete",       # hclustfun_method = c("ward", "single", "complete", "average", "mcquitty", "median" or "centroid") 
	dendrogram = "both",                                # dendrogram = c("both","row", "column", "none")
	symm = FALSE,
# data scaling
	scale = "none",                                     # scale = c("none", "row", "column")
	na.rm = TRUE,
# image plot
	revC = identical(Colv, "Rowv"),
	add.expr,
# mapping data to colors
	breaks,
	symbreaks = min(x < 0, na.rm = TRUE) || scale != "none",
# colors
	col = "heat.colors", # <------ Kevin 1-27-10 - MADE VARIABLE in loop below
# block separation
	colsep,
	rowsep, 
	sepcolor = "white",
	sepwidth = c(0.05, 0.05),
# cell labeling
	cellnote,
	notecex = 1, 
	notecol = "cyan",
	na.color = par("bg"),
# level trace
	trace = "none",                                     # trace = c("column", "row", "both", "none")
	tracecol = "cyan",
	hline = median(breaks), 
	vline = median(breaks),
	linecol = tracecol,
# Row/Column Labeling
	margins = c(5, 1),                                  ##### <------ Kevin 1-27-10 - specifcy the size of the margins
	ColSideColors,
	RowSideColors,
	row_lab_mult = 2, # <-----                          # used below to adjust font size of row labels - Kevin 3-9-10
	col_lab_mult = 3, # <-----                          # used below to adjust font size of column labels - Kevin 3-9-10
# added declarations of nr, nc for clean build
	nr, nc,
	cexRow = row_lab_mult*(1/log10(nr)),                # 0.1 + 1/log10(nr),  ##### <------ Kevin 1-27-10 (Dendogram row font size)  
	cexCol = col_lab_mult*(1/log10(nc)),                # 0.1 + 1/log10(nc),   ##### <------ Kevin 1-27-10 (Dendogram column font size)
# labRow = NULL,                                      # Kevin 1-27-10 - Dendrogram row labels (NA to remove)
# labCol = NULL,                                      # Kevin 1-27-10 - Dendrogram column labels (NA to remove)
# color key + density info
	key = FALSE,                                         # <------ Kevin 9-28-10 key scaling needs work
	keysize = .9,                                       ##### <------ Kevin 1-27-10 size of the key
	key_lines = 1,                                      # Kevin ADDED 1-27-10 0=no 1=yes for trace lines in the key (edited in loop below)
	key_text = "Key (min to max)",                      #\nand Histogram, # Kevin  1-27-10 - ADDED - MADE VARIABLE
	key_text_cex = 0.5,                                 # Kevin made this variable 4-7-10
	key_xlabel = NULL,                                  #"Value", # Kevin  1-27-10 - ADDED - MADE VARIABLE 
	key_ylabel = NULL,                                  #"Count", # Kevin  1-27-10 - ADDED - MADE VARIABLE
	density.info = c("histogram", "density", "none"),
	denscol = tracecol,                                 # <------ Kevin 1-27-10 - spcify color for key traceline
	symkey = min(x < 0, na.rm = TRUE) || symbreaks,
	densadj = 0.25,
# plot labels 
	xlab = NULL,
	ylab = NULL,
# plot layout
	lmat = NULL,
	lhei = NULL,                                        # <--- line height multiplier
	lwid = NULL, ...) {
	}


############################################
# These functions "wrap the file system" around
# matrix objects.  The idea is, in a standard way,
# to minimize the difference between working with 
# objects in memory and on disk (and maybe, on 
# the network).
# This idea is perhaps to be implemented by
# class methods, and for various classes other
# than matrix.
############################################

fsUnwrap <- function (x) {
	if (class (x) == "character") data.matrix (read.table (x, row.names= 1 , header = TRUE, sep = "\t", comment.char = "", quote = ""))
	else as.matrix (x)
	}

fsWrap <- function (x, toFile = NULL) {
	if (! is.null (toFile)) {
		if (class (x) == "matrix") write.table (x, file = toFile, sep = "\t", col.names = NA, row.names = TRUE, quote = FALSE)
		else save (x, file = toFile)
		toFile
		}
	else x
	}

oneof <- function (x, ...) any (x == unlist ( list (...)))

lastof <- function (x) { x [length (x)] }

grType <- function (fileName) {
	s <- lastof (strsplit (fileName, ".") [[1]])
	if (oneof (s, "jpeg", "pdf", "png", "ps")) s
	else "png"
	}
