
############################################
### statistical and graphical routines
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
# matrices or data.frames are written to file in a standard 
# text format.
# Other objects are written using "save" (in binary format).
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
			"dataset" = { P <- matrix (sample (as.vector (M)), nrow = m, ncol = n) },
# total sum of matrix entries is randomly redistributed; 
# should lose the sample and data set distributions.
			"complete" = {
				P <- 0
				redistrib <- table (sample ( 1:(m*n), size = totalSum, replace = TRUE ))
				P [as.numeric (names (redistrib))] <- redistrib
				}
			)
		if (verbose) sum_rand_data = base::sum(rand_data); verbose_report(k, sum_data, sum_rand_data, rand_data)
		if (! is.null (toFile)) write_files (perm_dir, file_name, rand_data, k)
		}
	M <- fsWrap (x)
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
	fsWrap (x, toFile)
	}

# check for valid significance test
# why "as.real"?
# is ok to run make_two_groups routine even for ANOVA and KW, which do not use it?
mStats <- function (x, groups, data_type = c ("raw", "normalized"),
					sig_test = c ("t-test-paired", "Wilcoxon-paired", "t-test-un-paired", "Mann-Whitney_un-paired-Wilcoxon", "ANOVA-one-way", "Kruskal-Wallis"),
					toFile = NULL) {
	suppressPackageStartupMessages (require (stats))
	suppressPackageStartupMessages (require (nlme))

### IMPORT AND FORMAT THE DATA ###
	M <- fsUnwrap (x)
	groups = data.matrix (scan (file = groups_file, what = "character", sep = "\t", quiet = TRUE)) [,1]
	m = nrow (M)
	n = ncol (M)
	ngroups <- nlevels (as.factor (groups))
# CREATE OUTPUT TABLE AND GIVE IT APPROPRIATE HEADERS
	output_table <- matrix (0, m, ngroups + 2)
	rownames (output_table) <- rownames (M)
	colnames (output_table) <- c (paste ("group_(", levels (as.factor (groups)), ")_stddev", sep = ""),
								paste (sig_test, "_stat", sep = ""),
								paste (sig_test, "_p_value", sep = ""))
### PERFORM THE ANALYSES
	for (j in 1:m) {
		thisrow <- data.frame (values = M [j,], ind = groups, stringsAsFactors = TRUE)

		for (k in 1:ngroups) {
			group_ind <- levels(row_data[,2])[k]
### CALCULATE THE STANDARD DEVIATION FOR EACH GROUP
			for(sample in 1:num_samples)
				if (thisrow [sample, 2] == group_ind)
					if (num_samples_in_group == 0)
						group_sample_counts <- thisrow [sample, 1]
					else
						group_sample_counts <- c (group_sample_counts, thisrow [sample, 1])
			output_table [j, k] <- as.real (sd (group_sample_counts))
			}
# this is the "make_two_groups" routine
		group_1 = levels(row_data[,2])[1] ; group_2 = levels(row_data[,2])[2]
		group_1_size = 0 ; group_2_size = 0
		for (i in 1:as.matrix(dim(row_data)[1])) {
			if(identical(as.character(row_data[i,2]), as.character(group_1)))
				group_1_size = group_1_size + 1
			if(identical(as.character(row_data[i,2]), as.character(group_2)))
				group_2_size = group_2_size + 1
			}
# integer type ok?  these are counts, right?
		group_1_data <- integer (group_1_size)
		group_2_data <- integer (group_2_size)
		group_1_index = 0 ; group_2_index = 0
		for (i in 1:as.matrix (dim (thisrow) [1])) {
			if (identical(as.character(thisrow [i,2]), as.character(group_1))) {
				group_1_index = group_1_index + 1
				group_1_data [group_1_index] <- thisrow [i,1]
				}
			if (identical(as.character(thisrow [i,2]), as.character(group_2))) {
				group_2_index = group_2_index + 1
				group_2_data [group_2_index] <- thisrow [i,1]
				}
			}
		output [matrix ( c (my_row, num_groups+1, my_row, num_groups+2), nrow = 2, ncol = 2, byrow = TRUE) ] <-
			as.real (switch (sig_test,
				"t-test-un-paired" =
					t.test (group_1_data, group_2_data) ["statistic", "p.value"],
				"t-test-paired" =
					t.test (group_1_data, group_2_data, paired = TRUE) ["statistic", "p.value"],
				"Mann-Whitney_un-paired-Wilcoxon" =
					wilcox.test (group_1_data, group_2_data, exact = TRUE) ["statistic", "p.value"],
				"Wilcoxon-paired" =
					wilcox.test (group_1_data, group_2_data, exact = TRUE, paired = TRUE) ["statistic", "p.value"],
				"Kruskal-Wallis" =
					kruskal.test (row_data [,1], row_data [,2]) ["statistic", "p.value"] ))
				"ANOVA-one-way" = {
					a <- anova (aov (values ~ ind, data = row_data))  ["F value", "Pr(>F)"]
					c (a ["F value"] [1,1], a ["Pr(>F)"] [1,1])
					}
		fsWrap (output_table, toFile)
		}
	}

# this routine is modified to boxplot any number of samples for mutual comparison
# x is expected to be a series of abundance matrices, which will be plotted together
# just shows some boxplots with nice formatting
mBoxplot <- function (x, titles, imgFile = NULL, figure_width = 950, figure_height = 1000, figure_res = NA) {
	suppressPackageStartupMessages (require (Cairo))

	fsUnwrap (x)
	if (! is.null (imgFile)) Cairo (figure_width, figure_height, toFile, grtype (imgFile), pointsize = 12, res = figure_res , units = "px")
	else quartz ()
	split.screen (c (2,1))
	screen (1); boxplot (x, main = raw_data_boxplot_title, las = 2)
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
	D <- mDist (M)
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
	if (! is.null (imgFile)) Cairo (figure_width, figure_height, imgFile, grtype (imgFile), pointsize = 12, res = figure_res, units = "px")
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
		write.table (P (my_pcaRes), file = toFile, sep = "\t", col.names = FALSE, row.names = TRUE, append = TRUE)
		}
	if (! is.null (imgFile)) dev.off (dev.cur ())
	}

mDendrogram <- function (
	file_in, file_out_column =  "col_clust", file_out_row    =  "row_clust",
	dist_method           = "euclidean", # ("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
	clust_method          = "ward",  # ("ward", "single", "complete", "average", "mcquitty", "median", "centroid")
	produce_figures       = FALSE,
	col_dendrogram_width  = 950, col_dendrogram_height = 500,
	row_dendrogram_width  = 950, row_dendrogram_height = 500,
	output_files_prefix   = "my_dendrograms") {
	if (! is.null (imgFile)) Cairo (figure_width, figure_height, toFile, grtype (imgFile), pointsize = 12, res = figure_res , units = "px")
	else quartz ()

	if (! is.null (imgFile)) dev.off (dev.cur ())
	}

mHeatmap <- function (...) {
	if (! is.null (imgFile)) Cairo (figure_width, figure_height, toFile, grtype (imgFile), pointsize = 12, res = figure_res , units = "px")
	else quartz ()
# will not change this routine much
	if (! is.null (imgFile)) dev.off (dev.cur ())
	}

mSuggestTest <- function (data_file, groups_file, data_type=c("raw", "normalized"), 
	paired = FALSE, file_out="suggest_stat_test.out") {
# simple to adapt
	}


############################################
### backward-compatible wrappers, mainly for 
#### existing production scripts
###
### these provide the stats functions prototyped 
### (almost) exactly as Kevin initially implemented
### them, but now they are wrappers for the
### routines as rewritten for general use, above
############################################

MGRAST_dendrograms <- function (
	file_in,
	file_out_column = "col_clust", file_out_row = "row_clust",
	dist_method = "euclidean",
	clust_method = "ward",
	produce_figures = FALSE,
	col_dendrogram_width = 950, col_dendrogram_height = 500, row_dendrogram_width  = 950, row_dendrogram_height = 500,
	output_files_prefix = "my_dendrograms") {
	}

MGRAST_do_stats <- function (
	data_file, groups_file,
	data_type = c ("raw", "normalized"),
	sig_test = c ("t-test-paired", "Wilcoxon-paired", "t-test-un-paired", "Mann-Whitney_un-paired-Wilcoxon", "ANOVA-one-way", "Kruskal-Wallis"),
	file_out) {
	}

MGRAST_plot_pca <- function (
	file_in, file_out = "my_pca",
	num_PCs = 2, produce_fig = FALSE, PC1="PC1", PC2="PC2",
	image_out = "my_pca", image_title = image_out,
	figure_width = 950, figure_height = 950, points_color = "red",
	figure_res = NA, lab_cex = 1, axis_cex = 1, points_text_cex = .8) {
	}

MGRAST_plot_pco <- function (
	file_in, file_out = "my_PCoA",
	dist_method = "bray-curtis",
	headers = 0) {
	}

MGRAST_preprocessing <- function(
	file_in, file_out = "preprocessed_data", produce_fig = FALSE, image_out = "my_boxplots",
	raw_data_boxplot_title = "raw data",
	centered_data_boxplot_title = "log2(x+1) & centered per sample, scaled 0 to 1 over all samples",
	figure_width = 950, figure_height = 1000, figure_res = NA) {
	}

MGRAST_suggest_test <- function (
	data_file, groups_file, 
	data_type = c ("raw", "normalized"), paired = FALSE, 
	file_out = "suggest_stat_test.out") {
	}

sample_matrix <- function(
	file_name, num_perm = 1, perm_type = "sample_rand", 
	write_files = FALSE, perm_dir = "./permutations/", verbose = FALSE, debug = FALSE ) {
	}

heatmap_dendrogram <- function (file_in,
# file_out,
	figure_type   = "png",                              # c("jpg" "pdf" "ps" or "png") # added this as an input argument 8-10-10
	image_out = gsub(" ", "", paste(file_in, ".HD.", figure_type)),
	image_title = image_out, # image_out
# x,                                             # x = input_object that contains the data                 
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


# These functions "wrap the file system" around the 
# objects we handle.  They are intended perhaps 
# to be eventually replaced by class methods
fsUnwrap (x) {
	if (class (x) == "character") data.matrix (read.table (x, row.names= 1 , header = TRUE, sep = "\t", comment.char = "", quote = ""))
	else as.matrix (x)
	}

fsWrap (x, toFile) {
	if (! is.null (toFile)) {
		if (class (x) == "matrix") write.table (M, file = file_out, sep = "\t", col.names = NA, row.names = TRUE, quote = FALSE)
		else save (x, file = toFile)
		toFile
	}
	else invisible (M)
	}

oneof <- function (x, ...) any (x == unlist ( list (...)))

lastof <- function (x) { x [length (x)] }

grtype <- function (fileName) {
	s <- lastof (strsplit (fileName, ".") [[1]])
	if (oneof (s, "jpeg", "pdf", "png", "ps")) s
	else "png"
	}

