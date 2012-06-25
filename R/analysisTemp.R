
### the generic purpose of "render" is to visualize an analysis.
### usually that is via a method for an object of superclass "analysis"
setGeneric ("render", function (x, ...) standardGeneric ("render"), useAsDefault = FALSE)

### merge similarly, specialized for use in function doing final rendering (i.e. "render" methods)
resolveParList <- function (call, object, defaults)
	resolveMerge (call, resolveMerge (object, resolveMerge (mConfig$par(), defaults)))


setClass ("pco",
	representation (
		values = "numeric",
		vectors = "matrix",
#		distances = "dist",					need to setOldClass I think
		par = "list"),
	contains = NULL)
#setMethod ("initialize", "pco",
#	function (.Object, ...) {
#		...
#		.Object } )
setMethod ("print", "pco", function (x, ...) print (list (values = x@values, vectors = x@vectors, distances = x@distances)))
setMethod ("summary", "pco", function (object, ...) print (object))
setMethod ("show", "pco", function (object) print (object))
#setIs ("pco", "...",
#	coerce = function (from) {},
#	replace = function (object, value) {} )
#setGeneric ("pco")
#setMethod ("pco", "mmatrix",
#		function (x, par = list (), ...) {
#			parDefaults <- list ()
#			pco (x, resolveMerge (par, parDefaults), ...)
#			} )


#################################################
### as an exception "render" can be applied to
### a "collection" object to produce boxplots of the 
### specified views
###
### USAGES
###   render (collection (c ("4441872.3", "4442034.3", "4448888.3", "4449999.3")))
#################################################

# setMethod ("render", "pco",
setMethod ("render", "list",
	function (x, ...) {
		plot (x$vectors[,1], x$vectors[,2])
		} )

setMethod ("render", "collection",
	function (x, views = c ("count", "normed"), ...) {
		parDefaults = list (
# split.screen()
			figs = c (2,1),
# boxplot()
			main = c ("raw data", "log2(x+1) & centered per sample, scaled 0 to 1 over all samples"),
			las = 2,
# Cairo()
			width = 950,
			height = 1000,
			pointsize = 12,
			res = NA,
			units = "px")
		n <- length (views)
#		args = list (...)
#		p <- resolveParList (args, list (), parDefaults)
		p <- list (...)
		print(unlist(p))
		if (is.null (p$figs)) p$figs <- c (n, 1)
		if (is.null (p$main)) p$main <- paste ("<", views, "> summary", sep = "")
		else p$main <- rep (p$main, length.out = n)
# Cairo(): width, height, file, type, pointside, bg, canvas, units
# dev.new(): unknown
		if (!is.null (p$file) && suppressWarnings (suppressPackageStartupMessages (require (Cairo))))
			eval (as.call (c (quote (Cairo), p)))
		else 
			eval (as.call (c (quote (dev.new), p)))
# split.screen(): figs
		split.screen (p$figs)
		main <- p$main
		p$width <- NULL			# resolve conflicts in parameters names
		p$height <- NULL
		for (j in 1:n) {
			screen (j)
# want the call to boxplot to accept _all_ elements of p, but this might do for now
# boxplot(): LOTS of options
			p$main <- main [j]
			eval (as.call (c (quote (boxplot), x = x [[views [j]]], p)))
			}
		if (!is.null (p$file)) dev.off ()
		} )
		
# returns: list of "values" (numeric), "vectors" (matrix), "dist" (dist)
# computes: distance, scaled eigenvalues, and eigenvectors
# ... consider better way to combine?  c, cbind, data.matrix ...
pco <- function (x, method = "bray-curtis") {
	reqPack ("ecodist")
	x <- as.matrix (x)
	D <- matR::dist (x, method)
	P <- ecodist::pco (D)
	scaled <- P$values / sum (P$values)
	names (scaled) <- paste ("PCO", 1:length(scaled), sep = "")
	dimnames (P$vectors) [[1]] <- dimnames (x) [[2]]
	list (values = scaled, vectors = P$vectors, distances = D)				# this is the template for our pco object
 	}

# note: distance is calculated between columns, a difference from others common distance functions
dist <- function (x, method = "bray-curtis") {
	x <- as.matrix (x)
	if (any (method == c ("bray-curtis", "jaccard", "mahalanobis", "sorensen", "difference"))) {
		reqPack ("ecodist")
		ecodist::distance (t (x), method = method)
		}
	else stats::dist (t (x), method = method)
### unifrac support will go here
	}

normalize <- function (x) {
	x <- as.matrix (x)
	x [is.na (x)] <- 0
# log scale
	x <- log2 (x + 1)
# then scale by mean and standard deviation per sample
	mu <- colMeans (x)
	sigm <- unlist (sapply (as.data.frame (x), sd))
	x <- t ((t (x) - mu) / sigm)
# then scale to [0,1] across all samples.
# it can occur that a column is uniformly zero, so
# na.rm is necessary for such cases
	shift <- min (x, na.rm = TRUE)
	scale <- max (x, na.rm = TRUE) - shift
	if (scale != 0) x <- (x - shift) / scale
	x
	}

permutations <- function (x, ntimes = 1, type = "sample") {
	x <- as.matrix (x)
	m <- nrow (x)
	n <- ncol (x)
	P <- matrix (nrow = m, ncol = n)
	rownames (P) <- rownames (x)
	colnames (P) <- colnames (x)

	totalSum <- base::sum (x)
	for (j in 1:ntimes) {
		switch (type,
# shuffle values within each sample;
# distributions are maintained within each sample.
			"sample" = { for (k in 1:n) P[,k] <- sample (x [,k]) },
# shuffle values across the whole matrix;
# distribution is maintained across the matrix, but not within samples.
			"dataset" = { P <- matrix (sample (as.vector (x)), nrow = m, ncol = n, dimnames = list (rownames (x), colnames (x))) },
# total sum of matrix entries is randomly redistributed; 
# should lose the sample and data set distributions.
			"complete" = {
				P <- 0
				redistrib <- table (sample ( 1:(m*n), size = totalSum, replace = TRUE ))
				P [as.numeric (names (redistrib))] <- redistrib
				dim (P) <- c (m, n) ; dimnames (P) <- list (rownames (x), colnames (x))
				}
			)
# FINISH:  how to write to file, how to return multiple permutations?
#		if (verbose) sum_rand_data = base::sum(rand_data); verbose_report(k, sum_data, sum_rand_data, rand_data)
#		if (! is.null (toFile)) write_files (perm_dir, file_name, rand_data, k)
#		fsWrap (P, toFile [j])
		}
	P
	}











# check for valid significance test
# why "as.real"?
# is ok to run make_two_groups routine even for ANOVA and KW, which do not use it?
doStats <- function (x, groups, sig_test =
			c ("t-test-paired", "Wilcoxon-paired", "t-test-un-paired", "Mann-Whitney_un-paired-Wilcoxon", 
				"ANOVA-one-way", "Kruskal-Wallis")) {
	reqPack ("stats")

	x <- as.matrix (x)
	m = nrow (x)
	n = ncol (x)
	ngroups <- nlevels (as.factor (groups))
	results <- matrix (0, m, ngroups + 2)
	rownames (results) <- rownames (x)
	colnames (results) <- c (paste ("group_(", levels (as.factor (groups)), ")_stddev", sep = ""),
								paste (sig_test, "_stat", sep = ""),
								paste (sig_test, "_p_value", sep = ""))
	for (j in 1:m) {
		oneRow <- data.frame (values = x [j,], group = groups, stringsAsFactors = TRUE)
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
	results
	}
