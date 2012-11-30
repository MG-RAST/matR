
### we use new-style classes, but in an old-style way.
### analysis objects are lists, with components
### hard-coded in the routines, not explicitly declared.
### ...we want to use S4 dispatching but not slots.

setClass ("pco", repr = NULL, contains = "list")
setClass ("heatmap", repr = NULL, contains = "list")
setClass ("sigtest", repr = NULL, contains = "list")

####################################################
### principal coordinates analysis
####################################################

setMethod ("pco", "ANY", function (x, ...) ecodist::dist (x, ...))
# see "dist" below for the rationale for an "ANY" method
# here as below, this is a temporary hack...
setMethod ("pco", "collection", function (x, view = "normed", components = c (1,2,3), 
																					method = "bray-curtis", file = NA, ...) {
	reqPack ("ecodist")
	D <- matR::dist (x, view, method)
	P <- ecodist::pco (D)
	scaled <- P$values / sum (P$values)
	names (scaled) <- paste ("PCO", 1:length(scaled), sep = "")
	rownames (P$vectors) <- colnames (x [[view]])
	P <- new ("pco", list (values = scaled, vectors = P$vectors, dist = D))

	par <- list ()
	par$main <- paste (views (x) [[view]], collapse = " : ")
	par$labels <- if (length (names (x)) != 0) names (x) else samples (x)
	if (length (groups (x)) != 0) par$labels <- paste (par$labels, " (", groups (x), ")", sep = "")
	par [c ("xlab", "ylab", if (length (components) == 3) "zlab" else NULL)] <-
		paste ("PC", components, ", R^2 = ", format (P$values [components], dig = 3), sep = "")
	col <- if (length (groups (x)) != 0) groups (x) else factor (rep (1, length (samples (x))))
	levels (col) <- colors() [sample (length (colors()), nlevels (col))]
	g <- as.character (col)
	par$pch <- 19
	par$cex <- 0.7

	i <- P$vectors [ ,components [1]]
	j <- P$vectors [ ,components [2]]
	k <- if (length (components) == 3) P$vectors [ ,components [3]] else NULL
	if (is.null (k)) {
		par$col <- col
		par <- resolveMerge (list (...), par)
		xcall (plot, x = i, y = j, with = par, without = "labels")
		xcall (points, x = i, y = j, with = par, without = "labels")
		grid ()
		}
	else {
# parameter "color" has to be specially handled.
# "points" above wants "col", scatterplot3d wants "color", and we
# want the user not to worry about it...
		par$color <- col
		par$type <- "h"
		par$lty.hplot <- "dotted"
		par$axis <- TRUE
		par$box <- FALSE
		par <- resolveMerge (list (...), par)
		reqPack ("scatterplot3d")
		xys <- xcall (scatterplot3d, x = i, y = j, z = k, with = par, 
									without = c ("cex", "labels")) $ xyz.convert (i, j, k)
 		i <- xys$x ; j <- xys$y
	}
	text (x = i, y = j, labels = par$labels, pos = 4, cex = par$cex)
	P
} )

setMethod ("print", "pco", function (x, ...) print (x@.Data))
setMethod ("summary", "pco", function (object, ...) print (object))
setMethod ("show", "pco", function (object) print (object))


####################################################
### heatmap-dendrogram analysis
####################################################

setMethod ("heatmap", "ANY", function (x, ...) stats::heatmap (x, ...))
# see "dist" below for the rationale for an "ANY" method
# here as below, this is a temporary hack...
setMethod ("heatmap", "collection", function (x, view = "normed", rows = TRUE, file = NA, ...) {	
	par <- list ()
	par$main <- paste (views (x) [[view]], collapse = " : ")
	par$colsep <- 1:length (samples (x))
	par$labRow <- NA
	par$labCol <- if (length (names (x)) != 0) names (x) else samples (x)
	if (length (groups (x)) != 0) par$labCol <- paste (par$labCol, " (", groups (x), ")", sep = "")
	par$key <- FALSE
	par$trace <- "none"
	par$sepwidth <- 0.01
	par <- resolveMerge (list (...), par)

	reqPack ("gplots")
	xcall (heatmap.2, x [[view, plain = TRUE]] [rows, ], with = par)
} )


####################################################
### statistical significance testing
####################################################

setMethod ("sigtest", "collection", 
					 function (x, view = "normed",
					 					test = c ("t-test-paired", "Wilcoxon-paired", "t-test-un-paired", 
					 										"Mann-Whitney_un-paired-Wilcoxon", "ANOVA-one-way", "Kruskal-Wallis"),
					 					fdr.level = NULL, qvalue = FALSE, ...)
					 	sigtest (x [[view, plain = TRUE]], groups (x), test, fdr.level, qvalue, ...))
# with this function we aim to provide both a generally-applicable routine, and
# a routine specialized for metagenome collections
setMethod ("sigtest", "matrix", function (x, groups, test = c ("t-test-paired", "Wilcoxon-paired", "t-test-un-paired", 
																															 "Mann-Whitney_un-paired-Wilcoxon", "ANOVA-one-way", 
																															 "Kruskal-Wallis"),
																					fdr.level = NULL, qvalue = FALSE, ...) {
	x <- as.matrix (x)
	groups <- as.factor (groups)
	test <- match.arg (test)
	res <- list()
	res$samples <- colnames (x)
	res$groups <- groups
	res$mean <- t (apply (x, 1, function (row)
		tapply (row, groups, mean)))
	res$sd <- t (apply (x, 1, function (row)
		tapply (row, groups, sd)))
	fun <- switch (test,
								 "t-test-un-paired" =
								 	function (x1, x2) t.test (x1, x2),
								 "t-test-paired" = 
								 	function (x1, x2) t.test (x1, x2, paired = TRUE),
								 "Mann-Whitney_un-paired-Wilcoxon" = 
								 	function (x1, x2) wilcox.test (x1, x2, exact = TRUE),
								 "Wilcoxon-paired" = 
								 	function (x1, x2) wilcox.test (x1, x2, exact = TRUE, paired = TRUE),
								 "Kruskal-Wallis" =
								 	function (r) unlist (kruskal.test (r, groups) [c ("statistic", "p.value")], use.names = FALSE),
								 "ANOVA-one-way" = 
								 	function (r) {
								 		a <- anova (aov (r ~ groups)) [c ("F value", "Pr(>F)")]
								 		c (a ["F value"] [1,1], a ["Pr(>F)"] [1,1])
								 	})
	stat <- as.data.frame (t (
		if (test %in% c ("Kruskal-Wallis", "ANOVA-one-way"))
			apply (x, 1, fun)
		else {
			g1 <- lapply (apply (x [ ,groups == levels (groups) [1]], 1, list), unlist)
			g2 <- lapply (apply (x [ ,groups == levels (groups) [2]], 1, list), unlist)
			mapply (
				function (x1, x2) 
					unlist (fun (x1, x2) [c ("statistic", "p.value")], use.names = FALSE),
				g1, g2)
		}))
	names (stat) <- c ("statistic", "p.value")
	if (test != "ANOVA-one-way" && qvalue) {
		reqPack ("qvalue")
		stat [c ("q.value", "significant")] <- 
			qvalue (stat$p.value, fdr.level = fdr.level) [c ("qvalues", "significant")]
	}
	res$stat <- stat
	invisible (res)
	#   new ("sigtest", res)
}	)

setMethod ("print", "sigtest", function (x, ...) {
#   group 1 - metagenomes
#   group 2 - metagenomes, etc..
#   
  print (x@.Data)
  } )
setMethod ("summary", "sigtest", function (object, ...) print (object))
setMethod ("show", "sigtest", function (object) print (object))

####################################################
### other computation functions
####################################################
# note: we calculate distance between columns, a difference from other common distance functions
# that needs to be documented!

# use: get ("stats", pos = match("package:matR", search ()) + 1)
setMethod ("dist", "ANY", function (x, ...) stats::dist (x, ...))
# typically our generic "dist" will mask stats::dist, so we must pass along calls intended for 
# that function, by defining a method that dispatches on "ANY".  It is not possible to make
# stats::dist the default method, because it lacks "..." as a formal parameter, and we need
# additional arguments ("view").
# presently, this is a bad hack. we should look for _any_ dist behind us in the search path,
# not explicitly stats::dist.
setMethod ("dist", "collection", function (x, view = "normed", method = "bray-curtis", ...) {
	x <- x [[view, plain = TRUE]]
	if (any (method == c ("bray-curtis", "jaccard", "mahalanobis", "sorensen", "difference"))) {
		reqPack ("ecodist")
		ecodist::distance (t (x), method = method)
		}
	else stats::dist (t (x), method = method)
### we want to add unifrac...
	} )

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

permutations <- function (x, ntimes = 1, type = "sample", ...) {
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

setClassUnion ("analysis", c ("pco", "heatmap", "sigtest"))





# 
# # setMethod ("pco", "matrix", 
# #            function (x, method = "bray-curtis", ...) {
# #              reqPack ("ecodist")
# #              D <- matR::dist (as.mmatrix (x), method)
# #              P <- ecodist::pco (D)
# #              scaled <- P$values / sum (P$values)
# #              names (scaled) <- paste ("PCO", 1:length(scaled), sep = "")
# #              rownames (P$vectors) <- colnames (x)
# #              new ("pco", list (par = list (...), values = scaled, vectors = P$vectors, dist = D))
# #            } )
# setMethod ("pco", "matrix",
#            function (x, method = "bray-curtis", ...) 
#              pco (mmatrix (x), method, ...))
# setMethod ("pco", "mmatrix", 
#            function (x, method = "bray-curtis", ...) {
#              reqPack ("ecodist")
#              D <- matR::dist (x, method)
#              P <- ecodist::pco (D)
#              scaled <- P$values / sum (P$values)
#              names (scaled) <- paste ("PCO", 1:length(scaled), sep = "")
#              rownames (P$vectors) <- colnames (x)
#              new ("pco", list (par = list (...), values = scaled, vectors = P$vectors, dist = D))
#            } )
# setMethod ("pco", "collection", 
#            function (x, view = "normed", method = "bray-curtis", ...) {
#              P <- pco (x [[view]], method, ...)
#              if (is.null (P$par$col)) {
#                g <- groups (x)
#                levels (g) <- colors() [sample (length (colors()), nlevels (g))]
#                P$par$col <- as.character (g)
#              }
#              if (is.null (P$par$labels)) P$par$labels <- names (x)
#              if (is.null (P$par$main)) P$par$main <- paste ("PCoA")
#              P
#            } )


# # setMethod ("heatmap", "matrix", 
# #            function (x, ...)
# #              heatmap (mmatrix (x), ...))
# setMethod ("heatmap", "mmatrix",
#            function (x, ...)
#              new ("heatmap", list (par = as.list(match.call()), x = x)))
# setMethod ("heatmap", "collection",
#            function (x, view = "normed", ...) {
#              H <- heatmap (x [[view]], ...)
#              if (is.null (H$par$labCol)) H$par$labCol <- names (x)
#              if (is.null (H$par$labRow)) H$par$labRow <- NA
#              if (is.null (H$par$col_lab_mult)) H$par$col_lab_mult <- 1.2
#              if (is.null (H$par$margins)) H$par$margins <- c (9,1)
#              H
#              } )
# setMethod ("print", "heatmap", function (x, ...) print (x@.Data))
# setMethod ("summary", "heatmap", function (object, ...) print (object))
# setMethod ("show", "heatmap", function (object) print (object))


# check for valid significance test
# why "as.real"?
# is ok to run make_two_groups routine even for ANOVA and KW, which do not use it?

# setMethod ("sigtest", "collection",
#            function (x, sig_test = c ("t-test-paired", "Wilcoxon-paired", "t-test-un-paired", 
#                                       "Mann-Whitney_un-paired-Wilcoxon", "ANOVA-one-way", "Kruskal-Wallis"),
#                      groups = groups(x), fdr.level = NULL, view = "normed", ...)
#              sigtest (x [[view]], sig_test, groups, fdr.level, ...))
# setMethod ("sigtest", "mmatrix",
#            function (x, sig_test = c ("t-test-paired", "Wilcoxon-paired", "t-test-un-paired", 
#                                       "Mann-Whitney_un-paired-Wilcoxon", "ANOVA-one-way", "Kruskal-Wallis"), 
#                      groups, fdr.level = NULL, ...)
#              sigtest (as.matrix (x), sig_test, groups, fdr.level, ...))
# setMethod ("sigtest", "matrix",
#            function (x, groups, 
#                      sig_test = c ("t-test-paired", "Wilcoxon-paired", "t-test-un-paired", 
#                                    "Mann-Whitney_un-paired-Wilcoxon", "ANOVA-one-way", "Kruskal-Wallis"), 
#                      fdr.level = NULL, ...) {
#              x <- as.matrix (x)
#              m <- nrow (x) ; n <- ncol (x)
#              ngroups <- nlevels (as.factor (groups))
#              results <- matrix (0, m, ngroups + 2)
#              rownames (results) <- rownames (x)
#              colnames (results) <- c (paste ("group_(", levels (as.factor (groups)), ")_stddev", sep = ""),
#                                       paste ("group_(", levels (as.factor (groups)), ")_mean", sep = ""),
#                                       paste (sig_test, "_stat", sep = ""),
#                                       paste (sig_test, "_p_value", sep = ""))             
# 
#              for (j in 1:m) {
#                oneRow <- data.frame (values = x [j,], group = groups, stringsAsFactors = TRUE)
#                for (k in 1:ngroups) results [j,k] <- sd (subset (oneRow, group == levels (as.factor (groups)) [k] ) $ values)
#                group1 <- subset (oneRow, subset = (group == levels (as.factor (groups)) [1])) $ values
#                group2 <- subset (oneRow, subset = (group == levels (as.factor (groups)) [2])) $ values
#                results [matrix ( c (j, ngroups + 1, j, ngroups + 2), nrow = 2, ncol = 2, byrow = TRUE) ] <-
#                  as.real (switch (sig_test,
#                                   "t-test-un-paired" =
#                                     t.test (group1, group2) [c ("statistic", "p.value")],
#                                   "t-test-paired" =
#                                     t.test (group1, group2, paired = TRUE) [c ("statistic", "p.value")],
#                                   "Mann-Whitney_un-paired-Wilcoxon" =
#                                     wilcox.test (group1, group2, exact = TRUE) [c ("statistic", "p.value")],
#                                   "Wilcoxon-paired" =
#                                     wilcox.test (group1, group2, exact = TRUE, paired = TRUE) [c ("statistic", "p.value")],
#                                   "Kruskal-Wallis" =
#                                     kruskal.test (oneRow$values, oneRow$group) [c ("statistic", "p.value")],
#                                   "ANOVA-one-way" = {
#                                     a <- anova (aov (values ~ group, data = oneRow))  [c ("F value", "Pr(>F)")]
#                                     c (a ["F value"] [1,1], a ["Pr(>F)"] [1,1])
#                                   }))
#              }
#              if (sig_test != "ANOVA-one-way") {
#                q <- qvalue (results [,"p.value"], fdr.level = fdr.level)
#                results <- cbind (results, qvalue = q$qvalues, significant = q$significant)
#              }
#              new ("sigtest", list (par = NULL, result = data.frame (results)))
#            } )

