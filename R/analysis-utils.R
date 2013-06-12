
###########################################################################
### these are elementary analysis routines, typically (mere)
### functions not methods (not strictly always though).
### they apply to "matrix", not "collection".
###########################################################################

###########################################################################
# we have three distance functions:

# dist (x, method = , ..., bycol = FALSE)
# group.dist (x, groups=factor(1), method =, ..., bycol = TRUE)
# dist2groups (x, v, groups=factor(1), ..., bycol = TRUE)
# 
# ...correct defaults for bycol?
# ...handling of cbind?

# dist() adds metrics ("methods") to the standard function stats::dist()
#
# our "dist" will mask any "dist" behind it in the search path.
# we only want to dispatch on "matrix".
# but we must not block, if some other dist does apply to another class.
# for instance, "stats::dist" applies to "data.frame".
# therefore we must define an "ANY" method to pass the call along.
# otherwise a call would fail: R sees our "dist" has no appropriate method and gives up.
#
# also, it is not possible to make stats::dist the default method, 
# because it lacks "..." as a formal parameter, 
# and we need additional arguments.
#
# this problem and solution are repeated for other analysis functions

#------> MUST RETURN STANDARD DISTANCE OBJECT FOR STANDARD CALLS

# group.dist() gives both intra- and inter-group mean pairwise distance
# input:		matrix with column groupings
# output:		symmetric matrix with entry (i,j) equal to MPD between groups i and j

# dist2group() computes distance from a single metagenome to groups:
# input:		vector, matrix, and (optionally) groups for matrix columns
# output:		vector of MPD distance(s) from given vector to given matrix or its groups
###########################################################################
setMethod ("dist", "matrix", 
					 function (x, y = NULL, groups = NULL,
					 					method = c ("euclidean", "bray-curtis", "jaccard", "mahalanobis", "sorensen", 
					 											"difference", "maximum", "manhattan", "canberra", "binary", "minkowski"), 
					 					..., bycol = FALSE) {
					 	method <- match.arg (method)
					 	if (bycol) x <- t (x)
					 	if (is.null (groups)) {
					 		groups <- if (!any (duplicated (rownames (x))) && !is.null (rownames (x)))
					 			rownames (x)
					 		else 1:nrow (x)
					 		nogroups <- TRUE
					 	}
					 	else nogroups <- FALSE
					 	groups <- as.factor (groups)
					 	
					 	dist.fun <- if (method %in% c ("bray-curtis", "jaccard", "mahalanobis", "sorensen", "difference")) {
					 		reqPack ("ecodist")
					 		ecodist::distance
					 	}
					 	else stats::dist
					 	
					 	if (!is.null (y)) return (
					 		tapply (
					 			apply (x, 1, function (x, y) dist.fun (rbind (x, y), method = method), y), 
					 			groups, mean))

					 	D <- as.matrix (dist.fun (x, method))
					 	rownames (D) <- colnames (D) <- groups

					 	n <- nlevels (groups)
					 	group.names <- levels (groups)
					 	dummy <- matrix (0, nrow = n, ncol = n, 
					 									 dimnames = list (group.names, group.names))
					 	row.groups <- as.vector (row (dummy, TRUE))
					 	col.groups <- as.vector (col (dummy, TRUE))
					 	res <- matrix (mapply (function (r, c, D) mean (D [rownames (D) == r, colnames (D) == c]),
					 												 row.groups, col.groups, MoreArgs = list (D)),
					 								 nrow = n, ncol = n,
					 								 dimnames = list (group.names, group.names))

# note: we assume table() gives results per level in the _same_order_ as levels()
# is it right??
					 	k <- table (groups)
					 	k <- k / ifelse (k == 1, 1, k - 1)
					 	diag (res) <- k * diag (res)
					 	if (nogroups) as.dist (res)
					 	else res
					 } )
setMethod ("dist", "ANY", prior ("dist"))


###########################################################################
# 
###########################################################################
remove.singletons <- function (x, lim.entry = 1, lim.row = 1, ...) {
	x <- as.matrix (x)
	x [is.na (x)] <- 0
	x [x <= lim.entry] <- 0
	x [apply (x, MARGIN = 1, sum) >= lim.row, ]
}


###########################################################################
# log scale,
# then scale by mean and standard deviation per sample,
# then scale to [0,1] across all samples.
# it can occur that a column is uniformly zero;
# na.rm is necessary for such cases
###########################################################################
normalize <- function (x, method = c("standard"), ...) {
	method <- match.arg(method)
	x <- as.matrix(x)
	x[is.na(x)] <- 0
	x <- log2(x + 1)
	mu <- matrix(apply(x, 2, mean), nr = nrow(x), nc = ncol(x),
							 byrow = TRUE)
	sigm <- apply(x, 2, sd)
	sigm <- matrix(ifelse(sigm == 0, 1, sigm), nr = nrow(x),
								 nc = ncol(x), byrow = TRUE)
	x <- (x - mu)/sigm
	shift <- min(x, na.rm = TRUE)
	scale <- max(x, na.rm = TRUE) - shift
	if (scale != 0) x <- (x - shift)/scale
	x
}


###########################################################################
# sample: shuffle entries within each sample (column)
# dataset: shuffle entries values across whole matrix
# complete: shuffle total sum of counts across whole matrix
###########################################################################
randomize <- function (x, ntimes = 1, method = c ("sample", "rowwise", "dataset", "complete"), 
											 seed = NULL, FUN = identity, ...) {
	x <- as.matrix (x)
	f <- switch(
		match.arg (method),
		sample = function (x, ...) apply (x, 2, sample),
		rowwise = function (x, ...) apply (x, 1, sample),
		dataset = function (x, ...) matrix (sample (as.vector (x)), nrow (x), ncol (x)),
		complete = function (x, tot, ...)
				matrix (tabulate (sample (1:length(x), tot, TRUE), length (x)), nrow (x), ncol (x)))

	perms <- sapply (replicate (ntimes, x, simplify = FALSE), f, tot = sum (x), simplify = FALSE)
	sapply (perms, FUN, ..., simplify = FALSE)
}


###########################################################################
# statistical significance testing.
# built on various stats tests from base R.
###########################################################################
sigtest <- function (x, groups, 
				 test = c ("Kruskal-Wallis", "t-test-paired", "Wilcoxon-paired", "t-test-unpaired", "Mann-Whitney-unpaired-Wilcoxon", "ANOVA-one-way"),
				 fdr.level = NULL, qvalue = FALSE, ...) {
	x <- as.matrix (x)
	groups <- as.factor (groups)
	test <- match.arg (test)
	fun <- switch (
		test,
		"t-test-unpaired" = function (x1, x2) t.test (x1, x2),
		"t-test-paired" = function (x1, x2) t.test (x1, x2, paired = TRUE),
		"Mann-Whitney-unpaired-Wilcoxon" = function (x1, x2) wilcox.test (x1, x2, exact = TRUE),
		"Wilcoxon-paired" = function (x1, x2) wilcox.test (x1, x2, exact = TRUE, paired = TRUE),
		"Kruskal-Wallis" = function (r) unlist (kruskal.test (r, groups) [c ("statistic", "p.value")], use.names = FALSE),
		"ANOVA-one-way" = function (r) {
			a <- anova (aov (r ~ groups)) [c ("F value", "Pr(>F)")]
			c (a ["F value"] [1,1], a ["Pr(>F)"] [1,1])
		})

	res <- list()
	res$samples <- colnames (x)
	res$groups <- groups
	res$mean <- t (apply (x, 1, function (row) tapply (row, groups, mean)))
	res$sd <- t (apply (x, 1, function (row) tapply (row, groups, sd)))

	stat <- as.data.frame (t (
		if (test %in% c ("Kruskal-Wallis", "ANOVA-one-way")) apply (x, 1, fun)
		else {
			g1 <- lapply (apply (x [ ,groups == levels (groups) [1]], 1, list), unlist)
			g2 <- lapply (apply (x [ ,groups == levels (groups) [2]], 1, list), unlist)
			mapply (function (x1, x2) unlist (fun (x1, x2) [c ("statistic", "p.value")], use.names = FALSE), g1, g2)
		}))
	names (stat) <- c ("statistic", "p.value")
	if (test != "ANOVA-one-way" && qvalue) {
		reqPack ("qvalue")
		stat [c ("q.value", "significant")] <- qvalue (stat$p.value, fdr.level = fdr.level) [c ("qvalues", "significant")]
	}

	append (res, stat)
}
