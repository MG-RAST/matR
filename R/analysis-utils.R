
####################################################
### these are elementary analysis routines, typically (mere) functions,
### not methods.  they apply to "matrix", not "collection".
####################################################

###
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
###

setMethod ("dist", "matrix", 
					 function (x, 
					 					method = c ("euclidean", "bray-curtis", "jaccard", "mahalanobis", "sorensen", 
					 											"difference", "maximum", "manhattan", "canberra", "binary", "minkowski"), 
					 					..., bycol = FALSE) {
					 	if (bycol) x <- t (x)
					 	method <- match.arg (method)
					 	if (method %in% c ("bray-curtis", "jaccard", "mahalanobis", "sorensen", "difference")) {
					 		reqPack ("ecodist")
					 		ecodist::distance (x, method = method, ...)
					 	}
					 	else stats::dist (x, method = method, ...)
					 } )
setMethod ("dist", "ANY", prior ("dist"))

remove.singletons <- function (x, lim.entry = 1, lim.row = 1, ...) {
	x <- as.matrix (x)
	x [is.na (x)] <- 0
	x [x <= lim.entry] <- 0
	x [apply (x, MARGIN = 1, sum) >= lim.row, ]
}

# log scale,
# then scale by mean and standard deviation per sample,
# then scale to [0,1] across all samples.
# it can occur that a column is uniformly zero;
# na.rm is necessary for such cases
normalize <- function (x, method = c ("standard"), ...) {
	method <- match.arg (method)
	x <- as.matrix (x)
	x [is.na (x)] <- 0
	x <- log2 (x + 1)
	mu <- colMeans (x)
	sigm <- unlist (sapply (as.data.frame (x), sd))
	x <- t ((t (x) - mu) / sigm)
	shift <- min (x, na.rm = TRUE)
	scale <- max (x, na.rm = TRUE) - shift
	if (scale != 0) x <- (x - shift) / scale
	x
}

# sample: shuffle entries within each sample (column)
# dataset: shuffle entries values across whole matrix
# complete: shuffle total sum of counts across whole matrix
randomize <- function (x, ntimes = 1, method = c ("sample", "dataset", "complete"), seed = NULL,
											 FUN = identity, ...) {
	method <- match.arg (method)
	x <- as.matrix (x)
	m <- nrow (x) ; n <- ncol (x)
	tot <- base::sum (x)
	L <- list() ; length (L) <- ntimes
	for (j in 1:ntimes)
		L [[j]] <- FUN (switch (method,
														sample = apply (x, 2, sample),
														dataset = matrix (sample (as.vector (x)), m, n),
														complete = matrix (tabulate (sample (1:(m*n), tot, TRUE), nbins = m*n), m, n)),
										...)
	simplify2array (L)
}
