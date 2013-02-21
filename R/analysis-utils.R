
####################################################
### find "elementary" analysis functions here.
### simple functions doing basic things.
### by and large these apply to vanilla matrices.
####################################################

# note: we calculate distance between columns, a difference from other common distance functions
# that ne	eds to be documented!

# typically our generic "dist" will mask stats::dist, so we must pass along calls intended for 
# that function, by defining a method that dispatches on "ANY".  It is not possible to make
# stats::dist the default method, because it lacks "..." as a formal parameter, and we need
# additional arguments ("view").
# presently, this is a bad hack. we should look for _any_ dist behind us in the search path,
# not explicitly stats::dist.

# make a function in utils.R: where am I on the search path?
# whereami <- function () match ("package:matR", search ()))
# maybe use: get ("dist", pos = match("package:matR", search ()) + 1)
# or...
# get("rownames", env=as.environment(find("rownames")[2])) (x)

setMethod ("dist", "ANY", function (x, ...) stats::dist (x, ...))
setMethod ("dist", "matrix", 
					 function (x, method = 
					 	c ("bray-curtis", "jaccard", "mahalanobis", "sorensen", 
					 		 "difference", "euclidean", "maximum", "manhattan", 
					 		 "canberra", "binary", "minkowski"), bycol = FALSE, ...) {
					 	if (bycol) {
					 		method <- match.arg (method)
					 		if (method %in% c ("bray-curtis", "jaccard", "mahalanobis", "sorensen", "difference")) {
					 			reqPack ("ecodist")
					 			ecodist::distance (t (x), method = method, ...)
					 		}
					 		else stats::dist (t (x), method = method, ...)
# we want to add unifrac, others
					 	}
					 	else stats::dist(x, method = method, ...) })
setMethod ("dist", "collection", function (x, view = length (views (x)),
																					 method = 
																					 	c ("bray-curtis", "jaccard", "mahalanobis", "sorensen", 
																					 		 "difference", "euclidean", "maximum", "manhattan", 
																					 		 "canberra", "binary", "minkowski"), ...)
	dist (x [[view, plain = TRUE]], method = method, bycol = TRUE, ...))

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
