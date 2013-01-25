
####################################################
### find "elementary" analysis functions here.
### simple functions doing basic things.
####################################################

# note: we calculate distance between columns, a difference from other common distance functions
# that needs to be documented!

# typically our generic "dist" will mask stats::dist, so we must pass along calls intended for 
# that function, by defining a method that dispatches on "ANY".  It is not possible to make
# stats::dist the default method, because it lacks "..." as a formal parameter, and we need
# additional arguments ("view").
# presently, this is a bad hack. we should look for _any_ dist behind us in the search path,
# not explicitly stats::dist.

# use: get ("stats", pos = match("package:matR", search ()) + 1)
setMethod ("dist", "ANY", function (x, ...) stats::dist (x, ...))
setMethod ("dist", "collection", function (x, view = "normed", method = "bray-curtis", ...) {
	x <- x [[view, plain = TRUE]]
	if (method %in% c ("bray-curtis", "jaccard", "mahalanobis", "sorensen", "difference")) {
		reqPack ("ecodist")
		ecodist::distance (t (x), method = method)
	}
	else stats::dist (t (x), method = method)
### we want to add unifrac, others
} )

# needs option to remove rows with total count less than a certain amount
remove.singletons <- function (x, lim = 1, ...) {
	x <- as.matrix (x)
	x [is.na (x)] <- 0
	x [x <= lim] <- 0
	x [apply (x, MARGIN = 1, sum) != 0, ]
}

normalize <- function (x, ...) {
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

randomize <- function (x, ntimes = 1, type = c ("sample", "dataset", "complete"), ...) {
	x <- as.matrix (x)
	m <- nrow (x)
	n <- ncol (x)
	P <- matrix (nrow = m, ncol = n)
	dimnames (P) <- dimnames (x)

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
