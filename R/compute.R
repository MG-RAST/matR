
### defining generics for common functions has to be done right
### so we don't produce weird effects in users' customary workflows.

setGeneric ("dist")
setGeneric ("normalize", function (x, ...) standardGeneric ("normalize"))
setGeneric ("permutations", function (x, ...) standardGeneric ("permutations"))

setGeneric ("sigtest", function (x, ...) standardGeneric ("sigtest"), useAsDefault = FALSE)
# setMethod ("sigtest", "collection", function (x, view = "normed") {...})
# setMethod ("sigtest", "mmatrix")
# setMethod ("sigtest", "matrix")
setGeneric ("pco", function (x, ...) standardGeneric ("pco"), useAsDefault = FALSE)
# setMethod ("pco", "collection", function (x, view = "normed") {...})
# setMethod ("pco", "mmatrix", ...)
# setMethod ("pco", "matrix", ...)
setGeneric ("heatmap")
# setMethod ("heatmap", "collection", function (x, view = "normed") {...})
# setMethod ("heatmap", "mmatrix", ...)
# setMethod ("heatmap", "matrix", ...)

setClass ("pco",
#           representation (
#             values = "numeric",
#             vectors = "matrix",
#             dist = "dist",					# need to setOldClass?
#             par = "list"),
          contains = "list")
setClass ("heatmap",
#           representation (
#             rowClust = "hclust",
#             colClust = "hclust"),
          contains = "list")
setClass ("sigtest",
#           representation (
#             data = "matrix",
#             pvals = "numeric",
#             qvals = "numeric"),
          contains = "list")

####################################################
### principal components analysis
####################################################

setMethod ("pco", "matrix", function (x, ...) {...} )

# returns: list of "values" (numeric), "vectors" (matrix), "dist" (dist)
# computes: distance, scaled eigenvalues, and eigenvectors
# ... consider better way to combine?  c, cbind, data.matrix ...
setMethod ("pco", "mmatrix", 
           function (x, method = "bray-curtis") {
             reqPack ("ecodist")
             x <- as.matrix (x)
             D <- matR::mdist (x, method)
             P <- ecodist::pco (D)
             scaled <- P$values / sum (P$values)
             names (scaled) <- paste ("PCO", 1:length(scaled), sep = "")
             dimnames (P$vectors) [[1]] <- dimnames (x) [[2]]
             list (values = scaled, vectors = P$vectors, distances = D)    		# this is the template for our pco object
           } )

setMethod ("print", "pco", function (x, ...) print (list (values = x@values, vectors = x@vectors, dist = x@dist)))
setMethod ("summary", "pco", function (object, ...) print (object))
setMethod ("show", "pco", function (object) print (object))
#setMethod ("initialize", "pco",
#  function (.Object, ...) {
#		...
#		.Object } )
#setIs ("pco", "...",
#	coerce = function (from) {},
#	replace = function (object, value) {} )
#setGeneric ("pco")
#setMethod ("pco", "mmatrix",
#		function (x, par = list (), ...) {
#			parDefaults <- list ()
#			pco (x, resolveMerge (par, parDefaults), ...)
#			} )


####################################################
### heatmap-dendrogram analysis
####################################################
setMethod ("heatmap", "collection", function (x, view = "normed", ...)
  {
# for now, just leave render doing all the work!  so for now a heatmap object 
# will be just .. the parameter itself
})


####################################################
### statistical significance testing
####################################################
# check for valid significance test
# why "as.real"?
# is ok to run make_two_groups routine even for ANOVA and KW, which do not use it?
setMethod ("sigtest", "matrix",
           function (x, groups, sig_test =
             c ("t-test-paired", "Wilcoxon-paired", "t-test-un-paired", "Mann-Whitney_un-paired-Wilcoxon", 
                "ANOVA-one-way", "Kruskal-Wallis"), ...) {
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
           } )


####################################################
### other computation functions
####################################################
### note: distance is calculated between columns, a difference from other common distance functions
### that needs to be documented!!
setMethod ("dist", "matrix", function (x, method = "bray-curtis") {
	x <- as.matrix (x)
	if (any (method == c ("bray-curtis", "jaccard", "mahalanobis", "sorensen", "difference"))) {
		reqPack ("ecodist")
		ecodist::distance (t (x), method = method)
		}
	else stats::dist (t (x), method = method)
### unifrac support to go here, too
	} )

setMethod ("normalize", "matrix", function (x, ...) {
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
	} )

setMethod ("permutations", "matrix",
  function (x, ntimes = 1, type = "sample", ...) {
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
	} )


setClassUnion ("analysis", c ("pco", "heatmap", "sigtest"))
