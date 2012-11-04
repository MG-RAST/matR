
### Defining generics for common functions has to be done right,
### so we don't produce weird effects in users' customary workflows.

setGeneric ("dist")
setGeneric ("normalize", function (x, ...) standardGeneric ("normalize"))
setGeneric ("permutations", function (x, ...) standardGeneric ("permutations"))
# setGeneric ("sigtest", function (x, ...) standardGeneric ("sigtest"))
setGeneric ("pco", function (x, ...) standardGeneric ("pco"))
setGeneric ("heatmap")

### we use new-style classes, but in an old-style way.
### analysis objects are lists, and within the code there
### are implicit _conventions_ telling the necessary components,
### but these are not declared to nor enforced by R itself.
### ...we want to use S4 dispatching but not slots.

# elements: par, values, vectors, dist, method
setClass ("pco", repr = NULL, contains = "namedList")
# assign in par: labels, colors

# elements: call (this is temporary, pending rewrite of viz routine)
setClass ("heatmap", repr = NULL, contains = "list")
# assign in par: nothing

# elements: result (data.frame), groups (factor)
setClass ("sigtest", repr = NULL, contains = "list")

####################################################
### principal coordinates analysis
####################################################
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

setMethod ("pco", "collection",
           function (x, view = "normed", components = c (1,2), method = "bray-curtis", file = NA, ...) {
             reqPack ("ecodist")
             D <- matR::dist (x [[view]], method)
             P <- ecodist::pco (D)
             scaled <- P$values / sum (P$values)
             names (scaled) <- paste ("PCO", 1:length(scaled), sep = "")
             rownames (P$vectors) <- colnames (x [[view]])
             P <- new ("pco", list (par = list (...), values = scaled, vectors = P$vectors, dist = D))
             
             par <- list ()
             par$main <- "Principal Coordinates Analysis"
             par$labels <- if (!all (groups (x) == 1)) paste (names (x), " (", groups (x), ")", sep = "")
             else names (x)
             par [c ("xlab", "ylab", if (length (components) == 3) "zlab" else NULL)] <-
               paste ("PC", components, ", R^2 = ", format (P$values [components], dig = 3), sep = "")
             
             g <- groups (x)
             levels (g) <- colors() [sample (length (colors()), nlevels (g))]
             par$col <- as.character (g)
             
             plot.new ()
             
             i <- P$vectors [ ,components [1]]
             j <- P$vectors [ ,components [2]]
             k <- if (length (components) == 3) P$vectors [ ,components [3]] else NULL
             if (is.null (k)) {
               plot (x = i, y = j, xlab = par$xlab, ylab = par$ylab, main = par$main)
               points (x = i, y = j, pch = 19, col = par$col)
             }
             else {
               reqPack ("scatterplot3d")
               xys <- scatterplot3d (x = i, y = j, z = k, type = "h", lty.hplot = "dotted", main = par$main, 
                                     pch = 19, color = par$col,
                                     xlab = par$xlab, ylab = par$ylab, zlab = par$zlab) $ xyz.convert (i, j, k)
               i <- xys$x ; j <- xys$y
             }
             text (x = i, y = j, labels = par$labels, pos = 4, cex = .7)
             invisible (P)
           } )

setMethod ("print", "pco", function (x, ...) print (x@.Data))
setMethod ("summary", "pco", function (object, ...) print (object))
setMethod ("show", "pco", function (object) print (object))


####################################################
### heatmap-dendrogram analysis
####################################################
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


setMethod ("heatmap", "collection",
           function (x, view = "normed", rows = TRUE, file = NA, ...) {
             reqPack ("gplots")

             par <- list ()
             par$main <- "Heatmap Dendrogram Analysis"
             par$margins <- c (9,1)
             par$colsep <- 1:length (names (x))
             par$labCol <- if (!all (groups (x) == 1)) paste (names (x), " (", groups (x), ")", sep = "")
             else names (x)

             plot.new ()
             heatmap.2(as.matrix (x [[view]] [rows, ]), margins = c(8,1), cexCol = .95, labRow = NA,
                       labCol = par$labCol,
                       key = FALSE, trace = "none", colsep = par$colsep, sepwidth = 0.01, 
                       main = par$main)
           } )


####################################################
### statistical significance testing
####################################################
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

sigtest <- function (x, groups, sig_test = 
  c ("t-test-paired", "Wilcoxon-paired", "t-test-un-paired",
     "Mann-Whitney_un-paired-Wilcoxon", "ANOVA-one-way", "Kruskal-Wallis"),
                     fdr.level = NULL, ...) {
  x <- as.matrix (x)
  groups <- as.factor (groups)
  sig_test <- match.arg (sig_test)
  res <- list()
  res$samples <- colnames (x)
  res$groups <- groups
  res$mean <- t (apply (x, 1, function (row)
    tapply (row, groups, mean)))
  res$sd <- t (apply (x, 1, function (row)
    tapply (row, groups, sd)))
  fun <- switch (sig_test,
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
    if (sig_test %in% c ("Kruskal-Wallis", "ANOVA-one-way"))
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
#   if (sig_test != "ANOVA-one-way") {
#     reqPack ("qvalue")
#     stat [c ("q.value", "significant")] <- 
#       qvalue (stat$p.value, fdr.level = fdr.level) [c ("qvalues", "significant")]
#   }
  res$stat <- stat
  invisible (res)
#   new ("sigtest", res)
  }


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
### note: distance is calculated between columns, a difference from other common distance functions
### that needs to be documented!
setMethod ("dist", "mmatrix", function (x, method = "bray-curtis") {
	x <- as.matrix (x)
	if (any (method == c ("bray-curtis", "jaccard", "mahalanobis", "sorensen", "difference"))) {
		reqPack ("ecodist")
		ecodist::distance (t (x), method = method)
		}
	else stats::dist (t (x), method = method)
### we'll support unifrac too
	} )

setMethod ("normalize", "mmatrix", function (x, ...) { })
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
setMethod ("normalize", "Matrix", getMethod ("normalize", "matrix"))

setMethod ("permutations", "mmatrix", function (x, ...) {} )
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
