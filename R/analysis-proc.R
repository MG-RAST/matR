### we use new-style classes in an old-style way.
### we want to use S4 dispatching but not slots.
### returned analysis objects are lists without formally defined components.

setClass ("pco", repr = NULL, contains = "list")
setClass ("heatmap", repr = NULL, contains = "list")
setClass ("sigtest", repr = NULL, contains = "list")

####################################################
### principal coordinates analysis
####################################################

# see "dist" below for the rationale for an "ANY" method
# here as below, this is a temporary hack...
setMethod ("pco", "ANY", function (x, ...) ecodist::pco (x, ...))
setMethod ("pco", "collection", function (x, view = "normed", components = c (1,2,3), 
																					method = "bray-curtis", ...) {
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
setMethod ("heatmap", "collection", function (x, view = "normed", rows = TRUE, ...) {	
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
	invisible (append (res, stat))
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

setClassUnion ("analysis", c ("pco", "heatmap", "sigtest"))
