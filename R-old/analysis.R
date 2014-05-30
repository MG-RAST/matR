
####################################################
### these are compound, not elementary, analysis procedures.
### they apply to "collection" first, and to "matrix"
### only via implicit or explicit coercion.
###
### the return value is (usually) a list in the style of 
### base R analysis functions, that is, without formally defined 
### components --- but the object is S4-classed to allow 
### S4-style dispatch.
###
### we use new-style classes in an old-style way: we want 
### dispatching but not slots.
####################################################

# shapes <- c (square.open = 0,
# 						 circle.open = 1,
# 						 triangle.open = 2,
# 						 diamond.open = 5,
# 						 invert.triangle.open = 6,
# 						 square.solid = 15,
# 						 circle.solid = 16,
# 						 triangle.solid = 17,
# 						 diamond.solid = 18,
# 						 square.fill = 22,
# 						 circle.fill = 21,
# 						 triangle.fill = 24,
# 						 diamond.fill = 23,
# 						 invert.triangle.fill = 25,						
# 						 plus = 3,
# 						 times = 4,
# 						 star = 8)
# shapes <- function (n) {
# shapes [switch (n,
# 								1 = "circle.solid",
# 								2 = c("square.solid", "triangle.solid"),
# 								3 = c("square.solid", "triangle.solid", "circle.solid"),
# 								4 = c("square.open", "triangle.open", "circle.open", "diamond.open"),
# 								5 = c("square.open", "triangle.open", "circle.open", "diamond.open", "invert.triangle.open"),
# 								6 = c("square.solid", "triangle.solid", "circle.solid", "square.open", "triangle.open", "circle.open"),
# 								7 = c("square.solid", "triangle.solid", "circle.solid", "square.open", "triangle.open", "circle.open", "diamond.open"),
# 								8 = c("square.solid", "triangle.solid", "circle.solid", "square.open", "triangle.open", "circle.open", "plus", "times"),
# 								9 = c("square.solid", "triangle.solid", "circle.solid", "square.open", "triangle.open", "circle.open", "plus", "times", "star"),
# 								
# 								]
# }	
# dont forget about pch='a', pch= 'b', etc.
# 	
# 
# 	if(n==1)...	c (circle = , triangle = , )
# }
# colors <- function (n) {
# 	if (n==1) "blue"
# 	else if (n=2) c (...)	
# }
# 
# 
# graphics.opts <- list (
# 	plot = list (...),
# 	points = list (...),
# 	scatterplot3d = list (...),
# 	text = list (...),
# 	boxplot = list (...),
# 	parcoord - list (...),
# 	heatmap.2 = list (...)
# 	)
# lapply (graphics.opts, formals)


####################################################
### boxplot.
### built on graphics::boxplot()
####################################################
setMethod ("boxplot", "collection", function  (x, 
																							 view = length (views (x)), ...) {
	par <- list ()
	par$las <- 2
	par$cex.axis <- 0.6
	par$main <- "annotation diversity"
# now set up graphics parameter defaults
	par <- resolveMerge (list (...), par)
	xcall (boxplot, x = x [[view]], with = par)
} )
setMethod ("boxplot", "ANY", prior ("boxplot"))

####################################################
### parallel coordinate plot.
### built on MASS::parcoord() and graphics::matplot() and matR::sigtest()
####################################################
setMethod ("parcoord", "collection", function  (x, 
																								groups = groups (x),
																								test = "Kruskal-Wallis", 
																								p.lim = 0.05, 
																								n.lim = 25, ...,
																								view = length (views (x))
																								) {
	reqPack ("MASS")
	par <- list ()
	par$main <- "parallel coordinates"
	par$var.label <- TRUE
	res <- sigtest (x, view = view, groups = groups, test = test)
	par <- resolveMerge (list (...), par)
# this is not terribly clear
# the point: plot annotations with p.value less than p.lim
# but only plot n.lim of them, at most, keeping those with smallest p.value
	which.p <- which (res$p.value < p.lim)
	if (length (which.p) > n.lim)
		which.p <- which.p [order (res$p.value [which.p]) [1:n.lim]]
# axis label orientation - a special case - also, a hack
	las.save <- par("las")
	par (las = 2)
	xcall (parcoord, x = t (x [[view]] [which.p,]), with = par)
	par (las=las.save)
} )
setMethod ("parcoord", "ANY", prior ("parcoord"))

####################################################
### principal coordinates analysis.
### built on ecodist::pco() and sactterplot3d::sactterplot3d() and base graphics
####################################################
setMethod ("pco", "collection", function (x, 
																					view = length (views (x)), 
																					components = c (1,2,3), 
																					method = "euclidean", ...) {
	reqPack ("ecodist")
	D <- as.dist (dist (x [[view]], method = method, bycol = TRUE))
	P <- ecodist::pco (D)
	scaled <- P$values / sum (P$values)
	names (scaled) <- paste ("PCO", 1:length(scaled), sep = "")
	rownames (P$vectors) <- colnames (x [[view]])                       # is this producing a reordering bug?
	P <- new ("pco", list (values = scaled, vectors = P$vectors, dist = D))

# some investigation of whether behavior here is good would be good
# elements are: labels, colors, groups(x), names(x), samples(x)
# see above: why do I use "rownames(x[[view]])"?
# fix col v. color once and for all --- it is NOT impossible
	par <- list ()
	par$main <- "principal coordinates"
	par$labels <- if (length (names (x)) != 0) names (x) else samples (x)
	if (length (groups (x)) != 0) par$labels <- paste (par$labels, " (", groups (x), ")", sep = "")
	par [c ("xlab", "ylab", if (length (components) == 3) "zlab" else NULL)] <-
		paste ("PC", components, ", R^2 = ", format (P$values [components], dig = 3), sep = "")
	col <- if (length (groups (x)) != 0) groups (x) else factor (rep (1, length (samples (x))))
	levels (col) <- colors() [sample (length (colors()), nlevels (col))]
	g <- as.character (col)
	par$pch <- 19
	par$cex <- 0.7
	par$pos <- 4

	i <- P$vectors [ ,components [1]]
	j <- P$vectors [ ,components [2]]
	k <- if (length (components) == 3) P$vectors [ ,components [3]] else NULL
	if (is.null (k)) {
		par$col <- col
		par <- resolveMerge (list (...), par)
		xcall (plot, x = i, y = j, with = par, without = c("labels","pos"))
		xcall (points, x = i, y = j, with = par, without = c("labels","pos"))
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
									without = c ("cex", "labels", "pos")) $ xyz.convert (i, j, k)
 		i <- xys$x ; j <- xys$y
	}
	text (x = i, y = j, labels = par$labels, pos = par$pos, cex = par$cex)
	invisible (P)
})
setMethod ("pco", "ANY", prior ("pco"))

####################################################
### heatmap-dendrogram analysis.
### built on gplots::heatmap.2()
####################################################
setMethod ("heatmap", "collection", function (x, 
																							view = length (views (x)), 
																							rows = TRUE, ...) {
	par <- list ()
	par$main <- "heatmap-dendrogram"
	par$col <- rgb (colorRamp(c("red", "green")) (seq(0, 1, length = 20)), max = 255)
	par$colsep <- 1:length (samples (x))
	par$labRow <- NA
	par$labCol <- if (length (names (x)) != 0) names (x) else samples (x)
	par$cexCol <- 0.6
	if (length (groups (x)) != 0) par$labCol <- paste (par$labCol, " (", groups (x), ")", sep = "")
	par$key <- FALSE
	par$trace <- "none"
	par$sepwidth <- 0.01
	par <- resolveMerge (list (...), par)

	reqPack ("gplots")
	invisible (xcall (heatmap.2, x [[view, plain = TRUE]] [rows, ], with = par))
})
setMethod ("heatmap", "ANY", prior ("heatmap"))


####################################################################################
### incomplete concept for permutation testing
####################################################################################

# summarize.dist <- function (x, groups) sapply (groups, function (g) mean (dist x [g,]))
# iterations <- randomize (cc$nsn, n = 1000, method = ..., summarize.dist, metadata (cc) ["source.and.seqtype"])
# iterations <- simplify2array (iterations)
# averages <- apply (iterations, 1, mean)
