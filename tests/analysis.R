####################################################################################
### test suite for high-level analysis routines
####################################################################################

library (matR)

####################################################################################
### test: boxplot()
####################################################################################
# simplest use
boxplot (Guts)
boxplot (Guts, view = "raw")

# "labels" and "names" synonymous
boxplot (Guts, labels = paste ("sample", 1:length (Guts), sep = ""))
boxplot (Guts, names = paste ("sample", 1:length (Guts), sep = ""))

# using more conventional labels
boxplot (Guts, names = projects (Guts, factor = TRUE))
boxplot (Guts, names = samples (Guts))

# label by metadata 
# boxplot (Guts, names = "*host_common_name")
# boxplot (Guts, names = "*project.id")

# using graphical parameters as found in ?graphics::boxplot
boxplot (Guts, view = "nsn", main = "no singletons, normalized",
				 las = 2, outcex = 0.2, cex.axis = 1, pch = 20, notch = TRUE)


####################################################################################
### test: pco()
####################################################################################
# simplest use, varying view and method
pco (Guts)
pco (Guts, view = "raw", method = "bray-curtis")

# varying row selection and number of components
row.subset <- (sigtest (Guts) $ p.value < 0.05)
pco (Guts, components = 1, rows = row.subset)
pco (Guts, components = c(1,2), rows = row.subset)
pco (Guts, components = c(1,2,3), rows = row.subset)

# "labels" and "names" synonymous
pco (Guts, labels = paste ("sample", 1:length (Guts), sep = ""))
pco (Guts, names = paste ("sample", 1:length (Guts), sep = ""))

# using more conventional labels
pco (Guts, names = projects (Guts, factor = TRUE))
pco (Guts, names = samples (Guts))

# label by metadata
pco (cc1, col = "*seq_meth", pch = "*project.id", labels = "")
pco (cc2, col = "*env_package.type", pch = "*project.id", labels = "")
# label, color, shape:  explicitly (as above), automatically, by metadata, by groups, by sample names, or by ID
XXXXXXXXXXXXXXXXXXXXXXXXXXX
colors1 <- c ("red", "red", "red", "blue", "blue", "orange", "orange")
colors2 <- list (c ("red", "blue", "orange"))
md <- "*host_common_name"
pco (Guts, col = colors1)
pco (Guts, col = c ())

# graphical parameters as found in ?scatterplot3d::scatterplot3d and ?graphics::points:
# pco (Guts, ...)

####################################################################################
### test: heatmap()
####################################################################################
# basic use:
heatmap (Guts)
groups (Guts) <- c (1,1,1,2,2,3,3)
row.subset <- (sigtest (Guts) $ p.value < 0.05)
heatmap (Guts, view = "raw", rows = row.subset)
# "labels", "names", and "labCol" synonymous
heatmap (Guts, labels = paste ("sample", 1:length (Guts), sep = ""))
heatmap (Guts, names = paste ("sample", 1:length (Guts), sep = ""))
# label: explicitly (as above), by metadata, by groups, by sample names, or by ID
heatmap (Guts, labels = "*host_common_name")
gg <- groups (Guts) ; groups (Guts) <- c ("A", "A", "A", "B", "B", "C", "C") ; heatmap (Guts) ; groups (Guts) <- gg
heatmap (Guts)
nn <- names (Guts) ; names (Guts) <- NULL ; heatmap (Guts) ; names (Guts) <- nn
# graphical parameters as found in ?gplots::heatmap2
heatmap (Guts, main = "", colsep = "", labRow = "", labCol = "", col = )
# use of project information in analyses:
heatmap (cc4, labels = "*project.id")
pnames <- factor (metadata (cc4) ["project.id"])
levels (pnames) <- c ("A", "B", "C")
heatmap (cc4, labels = as.character (pnames))



####################################################################################
### test: parcoord()
####################################################################################
# save groups:
gg <- groups (Guts)
# basic use: two equivalent invocations:
parcoord (Guts, view = "nrm", groups = c (1,1,1,2,2,3,3), test = "Kruskal.Wallis", p.lim = 0.05)
groups (Guts) <- c (1,1,1,2,2,3,3)
parcoord (Guts)
# plot top n significant functions, instead of functions meeting a significance threshhold
parcoord (Guts, view = "nrm", groups = c (1,1,1,2,2,3,3), test = "Kruskal.Wallis", n.lim = 25)
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# color, linetype, linewidth: explicitly, by metadata, by groups
col, lty, lwd
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# ... function labels, sample labels ... needs to be worked out
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# graphical parameters as found in ?MASS::parcoord and ?graphics::matplot
parcoord (Guts, main = "my title")
# restore groups
groups (Guts) <- gg
