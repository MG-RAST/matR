####################################################################################
### comprehensive test suite of matR functions
###
### this requires internet connectivity (to reach the KBase API) 
### and may take quite a while to run
###
### this file serves (ok, clumsily) at once to specify matR functionality,
### and possibly as a guide to new users who don't mind wading through all the
### permutations
####################################################################################


# must review modifications of Guts object for conflicts



library(matR)
msession$async(FALSE)

####################################################################################
### test: collection()
####################################################################################
# three commands with nearly same result:
collection (guts)
collection ("4441679.3 4441680.3 4441682.3 4441695.3 4441696.3 4440463.3 4440464.3")
collection (file = "guts-IDs.txt")
# examples of modifying parameters to obtain different views on the data:
#   entry--- what returned numbers represent, or what processing has been applied to them
#   annot--- whether data pertains to functional or taxonomic annotations
#   level--- what level of the relevant annotation hierarchy is considered
#   source--- what annotation database is applied
#   hit--- what method of counting hits is used (taxonomic annotations only)
collection (guts, raw = c(entry = "count"), nrm = c("normed.counts"))
collection (guts, L1 = c(level = "level1"), L2 = c(level = "level2"), L3 = c(level = "level3"), L4 = c(level = "function"))
collection (guts, nog = c(source = "NOG"), cog = c(source = "COG"), ko = c(source = "KO"))
collection (guts, lca = c(annot = "organism", hit = "lca"), repr = c(annot = "organism", hit = "single"), all = c(annot = "organism", hit = "all"))
# all possible parameters:
view.params
# make and use a list of frequently used parameter sets:
list.of.views <- list (L1 = c(level = "level1"), L2 = c(level = "level2"), L3 = c(level = "level3"), L4 = c(level = "function"))
collection (guts, list.of.views)
# this may be the most convenient form overall:
collection (file = "guts-IDs.txt", list.of.views)


####################################################################################
### test: collection() factored by project
####################################################################################
single project
multiple projects
projects plus metagenomes
analysis of these
new function: projects(cc)


####################################################################################
### test: collection() asynchronous
####################################################################################
msession$async(TRUE)
...build & use collections, here...
...should actually run the whole test battery again, or something...
msession$async(FALSE)


####################################################################################
### test: operations on collections
####################################################################################
# static elements of a collection:
samples (Guts)
views (Guts)
rownames (Guts, view = "raw")
rownames (Guts, view = "raw", sep = FALSE)
rownames (Guts, view = "raw", sep = TRUE)
rownames (Guts, view = "raw", sep = "\t")
# modifiable elements of a collection:
names (Guts)
groups (Guts)
viewnames (Guts)
metadata (Guts)
# assignments
names (Guts) <- as.character (1:7)
groups (Guts) <- c (1,1,1,2,2,3,3)
viewnames (Guts) <- c ("a","b","c","d")
# access views, as plain matrix
Guts$raw
Guts$nrm
Guts$nsc
Guts$nsn
# access views, potentially with attributes
Guts [["raw", full = FALSE, plain = FALSE]]
Guts [["raw", full = TRUE, plain = FALSE]]
Guts [["raw", full = TRUE, plain = TRUE]]
# add new views
Guts$taxa1 <- c (annot = "organism", level = "domain")
Guts$taxa2 <- c (annot = "organism", level = "phylum")
Guts$taxa3 <- c (annot = "organism", level = "class")
Guts$taxa4 <- c (annot = "organism", level = "order")
Guts$taxa5 <- c (annot = "organism", level = "family")
Guts$taxa6 <- c (annot = "organism", level = "genus")
Guts$taxa7 <- c (annot = "organism", level = "species")
Guts$taxa8 <- c (annot = "organism", level = "strain")
# extract subcollections
Cows <- Guts [c (1,2,3)]
Fish <- Guts [c (4,5)]
Mice <- Guts [c (6,7)]


####################################################################################
### test: metadata()[], metadata()[]<-
####################################################################################
# examples of flexibility of metadata indexing:
#   single index (length one);
#   single index (length one);
#   two indices (each length one);
#   same, but return value is formatted as data.frame;
#   single index (length two); and
#   three indices (each length two)
metadata (Guts) ["0464"]
metadata (Guts) ["body product"]
metadata (Guts) ["latitude", "longitude"]
metadata (Guts) ["latitude", "longitude", bygroup=TRUE]
metadata (Guts) [c("0464", "env package.data")]
metadata (Guts) [c("0464", "PI"), c("0464","seq "), c("0464","biome")]
# new metadata fields created by assignment:
host <- factor (metadata (Guts) ["host_common_name"])
levels(host) <- c ("c", "f", "m")
metadata (Guts) ["host_abbrev"] <- as.character (host)


####################################################################################
### test sigtest()
####################################################################################
# two equivalent invocations:
gg <- c (1,1,1,2,2,1,1)
sigtest (Guts, view = "nsn", groups = gg, test = "Kruskal-Wallis")
groups (Guts) <- gg
sigtest (Guts)
# unpaired tests
sigtest (Guts, test = "t-test-unpaired")
sigtest (Guts, test = "Mann-Whitney-unpaired-Wilcoxon")
# paired tests
groups (Waters) <- c (rep (1,12), rep (2,12))
sigtest (Waters, test = "t-test-paired")
sigtest (Waters, test = "Wilcoxon-paired")
# ANOVA
groups (Guts) <- c (1,1,1,2,2,3,3)
sigtest (Guts,  test = "ANOVA-one-way")


####################################################################################
### test boxplot()
####################################################################################
# basic use:
boxplot (Guts)
boxplot (Guts, view = "raw")
# "labels" and "names" synonymous
boxplot (Guts, labels = 1:7)
boxplot (Guts, names = 1:7)
# label by metadata 
boxplot (Guts, names = "*host_common_name")
# graphical parameters as found in ?graphics::boxplot:
boxplot (Guts, main = "raw counts", horizontal = TRUE)


####################################################################################
### test pco()
####################################################################################
# basic use:
pco (Guts)
row.subset <- (sigtest (Guts) $ p.value < 0.05)
pco (Guts, view = "nsn", components = c(1,2), rows = row.subset, method = "euclidean")
# "labels" and "names" synonymous
pco (Guts, labels = 1:7)
pco (Guts, names = 1:7)
# label, color, shape:  explicitly (as above), automatically, by metadata, by groups, by sample names, or by ID
XXXXXXXXXXXXXXXXXXXXXXXXXXX
colors1 <- c ("red", "red", "red", "blue", "blue", "orange", "orange")
colors2 <- list (c ("red", "blue", "orange"))
md <- "*host_common_name"
pco (Guts, col = colors1)
pco (Guts, col = c ())
# graphical parameters as found in ?scatterplot3d::scatterplot3d and ?graphics::points:
XXXXXXXXXXXXXXXXXXXXXXXXXXX
pco (Guts, ...)


####################################################################################
### test heatmap()
####################################################################################
# basic use:
heatmap (Guts)
groups (Guts) <- c (1,1,1,2,2,3,3)
row.subset <- (sigtest (Guts) $ p.value < 0.05)
heatmap (Guts, view = "raw", rows = row.subset)
# "labels", "names", and "labCol" synonymous
heatmap (Guts, labels = 1:7)
heatmap (Guts, names = 1:7)
# label: explicitly (as above), by metadata, by groups, by sample names, or by ID
heatmap (Guts, labels = "*host_common_name")
groups (Guts) <- c ("A", "A", "A", "B", "B", "C", "C") ; heatmap (Guts) ; groups (Guts) <- NULL
heatmap (Guts)
nn <- names (Guts) ; names (Guts) <- NULL ; heatmap (Guts) ; names (Guts) <- nn
# graphical parameters as found in ?gplots::heatmap2
heatmap (Guts, main = "", colsep = "", labRow = "", labCol = "", col = )


####################################################################################
### test parcoord()
####################################################################################



####################################################################################
### test: biom conversions
####################################################################################



####################################################################################
### test view.finish()
####################################################################################

matR:::view.finish(c(entry="count"))
# next should report error but quietly does the wrong thing.  same thing in other places
matR:::view.finish(c(entry="norm"))	
matR:::view.finish(c(entry="normed.c"))
matR:::view.finish(c(entry="ns.c"))
matR:::view.finish(c(entry="ns.normed"))

matR:::view.finish(c(lev="level"))
matR:::view.finish(c(lev="level1"))
matR:::view.finish(c(lev="clas"))
matR:::view.finish()

matR:::view.finish(c(source="RDP"))
matR:::view.finish(c(source="COG"))
matR:::view.finish(c(source="KEGG"))

matR:::view.finish(c(annot="org"))
matR:::view.finish(c(annot="org", lev="cl"))
# next is error:
# matR:::view.finish(c(annot="org", lev="func"))

matR:::view.finish(c(lev="dom",annot="org",ent="ns.c"))
matR:::view.finish(c(annot="org"))
matR:::view.finish(c(entry="fun"))
matR:::view.finish(c(entry="fun"))
matR:::view.finish(c(entry="fun"))

matR:::view.finish(c(annot="org", hi="sing"))
matR:::view.finish(c(annot="org", hi="lca"))


####################################################################################
### finally, test examples (maybe the package validator does this anyway?)
####################################################################################

utils::example("collection")
utils::example("metadata")
utils::example("render")
utils::example("pco")
utils::example("heatmap")
utils::example("sigtest")

utils::example("normalize")
utils::example("randomize")
utils::example("remove.singletons")

utils::example("heatmap")
utils::example("heatmap")
utils::example("heatmap")
utils::example("heatmap")
