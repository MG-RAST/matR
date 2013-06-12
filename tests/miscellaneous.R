####################################################################################
### test suite for type conversions, view finisher, and documentation examples
####################################################################################

library (matR)

####################################################################################
### test: type conversions
####################################################################################

XXXXXXXXXXXXXXXXXXXXXXXXXXX
bb <- mGet ("matrix", guts, parse = FALSE, enClass = FALSE)
class (bb) <- "biom"

# biom ----> list
as.list (biom)

# collection, biom ----> matrix
as.matrix (Guts)
as.matrix (bb)

# biom, matrix ----> collection
as.collection (bb)
as.collection (Guts$raw)

# collection, matrix ----> biom
as.biom (Guts)
as.biom (Guts$raw)
XXXXXXXXXXXXXXXXXXXXXXXXXXX

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
