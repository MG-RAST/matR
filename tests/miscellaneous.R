####################################################################################
### test suite for type conversions, view finisher, and documentation examples
####################################################################################

library (matR)

####################################################################################
### test: type conversions
####################################################################################

# it would be better to use mGet() here, but it's not working correctly yet
bb <- as (
	callRaw ("matrix/function?id=mgm4441679.3&id=mgm4441680.3&id=mgm4441682.3&result_type=abundance&group_level=level3&source=Subsystems&hit_type=na&asynchronous=0", parse = FALSE),
	"biom")

# biom ----> list
# biom ----> matrix
# biom ----> collection
as (bb, "list")
as (bb, "matrix")
as (bb, "collection")


# matrix ----> biom
# as (Guts$raw, "biom")

# matrix ----> collection
as (Guts$raw, "collection")


# collection ----> matrix
as (Guts, "matrix")

# collection ----> biom
# as (Guts, "biom")



####################################################################################
### test view.finish()
####################################################################################

matR:::view.finish(c(entry="count"))
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

matR:::view.finish(c(lev="dom",annot="org",ent="ns.c"))
matR:::view.finish(c(annot="org"))
matR:::view.finish(c(entry="fun"))
matR:::view.finish(c(entry="fun"))
matR:::view.finish(c(entry="fun"))

matR:::view.finish(c(annot="org", hi="sing"))
matR:::view.finish(c(annot="org", hi="lca"))

