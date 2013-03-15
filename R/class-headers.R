
##################################################################################################################
### Here we establish our classes and declare functions generic as necessary.
##################################################################################################################
###
### "metadata" is an S3 class, a named character vector.
### the attribute "grouped" factors the vector, 
### currently only by metagenome, but potentially by project as well.
###
### "selection" enables flexible specification of metagenomes (user should not see this class)
###
### "collection" is a "selection" plus a list of matrices.
###
### "biom" is meant to be minimal
###
### analysis functions return lists of computation products, as R does in many cases.
### however, these outputs are classed, to allow S4-style dispatch.
####

setOldClass ("metadata")
setClass ("selection", representation (ids = "character", 
																			 groups = "factor", 
																			 metadata = "metadata",
																			 ids.spec = "character",
																			 resource.spec = "character", 
																			 metadata.extent = "character"))


# setClass ("selection", representation (metagenomes = "character",  ..... c("mgm4441980.3", "mgm4441980.3", "mgm4441980.3", ....)
# 																			 projects = "factor", ...... factor("mgp10", "mgp10", "mgp12", "mgp12", "mgp12", ....)
# 																			 groups = "factor", ..... factor(1,1,1,2,2,....)
# 																			 metadata = "metadata",
# 																			 metadata.extent = "numeric"))     # code 0 (none), 1 (minimal),2 (full)


setClass ("collection", representation (views = "list", sel = "selection"))
setClass ("biom", repr = NULL, contains = "character")
setClass ("pco", repr = NULL, contains = "list")
setClass ("heatmap", repr = NULL, contains = "list")
setClass ("sigtest", repr = NULL, contains = "list")
setClassUnion ("analysis", c ("pco", "heatmap", "sigtest"))


##########################################################################################
### we find S4 dispatching useful, so define many generic functions
##########################################################################################

setGeneric ("print")
setGeneric ("summary")

# selections.R
setGeneric ("samples", function (x, ...) standardGeneric ("samples"))
setGeneric ("metadata", function (x, ...) standardGeneric ("metadata"))
setGeneric ("names")
setGeneric ("names<-")
setGeneric ("groups", function (x, ...) standardGeneric ("groups"))
setGeneric ("groups<-", function (x, value) standardGeneric ("groups<-"))
setGeneric ("selection", function (x, ...) standardGeneric ("selection"))

# collections.R
setGeneric ("views", function (x, ...) standardGeneric ("views"))
setGeneric ("viewnames", function (x, ...) standardGeneric ("viewnames"))
setGeneric ("viewnames<-", function (x, value) standardGeneric ("viewnames<-"))
setGeneric ("rownames", function (x, ...) standardGeneric ("rownames"))
setGeneric ("collection", function (x, ...) standardGeneric ("collection"))

# analysis-utils.R
setGeneric ("dist", function (x, ...) standardGeneric ("dist"))

# analysis.R
setGeneric ("pco", function (x, ...) standardGeneric ("pco"))
setGeneric ("heatmap", function (x, ...) standardGeneric ("heatmap"))
setGeneric ("sigtest", function (x, ...) standardGeneric ("sigtest"))
setGeneric ("parcoord", function (x, ...) standardGeneric ("parcoord"))

# render.R
setGeneric ("render", function (x, ...) standardGeneric ("render"))

# class-coercions.R
setGeneric ("asFile", function (x, file, ...) standardGeneric ("asFile"))





### more about selections...
###
### for instance, it should accept two project ids
### as specification of all subordinate metagenome ids,
### and remember that the metagenomes belong to two distinct groups.
###
### USAGES:
###		sel <- selection ("mgm111111.3", meta = "all")
###		x <- selection (ids, meta = "all")
###		metadata (x) $ env_package $ ...
###
### DESIDERATA:
###		handle specification by mgm, mgs, mgp (mgl?) arbitrarily (as intended in the design)
### the ids specified for construction: any of mgm, mgs, mgp, (mgl?)
### resource type, corresponding to each id
### representation of the selection content.  In the initial implementation, this
### just means all metagenome ids.  Later, some kind of relational tree
### complete metadata is always retrieved, but different approaches to metadata
### redundancy are possible: "none", "asis", "min", "full".  to be implemented
###
### specification of selections by project & sample id is not yet implemented
###
