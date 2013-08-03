
##################################################################################################################
### Here we establish our classes and declare functions generic as necessary.
##################################################################################################################
###
### "metadata" is a data.frame:
### rowname  value  sampleID  projectID  
### "biome"  "gut"  "mgm4441234.3"  "mgp33"
###
### "selection" contains metadata for a choice of metagenomes
### specified by another data.frame:
### rowname  sampleID  projectID  group
### "cow"  "mgm4441234.3"  "mgp12"  "1"
###
### "collection" is a "selection" plus annotation data
###
### "biom" is meant to be minimal
###
### analysis functions return lists of computation products, as base R does in many cases.
### however, our outputs are classed, to allow S4-style dispatch.
####


setOldClass ("metadata")
setClass ("selection", representation (samples = "data.frame", 
																			 metadata = "metadata", 
																			 metadata.extent = "character"))
setClass ("collection", representation (sel = "selection", 
																				views = "list"))
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
setGeneric ("projects", function (x, ...) standardGeneric ("projects"))
setGeneric ("metadata", function (x, ...) standardGeneric ("metadata"))
setGeneric ("metadata<-", function (x, value, ...) standardGeneric ("metadata<-"))
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
setGeneric ("boxplot", function (x, ...) standardGeneric ("boxplot"))
setGeneric ("parcoord", function (x, ...) standardGeneric ("parcoord"))
setGeneric ("pco", function (x, ...) standardGeneric ("pco"))
setGeneric ("heatmap", function (x, ...) standardGeneric ("heatmap"))
# setGeneric ("sigtest", function (x, ...) standardGeneric ("sigtest"))

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
