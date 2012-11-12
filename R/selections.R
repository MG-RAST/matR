

#################################################
### METADATA
###
### our working assumption is that the form of
### metadata will continue to be highly variable
### for the foreseeable future, so that providing
### metadata access via a very general structure
### is the best option.
###
### a metadata object is an object of class "rlist":
### a uniform recursive list structure with atomic 
### elements being strictly character-mode and length-one
###
### This makes element access straightforward, e.g.:
###		m$metadata$env_package$habitat
###
### list elements within an rlist object are also
### rlist, and a list of rlist objects naturally
### is itself an rlist object.
###
### USAGES:
###		new ("rlist", <existing list-like object>)
###		new ("rlist", mGet (<resource>, id, enClass = FALSE))
###		md <- metadata ("4441872.3", resource = "metagenome")
###		md $ metadata $ env_package $ data $ biome
###		ids <- c ("4441872.3", "4442034.3", "4448888.3", "4449999.3")
###		metadataList <- metadata (ids, resource = "metagenome")
###		metadataList $ 4441872.3 $ metadata $ env_package $ data $ biome
###
### Use <tab> liberally when exploring metadata.  Partial matching makes it easy to find the fields you want.
###		md <- metadata ("4443360.3")
###		md$metadata$env_package$data$chlo<tab> completes the word "chlorophyll"
### Use an index vector to quickly retrieve important metadata fields.
###		i <- c ("metadata","env_package", "data", "chlorophyll")
###		md [[i]]
###
### DESIDERATA:
###		shortcut access to deeply nested elements
###		auto-detect type according to prefix: mgp, mgl, mgs, mgm
###		recycle "resource" to allow simultaneous retrieval of different types of metadata
###		method for type "connection", to initialize from file
###		the right answer here is to vectorize mGet for parameter "resource"
#################################################
rlistPr <- function (x, ...) listPrinter (x)
rlistSh <- function (object) rlistPr (object)
print.rlist <- rlistPr
summary.rlist <- function (object, ...) rlistSh (object)

setClass ("rlist",
					representation = NULL,
					contains = "namedList",
					prototype = prototype (list ()))
setMethod ("initialize", "rlist",
					 function (.Object, ...) {
					 	L <- list (...)
					 	.Object @ .Data <-
					 		if (length (L) == 0) list ()
					 	else if (length (L) == 1) listify (L [[1]])
					 	else listify (L)
					 	.Object } )
setMethod ("$", "rlist",
					 function (x, name) {
					 	y <- unclass (x) [[name]]
					 	if (is (y, "list")) new ("rlist", y) else y
					 } )
setMethod ("[[", "rlist",
					 function (x, i, j, ..., exact = TRUE) { 
					 	y <- unclass (x) [[i]]
					 	if (is (y, "list")) new ("rlist", y) else y
					 } )
setMethod ("[", signature (x = "rlist", i = "list"), 
					 function (x, i) 
					 	lapply (X = i, FUN = function (i, x) x [[i]], x))
setMethod ("[", signature (x = "rlist", j = "character"),
					 function (x, i, j)
					 	lapply(lapply (X = x, FUN = function (y, j) y [[j]], j),
					 				 function (x) if (is.null (x)) NA else x))
setMethod ("print", "rlist", rlistPr)
setMethod ("summary", "rlist", function (object, ...) rlistSh (object))
setMethod ("show", "rlist", rlistSh)

### the intended user-facing construction function:
setMethod ("metadata", "character",
					 function (x, resource = "metagenome") {
					 	# FIX: this just is not right
					 	x <- chomp (x)
					 	resources <- chomp (resource)
					 	if (length (x) > 1) {
					 		L <- list ()
					 		for (j in 1:length (x)) L [[x [j]]] <- mGet (resource, x [j], enClass = FALSE)
					 		names (L) <- x
					 		new ("rlist", L)
					 	}
					 	else new ("rlist", mGet (resource, x, enClass = FALSE))
					 } )




#################################################
### SELECTION FOR ANALYSIS
### the point of this class is to enable flexible
### specification of a group of metagenomes to study.
### for instance, it should accept two project ids
### as specification of all subordinate metagenome ids,
### and remember that the metagenomes belong to 
### two distinct groups.
###
### resource is among "project", "sample", "metagenome"
### tagging is among "none", "asis", "min", "all"
###
### USAGES:
###		sel <- selection ("mgm111111.3", meta = "all")
###		x <- selection (ids, meta = "all")
###		metadata (x) $ env_package $ ...
###
### DESIDERATA:
###		handle specification by mgm, mgs, mgp (mgl?) arbitrarily (as intended in the design)
# the ids specified for construction: any of mgm, mgs, mgp, (mgl?)
# resource type, corresponding to each id
# representation of the selection content.  In the initial implementation, this
# just means all metagenome ids.  Later, some kind of relational tree
# complete metadata is always retrieved, but different approaches to metadata
# redundancy are possible: "none", "asis", "min", "full".  to be implemented
#################################################
selPr <- function (x, ... ) {
	if (all (x@ids == names(x))) {
		s <- x@ids
		names (s) <- NULL
		print (s)
	}
	else print (x@ids)
}
selSh <- function (object) selPr (object)
print.selection <- selPr
summary.selection <- function (object, ...) selSh (object)

setClass ("selection", 
					representation (
						ids = "character",
						groups = "factor",
						metadata = "rlist",
						ids.spec = "character",
						resources.spec = "character",
						metadata.extent = "character"))
setMethod ("names", "selection", function (x) names (x@ids))
setMethod ("groups", "selection", function (x) x@groups)
setMethod ("metadata", "selection", function (x) x@metadata )
setMethod ("print", "selection", selPr)
setMethod ("summary", "selection", function (object, ...) selSh (object))
setMethod ("show", "selection", selSh)
setMethod ("names<-", "selection",
					 function (x, value) {
					 	names (x@ids) <- value
					 	if (length (x@groups) > 0) names (x@groups) <- value
					 	x
					 })
setMethod ("groups<-", "selection", 
					 function (x, value) {
					 	x@groups <- as.factor (value)
					 	names (x@groups) <- names (x@ids)
					 	x
					 })
### specification of selections by project & sample id is not yet implemented
setMethod ("selection", "character",
					 function (x, resources = "metagenome", metadata.extent = "asis") {
					 	resources <- chomp (resources)
					 	ids <- scrubIds (x, resources)
					 	if (is.null (names (ids))) names (ids) <- ids
					 	metadata <- switch (metadata.extent,
					 											all = metadata (ids),
					 											asis = metadata (ids),
					 											smart = metadata (ids),
					 											min = metadata (ids),
					 											none = new ("rlist"))
					 	new ("selection",
					 			 ids = ids,
					 			 groups = as.factor (rep (1, length (ids))),
					 			 metadata = metadata,
					 			 ids.spec = ids,
					 			 resources.spec = resources,
					 			 metadata.extent = metadata.extent)
					 })
setMethod ("selection", "numeric", getMethod ("selection", "character"))


