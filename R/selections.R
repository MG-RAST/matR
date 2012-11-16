
### Metadata is implemented as an S3 class "metadata".
### It is a named character vector with attributes that help interpret it.
### Attribute "grouped" factors the vector, currently only by metagenome,
### but potentially by project as well.

regroup <- function (x) {
# this only works for metagenomes and is inflexible
# it should have an specifically recognize metagenome ID and project ID at least
	factor (substr (names (x), start = 1, stop = 12))
# return NULL if length one or unfactorable
}

setMethod ("metadata", "character", function (x, resource = c ("project", "sample", "metagenome")) {
# restriction to metadata objects OF collections, SPECIFIED by metagenome ID
# needs to be lifted:
	x <- scrubIds (x)
	names (x) <- x
	resource <- "metagenome"

	res <- unlist (lapply (x, mGet, resource = resource, enClass = FALSE))
	class (res) <- "metadata"
# if regroup returns NULL that is ok:
	attr (res, "grouped") <- regroup (res)
	res
})

# three return types are possible - metadata, list of metadata, data.frame (if bygroup = TRUE)
`[.metadata` <- function (x, i, ..., unique = FALSE, bygroup = TRUE) {
	L <- append (list (i), list (...))
	if (bygroup) {
		if (is.null (attr (x, "grouped"))) warning ("bygroup=TRUE with ungrouped metadata")
		else {
			
		}
	}
	keep <- lapply (L, function (i) apply (sapply (i, grepl, x = names (x), fixed = TRUE), 1, all))
	res <- lapply (L, function (i) unclass (x) [i])
	if (length (res) == 1 || all (lapply (res, length) %in% c (0,1))) {
		res <- unlist (res)
		class (res) <- "metadata"
		attr (res, "grouped") <- regroup (res)
		res
	}
	else {
		res <- lapply(res, `class<-`, "metadata")
# ... not sure here...		lapply (res, 'attr<-', which = "grouped", value = factor (regroup ()))
	}
# must adjust factor in return object
}

print.metadata <- function (x, ...) twoColPrint (x)

summary.metadata <- function (x, ...) {
	cat (length (x), "metadata fields")
	if (is.null (attr (x, "grouped"))) cat (", ungrouped\n")
	else {
		cat (" in", nlevels (attr (x, "grouped")), "group(s):\n")
		print (table (attr (x, "grouped")))
	}
}

setOldClass ("metadata")

###
### the "selection" class exists to enable flexible
### specification of a group of metagenomes to study.
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

setClass ("selection", representation (ids = "character", groups = "factor", metadata = "metadata",
																			 ids.spec = "character", resource.spec = "character", 
																			 metadata.extent = "character"))

setMethod ("selection", "selection", function (x) x@ids)
setMethod ("samples", "selection", function (x) x@ids)

setMethod ("names", "selection", function (x) names (x@ids))
setMethod ("names<-", "selection", function (x, value) { names (x@ids) <- value ; x })

setMethod ("groups", "selection", function (x) x@groups)
setMethod ("groups<-", "selection", function (x, value) { x@groups <- as.factor (value) ; x })

setMethod ("[", "selection", function (x, i) {
	# ... this needs completion
})

setMethod ("metadata", "selection", function (x) x@metadata)

setMethod ("selection", "character", function (x, resource = c ("project", "sample", "metagenome"), 
																							 metadata.extent = c ("none", "asis", "min", "all")) {
### again this function has been prototyped to allow for future development
	x <- scrubIds (x)
	resource <- "metagenome"
	if (!identical (metadata.extent, "none")) metadata.extent <- "all"

	new ("selection", ids = x, groups = factor(),
			 metadata = switch (metadata.extent, none = character (0), metadata (x)),
			 ids.spec = x, resource.spec = resource, metadata.extent = metadata.extent)
})
setMethod ("selection", "numeric", getMethod ("selection", "character"))

print.selection <- function (x, ...) {
	if (is.null (names (x)) && 0 == length(groups (x))) print (selection (x))
	else {
		s <- selection (x)
		n <- names (x)
		g <- if (0 == length (groups (x))) NULL else paste ("(", groups (x), ")", sep = "")
		names (s) <- if (is.null (n)) g else if (is.null (g)) n else paste (n, g)
		twoColPrint (s)
	}
}
summary.selection <- function (object, ...) print.selection (object)
setMethod ("print", "selection", print.selection)
setMethod ("summary", "selection", summary.selection)
setMethod ("show", "selection", function (object) print.selection (object))

