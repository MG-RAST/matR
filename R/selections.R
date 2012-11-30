
### Metadata is implemented as an S3 class "metadata".
### It is a named character vector with attributes that help interpret it.
### Attribute "grouped" factors the vector, currently only by metagenome,
### but potentially by project as well.

regroup <- function (x) {
# this is bad... a hard-coded method to inflexibly recognize a single format...
# at least, it should recognize both metagenome IDs and project IDs
	factor (substr (names (x), start = 1, stop = 12))
# and it should return NULL if length one or unfactorable
# Also, need to look into ordered and unordered factors, to make sure of no errors in subselection...
}

# dispatches on "ANY" to allow specifying "file" and omitting "x" entirely...
setMethod ("metadata", "ANY", function (x = "", file = NULL, resource = c ("project", "sample", "metagenome")) {
# currently restricted to metadata objects OF collections that are SPECIFIED by metagenome ID
# so the "resource" is ignored... should be revised...
	x <- scrubIds (if (is.null (file)) x else readIds (file))
	resource <- "metagenome"
# names given to metagenome IDs are ignored, although maybe there could be a way to allow them...
	names (x) <- x

	res <- unlist (lapply (x, mGet, resource = resource, enClass = FALSE))
	class (res) <- "metadata"
# it is understood that regroup may return NULL here, and that is ok...
	attr (res, "grouped") <- regroup (res)
	res
})

# three return types are possible:
# metadata (single index), list of metadata (multiple index), data.frame (bygroup=TRUE)
# it would be good for "names" of elements of the returned object to be set...
`[.metadata` <- function (x, i, ..., unique = FALSE, bygroup = FALSE) {
# we accept an arbitrary number of user-specified index vectors
# bear in mind that each index (vector) is possibly of length > 1
	J <- append (list (i), list (...))
# for the typical user, this special case is routine:
# insist on one item per index per metagenome (even if NA) and return a data.frame
# (in this scenario, multiple matches to an index signals an error)
	if (bygroup) {
		if (is.null (attr (x, "grouped"))) warning ("ignoring bygroup=TRUE with ungrouped metadata")
		else {
# first we get the results as a list (see below) and then worry about creating the non-ragged data.frame
			res <- eval (as.call (append (list (`[`, x), J)))
			if (!is.list (res)) return (res)
			return (as.data.frame (
# we turn all elements of the returned list into character vectors of the same length
# and assemble these into a data.frame
				sapply (res, function (e) {
					return.rows <- character (nlevels (attr (x, "grouped")))
					names (return.rows) <- levels (attr (x, "grouped"))
					return.rows [] <- NA
					return.rows [as.character (attr (e, "grouped"))] <- e
					return.rows } ),
				stringsAsFactors = FALSE))
		}
	}
# next line creates a list of logical vectors, each serving to index the metadata object...
# the j-th tells which elements of x should be returned for the j-th user-specified index vector
	keep.list <- lapply (J, function (j) apply (sapply (j, grepl, x = names (x), fixed = TRUE), 1, all))
# next line simply extracts those elements
# but note "unclass" is necessary to avoid calling the present function ("[.metadata") recursively
	res <- lapply (keep.list, function (j) unclass (x) [j])
# the results is a list, which we collapse into a character vector under two circumstances:
# (1) it has length one (because the user specified a single index vector)
# (2) each element has length one (e.g., each user-specifed index selected a single element)
	if (length (res) == 1 || all (lapply (res, length) %in% c (0,1))) {
		res <- unlist (res)
		class (res) <- "metadata"
		attr (res, "grouped") <- regroup (res)
	}
	else {
		res <- lapply(res, `class<-`, "metadata")
		res <- lapply (res, function (e) { attr (e, "grouped") <- regroup (e) ; e } )
	}
	res
}

print.metadata <- function (x, ...) twoColPrint (x)

summary.metadata <- function (object, ...) {
	cat (length (object), "metadata fields")
	if (is.null (attr (object, "grouped"))) cat (", ungrouped\n")
	else {
		cat (" in", nlevels (attr (object, "grouped")), "group(s):\n")
		print (table (attr (object, "grouped")))
	}
}

setOldClass ("metadata")


### the "selection" class is basically for internal use.
### a "collection" contains a "selection" plus a list of matrices.

setClass ("selection", representation (ids = "character", groups = "factor", metadata = "metadata",
																			 ids.spec = "character", resource.spec = "character", 
																			 metadata.extent = "character"))

setMethod ("selection", "selection", function (x) x@ids)
setMethod ("samples", "selection", function (x) x@ids)

setMethod ("names", "selection", function (x) names (x@ids))
setMethod ("names<-", "selection", function (x, value) { names (x@ids) <- value ; x })

# need to look into ordered and unordered factors, to make sure of no errors...
# should the factor have names / labels?
setMethod ("groups", "selection", function (x) x@groups)
setMethod ("groups<-", "selection", function (x, value) { x@groups <- as.factor (value) ; x })

setMethod ("[", "selection", function (x, i) {
	x@ids <- x@ids [i]
# we ungroup everything but should inherit grouping if it exists...
	x@groups <- factor()
# also for metadata, we take the easy way out, for now...
# subselecting drops all metadata
	z <- character (0)
	class (z) <- "metadata"
	x@metadata <- z
	attr (x@metadata, "grouped") <- NULL
	x@metadata.extent <- "none"
# when "ids.spec" and "resource.spec" become actually used, they will need to be handled more delicately here...
	x@ids.spec = character (0)
	x@resource.spec = character (0)
	x
	})

setMethod ("metadata", "selection", function (x) x@metadata)

setMethod ("selection", "character", function (x, resource = c ("project", "sample", "metagenome"), 
																							 metadata.extent = c ("none", "asis", "min", "all")) {
### this function (like metadata above) has been prototyped to allow for future development,
### and in this version, ignores certain parameters...
	x <- scrubIds (x)
	resource <- "metagenome"
	if (!identical (metadata.extent, "none")) metadata.extent <- "all"

	new ("selection", ids = x, groups = factor(),
			 metadata = switch (metadata.extent, none = character (0), metadata (x)),
			 ids.spec = x, resource.spec = resource, metadata.extent = metadata.extent)
})
setMethod ("selection", "numeric", getMethod ("selection", "character"))

print.selection <- function (x, ...) {
	if (is.null (names (x)) && 0 == length (groups (x))) print (selection (x))
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