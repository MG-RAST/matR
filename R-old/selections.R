
##################################################################################################################
#
##################################################################################################################

# only anticipates metagenome IDs (...and that should change...)
setMethod ("metadata", "ANY", 
					 function (x, file = NULL) {
					 	IDs <- unname (scrubIds (if (is.null (file)) x else readIds (file)))
					 	data <- lapply (sapply (IDs, function (e) mGet ("metagenome", e, verbosity = "full"), 
					 													simplify = FALSE), 
					 									unlist)
					 	items.per.sample <- sapply (data, length)


					 	mm <- unlist (unname (data))
					 	sample <- rep (IDs, items.per.sample)
					 	project.per.sample <- mm [grepl ("project1", names (mm), fixed = TRUE)]
					 	names (project.per.sample) <- IDs
					 	res <- data.frame (name = factor (names (mm)),
					 										 value = mm,
					 										 sample = factor (sample),
					 										 sample.name = factor (rep (names (x), items.per.sample)),
					 										 project = factor (project.per.sample [sample]),
					 										 stringsAsFactors = FALSE)
					 	class (res) <- c ("metadata", "data.frame")
					 	res
})

# three return types are possible:
# metadata (single index), list of metadata (multiple index), data.frame (bygroup=TRUE)
# it would be good for "names" of elements of the returned object to be set...
`[.metadata` <- function (x, i, ..., per = TRUE) {
# we accept an arbitrary number of user-specified index vectors
# bear in mind that each index (vector) is possibly of length > 1
	J <- append (list (i), list (...))
	if (length (J) == 1) {
		J <- J [[1]]
		keep <- sapply (J, grepl, rownames (x), fixed = TRUE)
		keep <- apply (keep, 1, all)
		class (x) <- "data.frame"
		res <- x [keep,]
		class (res) <- c ("metadata", "data.frame")
		res
	}
	else if (!per) lapply (J, function (j, x) x [j], x)
	else {
		
		res <- x [i, ..., per = FALSE]
	}
}		
# # for the typical user, this special case is routine:
# # insist on one item per index per metagenome (even if NA) and return a data.frame
# # (in this scenario, multiple matches to an index signals an error)
# # first we get the results as a list (see below) and then worry about creating the non-ragged data.frame
# 	res <- eval (as.call (append (list (`[`, x), J)))
# 	if (!is.list (res)) return (res)
# 	return (as.data.frame (
# 		# we turn all elements of the returned list into character vectors of the same length
# 		# and assemble these into a data.frame
# 		sapply (res, function (e) {
# 			return.rows <- character (nlevels (attr (x, "grouped")))
# 			names (return.rows) <- levels (attr (x, "grouped"))
# 			return.rows [] <- NA
# 			return.rows [as.character (attr (e, "grouped"))] <- e
# 			return.rows } ),
# 		stringsAsFactors = FALSE))
# 	}
# # next line creates a list of logical vectors, each serving to index the metadata object...
# # the j-th tells which elements of x should be returned for the j-th user-specified index vector
# keep.list <- lapply (J, sapply, grepl, names (x), fixed = TRUE)
# keep.list <- lapply (keep.list, as.matrix)
# keep.list <- apply (keep.list, 1, all)
# # next line simply extracts those elements
# # but note "unclass" is necessary to avoid calling the present function ("[.metadata") recursively
# res <- lapply ()
# res <- lapply (keep.list, function (j) unclass (x) [j])
# # the results is a list, which we collapse into a character vector under two circumstances:
# # (1) it has length one (because the user specified a single index vector)
# # (2) each element has length one (e.g., each user-specifed index selected a single element)
# 	if (length (res) == 1 || all (lapply (res, length) %in% c (0,1))) {
# 		res <- unlist (res)
# 		class (res) <- "metadata"
# 		attr (res, "grouped") <- regroup (res)
# 	}
# 	else {
# 		res <- lapply(res, `class<-`, "metadata")
# 		res <- lapply (res, function (e) { attr (e, "grouped") <- regroup (e) ; e } )
# 	}
# 	res
#}

# setMethod (`metadata<-`, "collection", 
# 					 function (x, value, seriously = FALSE) 
# 					 	if (seriously) {
# 					 		x@sel@metadata <- value
# 					 		x
# 					 	}
# 					 else stop ("prohibited replacement of metadata"))
# 
# `[<-.metadata` <- function (x, i, bygroup = TRUE, value) {
# 	mm <- value
# 	names (mm) <- unique (metadata (x) [i])
# 	mmn <- append (metadata (x), mm)
# 	class (mmn) <- "metadata"
# 	mmn
# }

print.metadata <- function (x, ...) twoColPrint (x)

summary.metadata <- function (object, ...) {
	cat (length (object), "metadata fields")
	if (is.null (attr (object, "grouped"))) cat (", ungrouped\n")
	else {
		cat (" in", nlevels (attr (object, "grouped")), "group(s):\n")
		print (table (attr (object, "grouped")))
	}
}

##################################################################################################################
### the "selection" class is basically for internal use.
### a "collection" contains a "selection" plus a list of matrices.
##################################################################################################################

setMethod ("samples", "selection", function (x) { y <- rownames (x@samples); names (y) <- x@samples$name; y })
setMethod ("projects", "selection", function (x) { y <- x@samples$project; names (y) <- x@samples$name; y })
setMethod ("names", "selection", function (x) { y <- x@samples$name; names (y) <- rownames (x@samples); y })
setMethod ("names<-", "selection", function (x, value) { x@samples$name <- value; x })
setMethod ("groups", "selection", function (x) { y <- x@samples$group; names (y) <- x@samples$name; y })
setMethod ("groups<-", "selection", function (x, value) { x@samples$group <- as.factor (value); x })
setMethod ("metadata", "selection", function (x) x@metadata)

setMethod ("[", "selection", function (x, i) {
# we ungroup everything but should inherit grouping if it exists...
# also for metadata, we take the easy way out, for now: subselecting drops all metadata

# 	x@ids <- x@ids [i]
# 	x@groups <- factor()
# 	z <- character (0)
# 	class (z) <- "metadata"
# 	x@metadata <- z
# 	attr (x@metadata, "grouped") <- NULL
# 	x@metadata.extent <- "none"
# 	x@ids.spec = character (0)
# 	x@resource.spec = character (0)
# 	x

	})

setMethod ("selection", "ANY",
					 function (x,
					 					resource = c ("metagenome", "project"),
					 					metadata.extent = c ("all", "none"),
					 					file = NULL) {
					 	x <- if (is.null (file))
					 		scrubIds (x, match.arg (resource, several.ok = TRUE))
					 	else
					 		scrubIds (readIds (file))
					 	resource <- scrapeResources (x)

# here belong: API calls to convert project IDs in x to metagenome IDs
# also: if x begins named and includes project IDs, make unique names per metagenome
# so after this, x is all metagenome IDs with unique or NULL names
					 	ll <- as.list (x)
					 	for (j in 1:length (resource))
					 		if (resource [j] == "project")
					 			ll [[j]] <- simplify2array (mGet ("project", x, verbosity = "full")$analyzed) [1,]
					 	x <- unlist (ll)

					 	metadata.extent <- match.arg (metadata.extent)
					 	if (metadata.extent == "all") {
					 		mm <- metadata (x)
					 		pp <- mm ["project.id", bygroup = TRUE]
					 	}
					 	else {
					 		mm <- data.frame()
					 		class (mm) <- c ("metadata", "data.frame")
					 		pp <- sapply (x, function (x) mGet ("metagenome", x, verbosity = "minim...")$...$project.id)
					 	}
					 	new ("selection",
					 			 samples = data.frame (name = if (is.null (names (x))) x else names (x),
					 			 											project = pp,
					 			 											group = 1, 
					 			 											row.names = x,
					 			 											stringsAsFactors = FALSE),
					 			 metadata = mm,
					 			 metadata.extent = metadata.extent)
})

print.selection <- function (x, ...) {
	if (is.null (names (x)) && 0 == length (groups (x))) cat (selection (x), "\n")
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

