

# need routines to handle view description vectors
# * create printable string (and use uniformly)
# * retrieve as vector from matrix attributes

# These data need to be available to matR and to the user.
# They should go in data/ but objects from source files there 
# are not accessible to package code (an empirical discovery).

view.params <- list (
	entry = c ("count", "normed", "evalue", "length", "percentid"),
	annot = c ("function", "organism"),
	level = list (taxa = c ("domain", "phylum", "class", "order", "family", "genus", "species", "strain"),
								func = c ("level1", "level2", "level3", "function")),
	source = list (rna = c ("M5RNA", "RDP", "Greengenes", "LSU", "SSU"),
								 ontology = c ("NOG", "COG", "KO", "Subsystems"),
								 protein = c ("M5NR", "SwissProt", "GenBank", "IMG", "SEED", "TrEMBL", "RefSeq", "PATRIC", 
								 						 "eggNOG", "KEGG")))

standard.views <- list (
	count = c (entry = "count"),
	normed = c (entry = "normed"))
all.views <- list (
	count = c (entry = "count"),
	normed = c (entry = "normed"),
	evalue = c (entry = "evalue"),
	length = c (entry = "length"),
	percentid = c (entry = "percentid"))

# assumes: a valid view
# returns: list of API parameters, primed for call to mGet
API.mapper.matrix <- function (view) {
	list (
		name = view ["annot"],
		result_type = c (count = "abundance", evalue = "evalue", percentid = "identity", length = "length") [view ["entry"]],
		group_level = view ["level"],
		source = view ["source"]
	)
}

setClass ("collection", representation (views = "list", sel = "selection"))

setMethod ("selection", "collection", function (x) selection (x@sel))
setMethod ("samples", "collection", function (x) samples (x@sel))
setMethod ("names", "collection", function (x) names (x@sel))
setMethod ("names<-", "collection", function (x, value) { names (x@sel) <- value ; x })
setMethod ("groups", "collection", function (x) groups (x@sel))
setMethod ("groups<-", "collection", function (x, value) { groups (x@sel) <- value ; x })

# the idea is:  the character vectors returned by this method should, themselves, 
# be usable as view descriptions to construct new collections
setMethod ("views", "collection", function (x) 
	lapply (x@views, function (e) unlist (attributes (e) [names (view.params)])))					 
setMethod ("viewnames", "collection", function (x) names (x@views))
setMethod ("viewnames<-", "collection", function (x, value) { names (x@views) <- value ; x })

setMethod ("metadata", "collection", function (x) metadata (x@sel))

# this access operator dependably returns a plain vanilla matrix
setMethod ("$", "collection", function (x, name) {
	res <- as.matrix (x@views [[name]])
# here we drop all attributes besides "dim" and "dimnames"
	attributes (res) <- attributes (res) [c ("dim", "dimnames")]
	res
})
# users are intended to use $<- but not [[<-
setMethod ("$<-", "collection", function (x, name, value) { x [[name]] <- value ; x })

# to avoid interfering with attributes set in Matrix package plumbing, we decide that plain requires full
setMethod ("[[", "collection", function (x, i, full = FALSE, plain = FALSE) {
	res <- if (plain || full) as.matrix (x@views [[i]]) else x@views [[i]]
	if (plain) attributes (res) <- attributes (res) [c ("dim", "dimnames")]
	res
})

setMethod ("rownames", "ANY", function (x, ...) base::rownames (x, ...))
setMethod ("rownames", "collection", function (x, view = "normed", cat = NULL) {
	if (is.null (cat))
		rownames (x [[view]])
	else if (as.logical (cat))
		apply (attr (x [[view]], "rowhier"), 1, paste, collapse = if (is.logical (cat)) "; " else cat)
	else attr (x [[view]], "rowhier")
})

setMethod ("[", "collection", function (x, i) {
	x@sel <- x@sel [i]
# need to keep an eye on this:
# attributes in the subselected object may need to be adjusted, for instance "rowgroups" when implemented...
# and this must work correctly for different kinds of index...
# want to use: numeric, logical, sample names, sample IDs, groups...
# for that matter, need to know exactly when/how column names in the matrices are assigned...
# **** the following drops attributes; this is not ok...
	newviews <- lapply (x@views, `[`, i=, j=i)
	names (newviews) <- names (x@views)
	for (e in names (newviews)) {
		a <- attributes (x@views [[e]])
		a [c ("dim", "dimnames")] <- NULL
		attributes (newviews [[e]]) [names (a)] <- a
	}
	x@views <- newviews
	x
	})

# dispatches on "ANY" to allow specifying "file" and omitting "x"
setMethod ("collection", "ANY", function (x = "", ..., file = NULL)
	collection (selection (if (is.null (file)) x else readIds (file)), ...))
# needs option to configure timeout...
setMethod ("collection", "selection", function (x, ...) {
	views <- list (...)
	if (length (views) == 0) views <- standard.views
	else if (is.list (views [[1]])) views <- views [[1]]
	if (is.null (names (views))) stop ("views must have names")
# what follows is a bit tricky, but seems ok
# the list of views is created using the views themselves as entries, then the matrices are written over
	res <- new ("collection", views = views, sel = x)
	for (e in names (views)) res [[e]] <- views [[e]]
	res
} )

setMethod ("[[<-", signature (x = "collection", i= "ANY", j = "missing", value = "character"), function (x, i, value) {
	vp <- sapply (view.params, unlist, use.names = FALSE)
	chooser <- array (0, dim = sapply (vp, length), dimnames = vp)

# we use a system of weights on all parameter combinations, to enable specifying views minimally
# there is still some flaky behavior, but mostly it works nicely...
# first, weight default values
	chooser ["count",,,] <- chooser ["count",,,] + 1
	chooser [,"function",,] <- chooser[,"function",,] + 1
	chooser [,,"species",] <- chooser [,,"species",] + 1
	chooser [,,"level3",] <- chooser [,,"level3",] + 1
	chooser [,,,"M5RNA"] <- chooser [,,,"M5RNA"] + 1
	chooser [,,,"Subsystems"] <- chooser [,,,"Subsystems"] + 1

# nullify impossible combinations
	chooser [,"function",view.params$level$taxa,] <- -1
	chooser [,"function",,view.params$source$rna] <- -1
	chooser [,"organism",view.params$level$func,] <- -1
	chooser [,"organism",,view.params$source$ontology] <- -1

# see what has been requested and nullify others
# should warn if no match to user input...
# view parameters should not be hard-coded here...
	names (value) <- names (vp) [sapply (names (value), pmatch, names (vp))]
	J <- sapply (names (vp), function (x) vp [[x]] [pmatch (value [x], vp [[x]])])
	j <- match (J ["entry"], vp$entry)
	if (!is.na (j)) chooser [-j,,,] <- -1
	j <- match (J ["annot"], vp$annot)
	if (!is.na (j)) chooser [,-j,,] <- -1
	j <- match (J ["level"], vp$level)
	if (!is.na (j)) chooser [,,-j,] <- -1
	j <- match (J ["source"], vp$source)
	if (!is.na (j)) chooser [,,,-j] <- -1
	
# choose remaining combination of maximum weight
# write this in a better way...
	J <- chooser == max (chooser)
	if (sum (J) != 1) stop ("cannot interpret view")
	J <- arrayInd (which (J), dim (chooser))
	if (chooser [J] <= 0) stop ("cannot interpret view")
	names (J) <- names (vp)
	
	v <- sapply (names (vp), function (x) vp [[x]] [J [x]], simplify = FALSE)

#	for "normed" view (and others, later) we check if we have the raw data already
	
	except.entry <- setdiff (names (view.params), "entry")
	if (v$entry == "normed" && any (j <- sapply (x@views, 
																							 function (e) 
																							 	identical (attributes (e) [except.entry], v [except.entry])))) {
		message ("calculating:   ", paste (unlist (v), collapse = " : "))
		x@views [[i]] <- normalize (x@views [[which (j)]])
	}
	else {
		normed <- FALSE
		if (v$entry == "normed") {
			v$entry <- "count"
			normed <- TRUE
		}
		message ("fetching:   ", paste (unlist (v), collapse = " : "))
# 		s <- paste ("format/plain",
# 								"/result_column/", switch (v$entry, 
# 																					 count = "abundance", 
# 																					 normed = "abundance", 
# 																					 evalue = "evalue", 
# 																					 length = "length", 
# 																					 percentid = "identity"),
# 								"/type/", v$annot,
# 								"/group_level/", v$level,
# 								"/source/", v$source, 
# 								sep = "")
# # here eventually should go support for storing matrices sparsely...
# 		x@views [[i]] <- as.matrix (mGet ("abundance", selection (x), param = s, enClass = FALSE))

		x@views [[i]] <- mGet ("matrix", selection (x), with = API.mapper.matrix (unlist (v)))

		if (normed) {
			x@views [[i]] <- normalize (x@views [[i]])
			v$entry <- "normed"
		}
	}
	attributes (x@views [[i]]) <- append (attributes (x@views [[i]]), v)
	x
})

print.collection <- function (x, ...) {
	vs <- sapply (x@views, function (v)
		paste (unlist (attributes (v)) [names (view.params)], collapse = " : "))
	ps <- paste ("$", viewnames (x), "  (", vs, ")", sep = "")
	cat (paste (ps, collapse = "\n"), "\n\n")
	print (x@sel)
}
summary.collection <- function (object, ...) print (object)
setMethod ("print", "collection", print.collection)
setMethod ("summary", "collection", summary.collection)
setMethod ("show", "collection", function (object) print.collection (object))




#################################################
### Class components:
### definition, inheritance, construction, methods, print 
#################################################


#################################################
### MATRIX INTERACTION 
###
### this class is called "collection" to recall the concept familiar from the existing UI
###
### USAGES:
###		sel <- selection ("mgm111111.3")
###		M <- collection (sel, g = view ("Greengenes"), r = view (source = "RDP"), full = view (source = "m5rna"))
###		M$g ; M$r ; M[[3]]						(these are "Matrix")
###		M$view(name = "new_view", annotation = "organism", level = "phylum")
###		metadata(M)
###		views(M)
### user-facing construction functions and manipulations:
###  	M <- collection (<selection>, views)
###		M <- collection (<character vector of ids>, views)
###		M <- collection (<file>, views)
###		md <- metadata (M)
### collection (file ("ids.txt"))
### M$newview <- view ("anno")
#################################################
