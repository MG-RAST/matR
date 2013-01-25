
########################################################################################
### "collection" objects and related functions / methods are defined here.
### "collection" and "selection" are S4 classes, whereas "metadata" is S3.
### "views" are implemented as a character vector, not as a class at all, 
### with a set of related routines located at the end of this file.
########################################################################################

# we anticipate implementing sparse matrices in the "collection" plumbing, but for now all matrices are dense

setClass ("collection", representation (views = "list", sel = "selection"))

setMethod ("selection", "collection", function (x) selection (x@sel))
setMethod ("samples", "collection", function (x) samples (x@sel))
setMethod ("names", "collection", function (x) names (x@sel))
setMethod ("names<-", "collection", function (x, value) { names (x@sel) <- value ; x })
setMethod ("groups", "collection", function (x) groups (x@sel))
setMethod ("groups<-", "collection", function (x, value) { groups (x@sel) <- value ; x })

# views as here returned are reusable to construct new collections
setMethod ("views", "collection", function (x) lapply (x@views, view.of.matrix))
setMethod ("viewnames", "collection", function (x) names (x@views))
setMethod ("viewnames<-", "collection", function (x, value) { names (x@views) <- value ; x })

setMethod ("metadata", "collection", function (x) metadata (x@sel))

# the "$" access operator returns a plain vanilla matrix; we drop all inessential attributes
setMethod ("$", "collection", function (x, name) {
	m <- as.matrix (x@views [[name]])
	attributes (m) <- attributes (m) [c ("dim", "dimnames")]
	m
})
# users are intended to use $<- but not [[<- which is used here and defined below
setMethod ("$<-", "collection", function (x, name, value) { x [[name]] <- value ; x })

# to avoid interfering with attributes used by the Matrix package, we decide that "plain" requires "full"
setMethod ("[[", "collection", function (x, i, full = FALSE, plain = FALSE) {
	res <- if (plain || full) as.matrix (x@views [[i]]) else x@views [[i]]
	if (plain) attributes (res) <- attributes (res) [c ("dim", "dimnames")]
	res
})

setMethod ("rownames", "ANY", function (x, ...) base::rownames (x, ...))
setMethod ("rownames", "collection", function (x, view = "normed", sep = NULL) {
	if (is.null (sep))
		rownames (x [[view]])
	else if (!is.logical (sep) || sep)
		apply (attr (x [[view]], "rowhier"), 1, paste, collapse = if (is.logical (sep)) "; " else sep)
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

# an option to configure timeout per call would be nice.
# "..." should be: one or multiple named views, a single list (of named views), or empty
# method to dispatch on "ANY" allows specifying "file" and omitting "x"
setMethod ("collection", "ANY", function (x = "", ..., file = NULL)
	collection (selection (if (is.null (file)) x else readIds (file)), ...))
setMethod ("collection", "selection", function (x, ...) {
	views <- list (...)
	if (length (views) == 0) views <- default.views
	else if (is.list (views [[1]])) views <- views [[1]]

# we iterate across named views, calling the "[[<-" method to actually retrieve / construct each
	L <- list() ; length (L) <- length (names (views))
	cc <- new ("collection", views = L, sel = x)
	for (e in names (views)) cc [[e]] <- views [[e]]
	cc
} )

# if the view already exists (whether or not by the given name) we do nothing.
# (so a possible, resulting scenario is that the view exists, but not by the requested name.)
# sometimes we already have the necessary data, and the view can be computed instead of downloaded.
# the logic of that is worked out in recursion that guarantees the existence of needed "auxiliary" view(s).
setMethod ("[[<-", signature (x = "collection", i= "ANY", j = "missing", value = "character"), function (x, i, value) {
	v <- view.finish (value)
	if (length (view.grep (v, x) != 0)) return (x)

	mmm <- if (v ["entry"] %in% c ("normed.counts", "ns.counts", "ns.normed.counts")) {
		auxv <- v
		auxv ["entry"] <- switch (v ["entry"], 
														 normed.counts = "counts", 
														 ns.counts = "counts",
														 ns.normed.counts = "ns.counts")
		j <- view.grep (auxv, x)
		m <- if (length (j) != 0) x [[j]] else {
			auxv.name <- paste (auxv, collapse = ".")
			x [[auxv.name]] <- auxv
			nnn <- x [[auxv.name]]
			x@views <- x@views [-which (names (x@views) == auxv.name)]
			nnn
		}
		message ("computing:   ", view.str (v))
		switch (v ["entry"],
						normed.counts = normalize (m),
						ns.counts = remove.singletons (m),
						ns.normed.counts = normalize (m))
	}
	else {
		message ("fetching:   ", view.str (v))
		mGet ("matrix", selection (x), with = view.API.mapper (v))
	}
	x@views [[i]] <- mmm
	attributes (x@views [[i]]) <- append (attributes (x@views [[i]]), v)
	x
})

print.collection <- function (x, ...) {
	print (x@sel)
	str <- paste ("$", viewnames (x), "  (", 
								sapply (x@views, function (y) view.str (view.of.matrix (y))),
								")", sep = "")
	str <- paste (str, collapse = "\n")
	cat ("\n", str, "\n", sep = "")
}
summary.collection <- function (object, ...) print (object)
setMethod ("print", "collection", print.collection)
setMethod ("summary", "collection", summary.collection)
setMethod ("show", "collection", function (object) print.collection (object))


########################################################################################
### below are routines for handling "view"s in a standard way
########################################################################################

# assumes: a complete view
# returns: list of appropriate API parameters, primed for call to mGet
view.API.mapper <- function (v) {
	list (
		name = v ["annot"],
		result_type = c (counts = "abundance",
										 normed.counts = "abundance",
										 ns.counts = "abundance",
										 ns.normed.counts = "abundance",
										 evalue = "evalue",
										 percentid = "identity",
										 length = "length") [v ["entry"]],
		group_level = v ["level"],
		`source` = v ["source"]
	)
}

# assumes: a complete view, and a collection
# returns: numerical index of "v" within views of "cc", disregarding names of views
view.grep <- function (v, cc) which (sapply (views (cc), identical, v))

# assumes: a complete view
# returns: its printable string representation
view.str <- function (v) paste (v, collapse = " : ")

# assumes: a matrix, with attributes fully describing what view it is
# returns: the view
view.of.matrix <- function (m) unlist (attributes (m) [names (view.params)])

# assumes: a partial or complete view description (as a named character vector)
# a best-guess complete view in a standard form
view.finish <- function (spec) {
	vp <- sapply (view.params, unlist, use.names = FALSE)
	chooser <- array (0, dim = sapply (vp, length), dimnames = vp)
	
# we use a system of weights on all parameter combinations, to enable specifying views minimally
# there is still some flaky behavior, but mostly it works nicely...
# first, weight default values
	chooser ["counts",,,] <- chooser ["counts",,,] + 1
	chooser [,"function",,] <- chooser[,"function",,] + 1
	chooser [,,"species",] <- chooser [,,"species",] + 1
	chooser [,,"level3",] <- chooser [,,"level3",] + 1
	chooser [,,,"M5RNA"] <- chooser [,,,"M5RNA"] + 1
	chooser [,,,"Subsystems"] <- chooser [,,,"Subsystems"] + 1
	
# nullify impossible combinations
	chooser [,"function",view.params$level$taxa,] <- -1
	chooser [,"function",,view.params$source$rna] <- -1
	chooser [,"organism",view.params$level$`function`,] <- -1
	chooser [,"organism",,view.params$source$ontology] <- -1

# see what has been requested and nullify others
# should warn if no match to user input...
# view parameters should not be hard-coded here...
	names (spec) <- names (vp) [sapply (names (spec), pmatch, names (vp))]
	J <- sapply (names (vp), function (x) vp [[x]] [pmatch (spec [x], vp [[x]])])
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

# test below changed from <= to < per Bill Orsi bug report
# pretty sure that is correct
	if (chooser [J] < 0) stop ("cannot interpret view")
	names (J) <- names (vp)
	sapply (names (vp), function (x) vp [[x]] [J [x]])
}
