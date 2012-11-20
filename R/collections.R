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
	source = list (rna = c ("m5rna", "RDP", "Greengenes", "LSU", "SSU"),
								 ontology = c ("NOG", "COG", "KO", "GO", "Subsystems"),
								 protein = c ("m5nr", "SwissProt", "GenBank", "IMG", "SEED", "TrEMBL", "RefSeq", "PATRIC", 
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

setClass ("collection", representation (views = "list", sel = "selection"))

setMethod ("selection", "collection", function (x) selection (x@sel))
setMethod ("samples", "collection", function (x) samples (x@sel))
setMethod ("names", "collection", function (x) names (x@sel))
setMethod ("names<-", "collection", function (x, value) { names (x@sel) <- value ; x })
setMethod ("groups", "collection", function (x) groups (x@sel))
setMethod ("groups<-", "collection", function (x, value) { groups (x@sel) <- value ; x })

# should return list of character vectors of view attributes; return value should be usable to create a new collection
setMethod ("views", "collection", function (x) viewnames (x))
setMethod ("viewnames", "collection", function (x) names (x@views))
setMethod ("viewnames<-", "collection", function (x, value) { names (x@views) <- value ; x })

setMethod ("metadata", "collection", function (x) metadata (x@sel))

# should drop attributes but does not.
setMethod ("$", "collection", function (x, name) as.matrix (x@views [[name]]))
setMethod ("$<-", "collection", function (x, name, value) { x [[name]] <- value ; x })

setMethod ("[[", "collection", function (x, i, sparse = TRUE, plain = FALSE) {
	res <- if (sparse) x@views [[i]] else as.matrix (x@views [[i]])
# does this potentially interfere with the Matrix plumbing?  I think so...
	if (plain) for (e in setdiff (names (attributes (res)), c("dim", "dimnames"))) attr (res, e) <- NULL
	res
})

setMethod ("[", "collection", function (x, i) {
	x@sel <- x@sel [i]
# "[" for "selection" still needs to be implemented
# also, attributes of views also may need to be adjusted in the subselected object
	x@views <- lapply (x@views, `[`, i=, j=i)
	x
	})

# needs configurable timeout
# this won't work to open a file because the method won't be dispatched
setMethod ("collection", "character", function (x, file = NULL...)
	collection (selection (if (is.null (file)) x else readIDs (file)), ...))
setMethod ("collection", "selection", function (x, ...) {
	views <- list (...)
	if (length (views) == 0) views <- standard.views
	else if (is.list (views [[1]])) views <- views [[1]]
# should check here for names & reject if absent
	res <- new ("collection", views = views, sel = x)
	for (e in names (views)) res [[e]] <- views [[e]]
	res
} )

setMethod ("[[<-", signature (x = "collection", i= "ANY", j = "missing", value = "character"), function (x, i, value) {
	vp <- sapply (view.params, unlist, use.names = FALSE)
	chooser <- array (0, dim = sapply (vp, length), dimnames = vp)

# view construction logic:
# weight default values
	chooser ["count",,,] <- chooser ["count",,,] + 1
	chooser [,"function",,] <- chooser[,"function",,] + 1
	chooser [,,"species",] <- chooser [,,"species",] + 1
	chooser [,,"level3",] <- chooser [,,"level3",] + 1
	chooser [,,,"m5rna"] <- chooser [,,,"m5rna"] + 1
	chooser [,,,"Subsystems"] <- chooser [,,,"Subsystems"] + 1

# nullify impossible combinations
	chooser [,"function",view.params$level$taxa,] <- -1
	chooser [,"function",,view.params$source$rna] <- -1
	chooser [,"organism",view.params$level$func,] <- -1
	chooser [,"organism",,view.params$source$ontology] <- -1

# see what has been requested and nullify others
# should warn if no match to user input
	names (value) <- names (vp) [sapply (names (value), pmatch, names (vp))]
	J <- sapply (names (vp), function (x) vp [[x]] [pmatch (value [x], vp [[x]])])
	j <- match (J ["entry"], vp$entry)
	if (!is.na (j)) chooser [-j,,,] <- -1
	j <- match (J ["annot"], vp$annot)
	if (!is.na (j)) chooser [,-j,,] <- -1
	j <- match (J ["level"], vp$level)
	if (!is.na (j)) chooser [,,-j,] <- -1
	j <- match (J ["source"], vp$source)
	if (!is.na (j)) chooser [,,,-j] <- -1       # this needs to be prettier
	
# choose remaining combination of maximum weight
# write this in a better way.
	J <- chooser == max (chooser)
	if (sum (J) != 1) stop ("cannot interpret view")
	J <- arrayInd (which (J), dim (chooser))
	if (chooser [J] <= 0) stop ("cannot interpret view")
	names (J) <- names (vp)
	
	v <- sapply (names (vp), function (x) vp [[x]] [J [x]], simplify = FALSE)
	message ("getting view:   ", paste (unlist (v), collapse = " : "))
	s <- paste (
		"format/plain",
		"/result_column/", switch (v$entry, count = "abundance", normed = "abundance", evalue = "evalue", length = "length", percentid = "identity"),
		"/type/", v$annot,
		"/group_level/", v$level,
		"/source/", v$source, sep = "")

	x@views [[i]] <- as.matrix (mGet ("abundance", selection (x), param = s, enClass = FALSE))
#	should not call for "normed" if we have the data already.
	if (v$entry == "normed") x@views [[i]] <- normalize (x@views [[i]])
	for (e in names (v)) attr (x@views [[i]], e) <- v [[e]]
	x
})


print.collection <- function (x, ...) {
	vs <- sapply (x@views, function (v)
		paste (sapply (names (view.params), function (p) attr (v, p)), collapse = " : "))
	ps <- paste ("$", viewnames (x), "  (", vs, ")", sep = "")
	cat (paste (ps, collapse = "\n"), "\n\n")
	twoColPrint (selection (cc))
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
