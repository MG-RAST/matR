
# these data need to be available to matR and to the user.
# cannot go in pkgdata.R for unclear reasons.

view.params <- list (
	entry = c ("count", "normed", "evalue", "length", "percentid"),
	annot = c ("function", "organism"),
	level = list (taxa = c ("domain", "phylum", "class", "order", "family", "genus", "species", "strain"),
								func = c ("level1", "level2", "level3", "function")),
	source = list (rna = c ("m5rna", "RDP", "Greengenes", "LSU", "SSU"),
								 ontology = c ("NOG", "COG", "KO", "GO", "Subsystems"),
								 protein = c ("m5nr", "SwissProt", "GenBank", "IMG", "SEED", "TrEMBL", "RefSeq", "PATRIC", 
								 						 "eggNOG", "KEGG")))

standardViews <- list (
	count = c (entry = "count"),
	normed = c (entry = "normed"))

allViews <- list (
	count = c (entry = "count"),
	normed = c (entry = "normed"),
	evalue = c (entry = "evalue"),
	length = c (entry = "length"),
	percentid = c (entry = "percentid"))


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
# M$newview <- view ("anno")
#################################################
setClass ("collection", representation ("namedList", sel = "selection"), contains = "namedList")

setMethod ("samples", "collection", function (x) samples (x@sel))
setMethod ("names", "collection", function (x) names (x@sel))
setMethod ("names<-", "collection", function (x, value) { names (x@sel) <- value ; x })
setMethod ("groups", "collection", function (x) groups (x@sel))
setMethod ("groups<-", "collection", function (x, value) { groups (x@sel) <- value ; x })

setMethod ("views", "collection", function (x) viewnames (x))				# needs attention here
setMethod ("viewnames", "collection", function (x) names (x@.Data))
setMethod ("viewnames<-", "collection", function (x, value) { names (x@.Data) <- value ; x })

setMethod ("metadata", "collection", function (x) metadata (x@sel))
setMethod ("selection", "collection", function (x) selection (x@sel))

setMethod ("$", "collection", function (x, name) as.matrix (x [[name]]))
setMethod ("$<-", "collection", function (x, name, value) { x [[name]] <- value ; x })

setMethod ("[[", "collection", function (x, i, sparse = TRUE, plain = FALSE) {
	res <- if (sparse) x@.Data [[i]] else as.matrix (x@.Data [[i]])
	if (plain) for (e in setdiff (names (attributes (res)), c("dim", "dimnames"))) attr (res, e) <- NULL   # does this potentially interfere with the Matrix plumbing?
	res
})

setMethod ("[", "collection", function (x, i) {
### superstition gives rise to variable I just below
	I <- i
	x@sel <- x@sel [i]
	x@.Data <- lapply (x@.Data, `[`, i =, j = I)
	x
	})

setMethod ("collection", "character", function (x, ...) collection (selection (x), ...))
setMethod ("collection", "selection", function (x, ...) {
	views <- unlist (list (...))
	if (length (views) == 0) views <- standardViews

	res <- new ("collection", list(), sel = x)
	length (res @ .Data) <- length (views)
#	viewnames (res) <- names (views)
	names (res @ .Data) <- names (views)

#	for (e in names (views)) res [[e]] <- views [[e]]
	res
} )


print.collection <- function (x, ...) {
	print (x@sel)
	cat ("\n")
# for (j in 1:length (x) { cat ("$", names (x@views) [j], " :: ", sep = "") ; print (x@views [[j]]) }
}
summary.collection <- function (object, ...) print (object)
setMethod ("print", "collection", print.collection)
setMethod ("summary", "collection", summary.collection)
setMethod ("show", "collection", function (object) print.collection (object))


setMethod ("[[<-", signature (x = "collection", value = "character"), function (x, i, value) {
	vp <- sapply (view.params, unlist, use.names = FALSE)
	chooser <- array (0, dim = sapply (vp, length), dimnames = vp)

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
	
#	should not call for "normed" if we have the data already...
	x @ .Data [[i]] <- as.matrix (mGet ("abundance", selection (x), param = s, enClass = FALSE))
	#if (v$entry == "normed") x @ .Data [[i]] <- normalize (x [[i]])
	#for (e in names (y)) attr (x @ .Data [[i]], e) <- y [[e]]
	x
})



#################################################
### Class components:
### definition, inheritance, construction, methods, print 
#################################################

