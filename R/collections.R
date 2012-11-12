
# these data need to be available to matR and to the user.
# cannot go in pkgdata.R for unclear reasons.
sources <- list (
	m5rna = "m5rna", rdp = "RDP", greengenes = "Greengenes", lsu = "LSU", ssu = "SSU",				# rna
	nog = "NOG", cog = "COG", ko = "KO", go = "GO", subsystems = "Subsystems",						# ontology
	m5nr = "m5nr", swissprot = "SwissProt", genbank = "GenBank", img = "IMG", seed = "SEED",		# protein
	TrEMBL = "TrEMBL", refseq = "RefSeq", patric = "PATRIC", eggnog = "eggNOG", kegg = "KEGG")
ofs <- c ("count", "normed", "evalue", "length", "percentid")
annotations <- c ("function", "organism")
orgLevels <- c ("domain", "phylum", "class", "order", "family", "genus", "species", "strain")
funcLevels <- c ("level1", "level2", "level3", "function")

#################################################
### Class components:
### definition, inheritance, construction, methods, print 
#################################################


#################################################
### MATRIX
### (1) based for now on the "Matrix" class; maybe to be reimplemented with an underlying BIOM object
### (2) the advantages of defining this class (instead of just using a "Matrix") are not yet clear.
###		But I am close to certain it will prove a good idea.  the (unused) hierarchy slot, for
###		instance will find a purpose.
###
### DESIDERATA:
###		retrieve the entire hierarchy into the hierarchy field, with short names in rownames
#################################################
matrixPr <- function (x, ...) matrixPrinter (x)
matrixSh <- function (object) matrixPr (object)
print.mmatrix <- matrixPr
summary.mmatrix <- function (object, ...) matrixSh (object)

setClass ("mmatrix",
					representation (
						data = "Matrix", 
						metadata = "rlist", 
						hierarchy = "character"),
					contains = NULL)
setIs ("mmatrix", "Matrix",
			 coerce = function (from) from@data,
			 replace = function (object, value) { from @ data <- value ; from } )
setIs ("mmatrix", "matrix",
			 coerce = function (from) as.matrix (from@data),
			 replace = function (object, value) { from @ data <- Matrix::Matrix (value) ; from } )
setMethod ("initialize", "mmatrix",
					 function (.Object, data = Matrix::Matrix(), metadata = new ("rlist"), hierarchy = character(0)) {
					 	.Object@data <- data; .Object@metadata <- metadata; .Object@hierarchy <- hierarchy; .Object } )
setMethod ("print", "mmatrix", matrixPr)
setMethod ("summary", "mmatrix", function (object, ...) matrixSh (object))
setMethod ("show", "mmatrix", matrixSh)

### the intended user-facing construction function:
setMethod ("mmatrix", "character",
					 function (x, view = standardViews$count, ...)
					 	new ("mmatrix", 
					 			 data = matrixView (scrubIds (x), view),
					 			 metadata = metadata (x), 
					 			 hierarchy = character (0)))
setMethod ("mmatrix", "matrix",
					 function (x, ...)
					 	new ("mmatrix",
					 			 data = Matrix::Matrix (x),
					 			 metadata = new ("rlist"),
					 			 hierarchy = character (0)))

#################################################
### SPECIFIC OF VIEW(S)
###
### this class is built with extensions in mind.
### for now it looks like just a list, but perhaps
### that some elements of a view specification eventually
### will not fit this paradigm.  that is why it is 
### a proper class.
###
### USAGE:
###   v <- view (annotation = "organism")
###   v <- list ()
###   for (s in mSources) v [[s]] <- view (source = s)
###
###	Tip: arguments do not need to be specified in full; e.g. view (ann = "func")
### Valid values:
###		... etc
#################################################
viewPr <- function (x, ... )
	cat(x@of, " : ", x@annotation, " : ", x@level, " : ", x@source, "\n", sep = "")
viewSh <- function (object) viewPr (object)
print.view <- viewPr
summary.view <- function (object, ...) viewSh (object)

setClass ("view",
					representation (
						of = "character",
						annotation = "character",
						level =	"character",					# "species", "phylum", etc. OR "Subsystem", "level1", etc.
						source = "character",					# "m5rna", "Greengenes", etc.
						other = "list"))						# just a precaution for extensibility
setMethod ("print", "view", viewPr)
setMethod ("summary", "view", function (object, ...) viewSh (object))
setMethod ("show", "view", viewSh)

# funcLevels and orgLevels are defined in pkgdata.R
# c,f,l,S <- view()		c,o,s,m <- view(ann="org")		c,o,p,m <- view(ann="org",lev="phy")
view <- function (of = "count",
									annotation = "function",
									level = if (!is.na (pmatch (annotation, "function"))) "level3" else "species",
									source = if (!is.na (pmatch (annotation, "function"))) "Subsystems" else "m5rna") {
	of = ofs[pmatch(of,ofs)]
	annotation = annotations[pmatch(annotation, annotations)]    
	new("view", of = of,
			annotation = annotation, 
			level = if (annotation == "function") funcLevels[pmatch(level, funcLevels)] 
			else orgLevels[pmatch(level, orgLevels)], 
			source = sources[[pmatch(tolower(source), tolower(sources))]])
}

# retrieves a matrix with given ids in a given view
# I think -- reconsider? -- it is sound for this function to take an id list, not a selection object
# value is "Matrix" (not "mmatrix")
matrixView <- function (ids, v) {
	s <- paste (
		"format/plain",
		"/result_column/", switch (v@of, count = "abundance", normed = "abundance", evalue = "evalue", length = "length", percentid = "identity"),
		"/type/", v@annotation,
		"/group_level/", v@level,
		"/source/", v@source, sep = "")
	# irritated this does not work:
	# 	(if (v@of == "normed") function (x) Matrix::Matrix (normalize (x)) else identity) (mGet ("abundance", scrubIds (ids), param = s, enClass = FALSE))
	x <- mGet ("abundance", scrubIds (ids), param = s, enClass = FALSE)
	if (v@of == "normed") Matrix::Matrix (normalize (x))
	else x
}

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
colPr <- function (x, ...) {
	print (x@sel)
	cat ("\n")
	for (j in 1:length(x@views)) { cat ("$", names (x@views) [j], " :: ", sep = "") ; print (x@views [[j]]) }
}
colSh <- function (object) colPr (object)
print.collection <- colPr
summary.collection <- function (object, ...) colSh (object)

setClass ("collection",
					representation (
						sel = "selection",						# metagenome selection used to construct this collection object
						data = "list",							# list of named "Matrix" objects (or possibly "mmatrix" for short/long names?)
						views = "list"),							# list of corresponding named "view" objects
					contains = NULL)
setMethod ("[[", "collection", function (x, i, exact = TRUE) x@data [[i]])
setMethod ("$", "collection", function (x, name) x [[name]])
setMethod ("print", "collection", colPr)
setMethod ("summary", "collection", function (object, ...) colSh (object))
setMethod ("show", "collection", colSh)
setMethod ("selection", "collection", function (x) x@sel)
setMethod ("samples", "collection", function (x) selection (x))
setMethod ("metadata", "collection", function (x) x@sel@metadata)
setMethod ("views", "collection", function (x) x@views)
setMethod ("viewnames", "collection", function (x) names (x@views))
setMethod ("viewnames<-", "collection", 
					 function (x, value) {
					 	names(x@views) <- value
					 	names(x@data) <- value
					 	x
					 } )
setMethod ("collection", "character",
					 function (x, ...)
					 	collection (selection (x), ...))
setMethod ("collection", "selection",
					 function (x, ...) {
					 	views <- unlist (list (...))
					 	if (length (views) == 0) {
					 		message ("matR: no matrix views specified; using defaults")
					 		views <- standardViews
					 	}
					 	data <- list ()
					 	for (j in 1:length (views))
					 		data [[j]] <- new ("mmatrix", data = matrixView (x@ids, views [[j]]))
					 	names (data) <- names (views)
					 	new ("collection", sel = x, data = data, views = views)
					 } )
setMethod ("$<-", signature (x = "collection", value = "view"),
					 function (x, name, value) {
					 	x @ data [[name]] <- new ("mmatrix", data = matrixView (x@sel@ids, value))
					 	x @ views [[name]] <- value
					 	x
					 } )
setMethod ("[[<-", signature (x = "collection", i = "character", j = "missing", value = "view"), 
					 function (x, i, value) {
					 	x @ data [[i]] <- new ("mmatrix", data = matrixView (x@sel@ids, value))
					 	x @ views [[i]] <- value
					 	x
					 } )
setMethod ("names", "collection", function (x) names (x@sel))
setMethod ("names<-", "collection", 
					 function (x, value) {
					 	names (x@sel) <- value
					 	x } )
setMethod ("groups", "collection", function (x) groups (x@sel))
setMethod ("groups<-", "collection", 
					 function (x, value) {
					 	groups (x@sel) <- value
					 	x } )


# this is a constant intended to be available to the user.
# cannot go in pkgdata.R because its definition requires functionality from the package.
standardViews <- list (
	count = view (of = "count"),
	normed = view (of = "normed"))

allViews <- list (
	count = view (of = "count"),
	normed = view (of = "normed"),
	evalue = view (of = "evalue"),
	length = view (of = "length"),
	percentid = view (of = "percentid"))
