
# these data need to be available to matR and to the user.
# cannot go in pkgdata.R for unclear reasons.
sources <- list (
	m5rna = "m5rna", rdp = "RDP", greengenes = "Greengenes", lsu = "LSU", ssu = "SSU",				# rna
	nog = "NOG", cog = "COG", ko = "KO", go = "GO", subsystems = "Subsystems",						# ontology
	m5nr = "m5nr", swissprot = "SwissProt", genbank = "GenBank", img = "IMG", seed = "SEED",		# protein
	TrEMBL = "TrEMBL", refseq = "RefSeq", patric = "PATRIC", eggnog = "eggNOG", kegg = "KEGG")
orgLevels <- c ("domain", "phylum", "class", "order", "family", "genus", "species", "strain")
funcLevels <- c ("level1", "level2", "level3", "function")
matrixOf <- c ("count", "normed", "evalue", "length", "percentid")


#################################################
### Class components:
### definition, inheritance, construction, methods, print 
###
### We declare some generics, with methods defined below.
### Additionally we implement methods for show() which
### is already generic
###
### ALSO: set S3 methods (e.g., print.rlist)
#################################################

setGeneric ("print")
setGeneric ("summary")
setGeneric ("plot")
setGeneric ("metadata", function (x, ...) { standardGeneric ("metadata") }, useAsDefault = FALSE)
setGeneric ("collection", function (sel, ...) { standardGeneric ("collection") }, useAsDefault = FALSE)

rlistPr <- function (x, ...) listPrinter (x)
rlistSh <- function (object) rlistPr (object)

matrixPr <- function (x, ...) matrixPrinter (x)
matrixSh <- function (object) matrixPr (object)


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
	function (x, name) { y <- unclass (x) [[name]] ; if (is (y, "list")) new ("rlist", y) else y })
setMethod ("[[", "rlist",
	function (x, i, j, ..., exact = TRUE) { y <- unclass (x) [[i]] ; if (is (y, "list")) new ("rlist", y) else y })
setMethod ("print", "rlist", rlistPr)
setMethod ("summary", "rlist", function (object, ...) rlistSh (object))
setMethod ("show", "rlist", rlistSh)
print.rlist <- rlistPr
summary.rlist <- function (object, ...) rlistSh (object)

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
### MATRIX
### (1) based for now on the "Matrix" class; maybe to be reimplemented with an underlying BIOM object
### (2) the advantages of defining this class (instead of just using a "Matrix") are not yet clear.
###		But I am close to certain it will prove a good idea.  the (unused) hierarchy slot, for
###		instance will find a purpose.
###
### DESIDERATA:
###		retrieve the entire hierarchy into the hierarchy field, with short names in rownames
#################################################
setClass ("mmatrix",
	representation (data = "Matrix", metadata = "rlist", hierarchy = "character"),
	contains = NULL)
setIs ("mmatrix", "Matrix",
	coerce = function (from) { from @ data },
	replace = function (object, value) { from @ data <- value ; from } )
setIs ("mmatrix", "matrix",
	coerce = function (from) { as.matrix (from @ data) },
	replace = function (object, value) { from @ data <- Matrix::Matrix (value) ; from } )
setMethod ("initialize", "mmatrix",
	function (.Object, data = Matrix::Matrix(), metadata = new ("rlist"), hierarchy = character(0)) {
		.Object@data <- data; .Object@metadata <- metadata; .Object@hierarchy <- hierarchy; .Object } )
setMethod ("print", "mmatrix", matrixPr)
setMethod ("summary", "mmatrix", function (object, ...) matrixSh (object))
setMethod ("show", "mmatrix", matrixSh)
print.mmatrix <- matrixPr
summary.mmatrix <- function (object, ...) matrixSh (object)

### the intended user-facing construction function:
mmatrix <- function (ids, view = standardViews$count)
	new ("mmatrix", 
		data = matrixView (scrubIds (ids), view),
		metadata = metadata (ids), 
		hierarchy = character (0))

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
#################################################
setClass ("selection", 
	representation (
		ids = "character",						# the ids specified for construction: any of mgm, mgs, mgp, (mgl?)
		resources = "character",				# resource type, corresponding to each id
		sel =									# representation of the selection content.  In the initial implementation, this
		  "character",							#   just means all metagenome ids.  Later, some kind of relational tree
		tagging =								# complete metadata is always retrieved, but different approaches to metadata
		  "character",							#   redundancy are possible: "none", "asis", "min", "full".  to be implemented
		metadata = "rlist"))
setMethod ("initialize", "selection",
	function (.Object, ids, resources = "metagenome", tagging = "asis") { 
		.Object @ ids <- ids
		.Object @ resources <- resources						# need some parsing here, to actually use "resources"
		.Object @ sel <- .Object @ ids							# this needs to apply the value(s) of "resources"
		.Object @ tagging <- tagging
		.Object @ metadata <-									# this needs to implement all values of "tagging"
			if (tagging != "none") metadata (.Object @ sel)
			else new ("rlist")
		.Object } )
setMethod ("metadata", "selection", function (x) { x@metadata } )
selPr <- function (x, ... ) cat ("<metagenome selection:  ", paste (x @ sel, collapse = ", "), ">\n", sep = "")
selSh <- function (object) selPr (object)
setMethod ("print", "selection", selPr)
setMethod ("summary", "selection", function (object, ...) selSh (object))
setMethod ("show", "selection", selSh)
print.selection <- selPr
summary.selection <- function (object, ...) selSh (object)

### the intended user-facing construction function:
# this is not right; need to scrub / scrape
selection <- function (ids, resources = "metagenome", tagging = "asis")
	new ("selection", chomp (ids), chomp (resources), tagging)


#################################################
### SPECIFIC OF VIEW(S)
###
### this class is built with extensions in mind
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
setClass ("view",
	representation (
		of = "character",
		annotation = "character",
		level =	"character",					# "species", "phylum", etc. OR "Subsystem", "level1", etc.
		source = "character",					# "m5rna", "Greengenes", etc.
		other = "list"))						# just a precaution for extensibility
setMethod ("initialize", "view", function (.Object) .Object)
viewPr <- function (x, ... ) cat ("<matrix view:  ", x@of, ", ", x@annotation, ", ", x@level, ", ", x@source, ">\n", sep = "")
viewSh <- function (object) viewPr (object)
setMethod ("print", "view", viewPr)
setMethod ("summary", "view", function (object, ...) viewSh (object))
setMethod ("show", "view", viewSh)
print.view <- viewPr
summary.view <- function (object, ...) viewSh (object)

# here, the class initialization function here does nothing,
# while the user-facing construction function assigns defaults.
# this is done differently than other classes, for no clear reason
#
# funcLevels and orgLevels are defined in pkgdata.R
# c,f,l,S <- view()		c,o,s,m <- view(ann="org")		c,o,p,m <- view(ann="org",lev="phy")
view <- function (of = "count",
				annotation = "function",
				level = if (!is.na (pmatch (annotation, "function"))) "level3" else "species",
				source = if (!is.na (pmatch (annotation, "function"))) "Subsystems" else "m5rna")  {
	v <- new ("view")
	ofs <- c ("count", "normed", "evalue", "length", "percentid")
	v@of <- ofs [pmatch (of, ofs)]
	annotations <- c ("function", "organism")
	v@annotation <- annotations [pmatch (annotation, annotations)]
	v@level <- if (v@annotation == "function") funcLevels [pmatch (level, funcLevels)]
		else orgLevels [pmatch (level, orgLevels)]
	v@source <- sources [[pmatch (tolower (source), tolower (sources))]]
	v
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
### this class is called "collection" in order to recall the concept familiar from the existing UI.
###
### USAGES:
###		sel <- selection ("mgm111111.3")
###		M <- collection (sel, g = view ("Greengenes"), r = view (source = "RDP"), full = view (source = "m5rna"))
###		M$g ; M$r ; M[[3]]						(these are "Matrix")
###		M$view(name = "new_view", annotation = "organism", level = "phylum")
###		metadata(M)
###		views(M)
#################################################
setClass ("collection",
	representation (
		sel = "selection",						# metagenome selection used to construct this collection object
		data = "list",							# list of named "Matrix" objects (or possibly "mmatrix" for short/long names?)
		views = "list",							# list of corresponding named "view" objects
		view = "function"))						# M$view(name = , ...) saves & returns a Matrix in the given view
setMethod ("initialize", "collection",
	function (.Object, sel, data, views, ...) {
		.Object @ sel <- sel ; .Object @ data <- data ; .Object @ views <- views
#		.Object @ view <- function (...) { v = list (...) ; }
		.Object } )
setMethod ("[[", "collection",
	function (x, i, j, ..., exact = TRUE)
		x@data[[i]])
setMethod ("$", "collection",
	function (x, name)
		x@data[[name]])
colPr <- function (x, ... ) {
	cat ("<collection of ", length (x@sel@sel), " metagenome(s), with ", length (x@views), " matrix view(s)>\n", sep = "")
	print (x@sel) ; cat ("\n")
	for (e in x@views) print (e)
	}
colSh <- function (object) colPr (object)
setMethod ("print", "collection", colPr)
setMethod ("summary", "collection", function (object, ...) colSh (object))
setMethod ("show", "collection", colSh)
print.collection <- colPr
summary.collection <- function (object, ...) colSh (object)

### user-facing construction functions and manipulations:
###		M <- collection (<selection>, views)
###		M <- collection (<character vector of ids>, views)
###		M <- collection (<file>, views)
###		md <- metadata (M)
views <- function (x) x@views
setMethod ("metadata", "collection", function (x) x@sel@metadata)
setMethod ("collection", "selection",
	function (sel, ...) {
		views <- unlist (list (...))
		if (length (views) == 0) {
			message ("matR: no matrix view(s) specified; using defaults")
			views <- standardViews
			}
		data <- list ()
		for (j in 1:length (views)) data [[j]] <- new ("mmatrix", matrixView (sel@sel, views [[j]]))
		names (data) <- names (views)
		new ("collection", sel, data, views)
		} )
setMethod ("collection", "character",
	function (sel, ...)
		collection (selection (sel), ...))
#setMethod ("collection", "connection",
#	function (sel, ...)
#		stop ("matR: unimplemented function"))			# this is EASY to implement and a BIG advantage.  or maybe .. need to setOldClass I think


# this is a constant intended to be available to the user.
# cannot go in pkgdata.R because its definition requires functionality from the package.
standardViews <- list (
	count = view (of = "count"),
	normed = view (of = "normed"),
	evalue = view (of = "evalue"),
	length = view (of = "length"),
	percentid = view (of = "percentid"))
