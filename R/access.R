
### ... NOTE: need to also set S3 methods (e.g., print.rlist)

#################################################
### we declare these generics, with methods defined below.
### additionally we implement methods for "show()", which
### is already generic
#################################################

setGeneric ("print")
setGeneric ("summary")
setGeneric ("plot")
setGeneric ("metadata", function (x, ...) { standardGeneric ("metadata") }, useAsDefault = FALSE)
setGeneric ("collection", function (sel, ...) { standardGeneric ("collection") }, useAsDefault = FALSE)

#################################################
### We present class components in this order, below:
###		definition
###		inheritance
###		construction
###		methods
###		print methods
#################################################

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
### the metadata class is a uniform recursive list 
### structure with atomic elements being strictly 
### character-mode and length-one
###
### This makes element access simple, e.g.:
###   m$metadata$env_package$habitat
###
### list elements within an meta object are also
### meta, and a list of meta objects naturally
### is itself an meta object.
###
### USAGES:
###
### new ("rlist", <existing list-like object>)
### new ("rlist", mGet (<resource>, ID, enClass = FALSE))
### md <- metadata ("4441872.3", resource = "metagenome")
### md $ metadata $ env_package $ data $ biome
### IDs <- c ("4441872.3", "4442034.3", "4448888.3", "4449999.3")
### metadataList <- metadata (IDs, resource = "metagenome")
### metadataList $ 4441872.3 $ metadata $ env_package $ data $ biome
###
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
# "Use <tab> liberally when exploring metadata.  Partial matching makes it easy to find the fields you want."
# "md <- metadata ("4443360.3")"
# "md$metadata$env_package$data$chlo<tab> completes the word "chlorophyll""
# "Use an index vector to quickly retrieve important metadata fields."
# i <- c ("metadata","env_package", "data", "chlorophyll")
# md [[i]]

# DESIDERATA:
# ...shortcut access to deeply nested elements
setMethod ("$", "rlist",
	function (x, name) { y <- unclass (x) [[name]] ; if (is (y, "list")) new ("rlist", y) else y })
setMethod ("[[", "rlist",
	function (x, i, j, ..., exact = TRUE) { y <- unclass (x) [[i]] ; if (is (y, "list")) new ("rlist", y) else y })
setMethod ("print", "rlist", rlistPr)
setMethod ("summary", "rlist", rlistSh)
setMethod ("show", "rlist", rlistSh)
print.rlist <- rlistPr
summary.rlist <- rlistSh

### a metadata object is an object of class "rlist", that is all.
### we provide this construction function:

setMethod ("metadata", "character",
	function (x, resource = "metagenome") {
# DESIDERATA
# ... auto-detect type according to prefix: mgp, mgl, mgs, mgm
# ... recycle "resource" to allow simultaneous retrieval of different types of metadata
# ... method for type "connection", to initialize from file
	x <- chomp (x)
	resources <- chomp (resource)
	if (length (x) > 1) {
		L <- list ()
# the right answer here is to vectorize mGet for parameter "resource"
		for (j in 1:length (x)) L [[x [j]]] <- mGet (resource, x [j], enClass = FALSE)
		names (L) <- x
		new ("rlist", L)
		}
	else new ("rlist", mGet (resource, x, enClass = FALSE))
	} )

#################################################
### MATRIX
### (1) based on old schema but perhaps still useful---
### (2) note, this is currently defined twice, also in oldclasses.R---
### (3) based for now on the Matrix class; maybe to be reimplemented with underlying BIOM object
### (4) it is unclear for now whether using an object such as this, for the "collection" class, has any real
###		advantage to using a "Matrix" object.  I suspect however that good extensions here, yet to be imagined,
###		will prove the answer yes.
###
#################################################
setClass ("mmatrix",
	representation (data = "Matrix", metadata = "rlist", hierarchy = "character"),
#	prototype = prototype (data = Matrix::Matrix (), metadata = new ("rlist")),
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
setMethod ("summary", "mmatrix", matrixSh)
setMethod ("show", "mmatrix", matrixSh)
print.mmatrix <- matrixPr
summary.mmatrix <- matrixSh

### the intended user-facing construction function:
### DESIDERATA:
###		retrieve the entire hierarchy into the hierarchy field, with short names in rownames
mmatrix <- function (IDs) 
	new ("mmatrix", 
		data = Matrix::Matrix (mGet ("abundance", IDs, enClass = FALSE)),
		metadata = metadata (IDs), 
		hierarchy = "")

#################################################
### SELECTION FOR ANALYSIS
### the point of this class is to enable flexible
### specification of a group of metagenomes to study.
### for instance, it accepts specification of two
### _project_ IDs, identifies all related metagenome IDs,
### and remembers that the metagenomes belong to 
### two distinct groups.
###
### resource = c ("project", "sample", "metagenome")
### metadata = c ("none", "asis", "min", "all")
###
### USAGES:
###
### sel <- selection ("mgm111111.3", meta = "all")
### x <- selection (IDs, meta = "all")
### metadata (x) $ env_package $ ...
#################################################
setClass ("selection", 
	representation (
		IDs = "character",						# the IDs specified for construction: any of mgm, mgs, mgp, (mgl?)
		resources = "character",				# resource type, corresponding to each ID
		sel =									# representation of the selection content.  In the initial implementation, this
		  "character",							#   just means all metagenome IDs.  Later, some kind of relational tree
		tagging =								# complete metadata is always retrieved, but different approaches to metadata
		  "character",							#   redundancy are possible: "none", "asis", "min", "full".  to be implemented
		metadata = "rlist"))
setMethod ("initialize", "selection",
### DESIDERATA:
###		handle specification by mgm, mgs, mgp (mgl?) arbitrarily, as intended in the design
	function (.Object, IDs, resources = "metagenome", tagging = "asis") { 
		.Object @ IDs <- IDs
		.Object @ resources <- resources						# need some parsing here, to actually use "resources"
		.Object @ sel <- .Object @ IDs							# this needs to apply the value(s) of "resources"
		.Object @ tagging <- tagging
		.Object @ metadata <-									# this needs to implement all values of "tagging"
			if (tagging != "none") metadata (.Object @ sel)
			else new ("rlist")
		.Object } )
setMethod ("metadata", "selection", function (x) { x@metadata } )
selPr <- function (x, ... ) cat ("<metagenome selection:  ", paste (x @ sel, collapse = ", "), ">\n", sep = "")
selSh <- function (object) selPr (object)
setMethod ("print", "selection", selPr)
setMethod ("summary", "selection", selSh)
setMethod ("show", "selection", selSh)
print.selection <- selPr
summary.selection <- selSh

### again, the intended user-facing construction function:

selection <- function (IDs, resources = "metagenome", tagging = "asis")
	new ("selection", chomp (IDs), chomp (resources), tagging)


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
###		"tip: argument names do not need to be specified in full; e.g. view(anno="organism")"
#################################################
setClass ("view",
	representation (
		of = "character",						# "count", "normed", "evalue", "length", "percentid"
		annotation = "character",				# "function" or "organism"
		level =	"character",					# "species", "phylum", etc. OR "Subsystem", "level1", etc.
		source = "character",					# "m5rna", "Greengenes", etc.
		other = "list"))						# just a precaution for extensibility
setMethod ("initialize", "view", function (.Object) .Object)
viewPr <- function (x, ... ) cat ("<matrix view:  ", x@of, " | ", x@annotation, " | ", x@level, " | ", x@source, ">\n", sep = "")
viewSh <- function (object) viewPr (object)
setMethod ("print", "view", viewPr)
setMethod ("summary", "view", viewSh)
setMethod ("show", "view", viewSh)
print.view <- viewPr
summary.view <- viewSh

# here, the class initialization function here does nothing,
# while the user-facing construction function assigns defaults.
# this is done differently than other classes, for no clear reason
# CHECK DEFAULTS
view <- function (of = "count",
				annotation = "function",
				level = if (annotation == "function") "Subsystem" else "species",
				source = if (annotation == "function") "m5rna" else "m5nr")  {
	v <- new ("view")
	v@of <- of; v@annotation <- annotation; v@level <- level; v@source <- source; v }

# retrieves a matrix with given IDs in a given view
# I think -- reconsider? -- it is appropriate for this function to take an ID list, not a selection object
# value is "Matrix" (not "mmatrix")
getView <- function (IDs, v) {
	s <- paste ("format/plain/type/", v@annotation, "/group_level/", v@level, "/source/", v@source, sep = "")
	if (oneof (v@of, "evalue", "length", "percentid"))
		s <- paste (s, "/result_column/", switch (v@of, evalue = "evalue", length = "length", percentid = "identity"), sep = "")
	x <- mGet ("abundance", IDs, param = s, enClass = FALSE)
	if (v @ of == "normed")
		x <- normalize (x)
# change again here to a construction method
	new ("mmatrix", x)
	}

#################################################
### MATRIX INTERACTION 
###
### (...builds on the UI collection concept, blah blah...)
### USAGES:
###
###	sel <- selection ("mgm111111.3")
###	M <- collection (sel, g = view ("Greengenes"), r = view (source = "RDP"), full = view (source = "m5rna"))
### M$g ; M$r ; M[[3]]						(these are "Matrix")
### M$view(name = "new_view", annotation = "organism", level = "phylum")
### metadata(M)
### views(M)
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
	function (x, i, j, ..., exact = TRUE)	{ x @ data [[i]] } )
setMethod ("$", "collection",
	function (x, name)	{ x @ data [[name]] } )
colPr <- function (x, ... ) {
	cat ("<collection of ", length (x @ sel @ sel), " metagenome(s), with ", length (x @ views), " matrix view(s)>\n", sep = "")
	print (x @ sel) ; cat ("\n")
	print (x @ views)
	}
colSh <- function (object) colPr (object)
setMethod ("print", "collection", colPr)
setMethod ("summary", "collection", colSh)
setMethod ("show", "collection", colSh)
print.collection <- colPr
summary.collection <- colSh


### user-facing construction functions and manipulations:
###   md <- metadata (M)
###   M <- collection (sel, views)			# <views> optional
###   M <- collection (IDs, views)			# <IDs> : character string(s)
views <- function (x) { x @ views }
setMethod ("metadata", "collection", function (x) { x @ sel @ metadata } )
setMethod ("collection", "selection",
	function (sel, ...) {
		views <- unlist (list (...))
		if (length (views) == 0) {
			message ("matR: no view(s) specified; defaulting to standard")
			views <- standardViews
			}
		data <- list ()
		for (j in 1:length (views)) data [[j]] <- getView (sel @ sel, views [[j]])
		names (data) <- names (views)
		new ("collection", sel, data, views)
		} )
setMethod ("collection", "character",
	function (sel, ...)
		collection (selection (sel), ...))

#################################################
### VIEWS FOR STANDARD INTERACTION
###
### USAGE:
###   matrix (sel, standardViews)
#################################################
standardViews = list (
	raw = view (of = "count", anno="organism", level="species",source="m5nr"),
#	norm = view (of = "normed", anno="organism", level="species",source="m5nr"),
	e = view (of = "evalue", anno="organism", level="species",source="m5nr"),
	len = view (of = "length", anno="organism", level="species",source="m5nr"),
	id = view (of = "percentid", anno="organism", level="species",source="m5nr"))



############################################################
### initialize class objects from various sources...
############################################################

#setMethod ("initialize",
#	"meta", function (.Object, conn) { } )
#setMethod ("initialize",
#	"mmatrix", function (.Object, Matr) { } )
#setMethod ("initialize",
#	"mmatrix", function (.Object, matr) { } )
#setMethod ("initialize",
#	"mmatrix", function (.Object, conn) { } )

############################################################


