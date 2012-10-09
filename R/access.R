
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
###
### We declare generics with methods defined below.
### Additionally we implement methods for "show"
### which is already generic.  We also set S3
### methods (e.g., print.rlist)
#################################################

setGeneric ("print")
setGeneric ("summary")
setGeneric ("plot")
setGeneric ("names")
setGeneric ("names<-")
setGeneric ("groups", function (x, ...) standardGeneric ("groups"))
setGeneric ("groups<-", function (x, value) standardGeneric ("groups<-"))
setGeneric ("mmatrix", function (x, ...) standardGeneric ("mmatrix"))
setGeneric ("metadata", function (x, ...) standardGeneric ("metadata"))
setGeneric ("selection", function (x, ...) standardGeneric ("selection"))
setGeneric ("collection", function (x, ...) standardGeneric ("collection"))

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
rlistPr <- function (x, ...) listPrinter (x)
rlistSh <- function (object) rlistPr (object)
print.rlist <- rlistPr
summary.rlist <- function (object, ...) rlistSh (object)

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
           function (x, name) {
             y <- unclass (x) [[name]]
             if (is (y, "list")) new ("rlist", y) else y
           } )
setMethod ("[[", "rlist",
           function (x, i, j, ..., exact = TRUE) { 
             y <- unclass (x) [[i]]
             if (is (y, "list")) new ("rlist", y) else y
           } )
setMethod ("[", signature (x = "rlist", i = "list"), 
           function (x, i) 
             lapply (X = i, FUN = function (i, x) x [[i]], x))
setMethod ("[", signature (x = "rlist", j = "character"),
           function (x, i, j)
             lapply(lapply (X = x, FUN = function (y, j) y [[j]], j),
                    function (x) if (is.null (x)) NA else x))
setMethod ("print", "rlist", rlistPr)
setMethod ("summary", "rlist", function (object, ...) rlistSh (object))
setMethod ("show", "rlist", rlistSh)

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
# the ids specified for construction: any of mgm, mgs, mgp, (mgl?)
# resource type, corresponding to each id
# representation of the selection content.  In the initial implementation, this
# just means all metagenome ids.  Later, some kind of relational tree
# complete metadata is always retrieved, but different approaches to metadata
# redundancy are possible: "none", "asis", "min", "full".  to be implemented
#################################################
selPr <- function (x, ... ) {
  if (all (x@ids == names(x))) {
    s <- x@ids
    names (s) <- NULL
    print (s)
  }
  else print (x@ids)
}
selSh <- function (object) selPr (object)
print.selection <- selPr
summary.selection <- function (object, ...) selSh (object)

setClass ("selection", 
          representation (
            ids = "character",
            groups = "factor",
            metadata = "rlist",
            ids.spec = "character",
            resources.spec = "character",
            metadata.extent = "character"))
setMethod ("names", "selection", function (x) names (x@ids))
setMethod ("groups", "selection", function (x) x@groups)
setMethod ("metadata", "selection", function (x) x@metadata )
setMethod ("print", "selection", selPr)
setMethod ("summary", "selection", function (object, ...) selSh (object))
setMethod ("show", "selection", selSh)
setMethod ("names<-", "selection",
           function (x, value) {
             names (x@ids) <- value
             if (length (x@groups) > 0) names (x@groups) <- value
             x
           })
setMethod ("groups<-", "selection", 
           function (x, value) {
             x@groups <- as.factor (value)
             names (x@groups) <- names (x@ids)
             x
           })
### specification of selections by project & sample id is not yet implemented
setMethod ("selection", "character",
           function (x, resources = "metagenome", metadata.extent = "asis") {
             resources <- chomp (resources)
             ids <- scrubIds (x, resources)
             if (is.null (names (ids))) names (ids) <- ids
             metadata <- switch (metadata.extent,
                                 all = metadata (ids),
                                 asis = metadata (ids),
                                 smart = metadata (ids),
                                 min = metadata (ids),
                                 none = new ("rlist"))
             new ("selection",
                  ids = ids,
                  groups = as.factor (rep (1, length (ids))),
                  metadata = metadata,
                  ids.spec = ids,
                  resources.spec = resources,
                  metadata.extent = metadata.extent)
             })
setMethod ("selection", "numeric", getMethod ("selection", "character"))


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
setMethod ("metadata", "collection", function (x) x@sel@metadata)
views <- function (x) x@views
viewnames <- function (x) names (x@views)
`viewnames<-` <- function (x, value) {
  names(x@views) <- value
  names(x@data) <- value
  x
}
# setMethod ("collection", "connection",
#            function (x, ...)
#              collection (selection (readLines (x, n = -1, ok = TRUE, warn = FALSE))))
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
