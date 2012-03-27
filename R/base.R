

############################################
### first, a few little things
############################################

# this is good for inspecting objects
shew <- function (a) { 
	cat ("typeof =", typeof (a), "  mode =", mode (a), "  class =", class (a), "\nsummary:\n")
	print ("dimensions = ", dim (a))shh
	summary (a) 
	}

# abbreviations are nice
shh <- suppressPackageStartupMessages

# this is good for building demos
hold <- function (messg = NULL, cmd = NULL) { 
	message (messg); 
	if (!is.null (cmd)) writeLines (paste (options ("prompt"), cmd), sep="")
	invisible (readLines (n=1, warn=FALSE))
	}

# open documentation page in a browser
seeDoc <- function (f) {
	outfile <- tempfile (fileext = ".html")
	browseURL (tools::Rd2HTML (f, outfile))
	}

# I forgot how exactly this is used
buildHTMLDocs <- function () {
	for (d in docs) tools::Rd2HTML (d, paste ("./html/", unlist (strsplit (d, ".", fixed = TRUE)) [1], ".html", sep=""), Links = tools::findHTMLlinks ())
	}

# split a character vector at all semicolons
chomp <- function (s) {
	if (is.null (s)) return (NULL)
	unlist (strsplit (paste (as.character (s), collapse = ";", sep = ""), ";", fixed = TRUE))
	}

# concatenates to a single string, separating by semicolons
# that is how the API wants to see multiple values of the same parameter
glom <- function (s) { 
	if (is.null (s)) return (NULL)
	paste (as.character (s), collapse = ";", sep = "")
	}

# tests first argument for equality with any of others
oneof <- function (x, ...) any (x == unlist ( list (...)))
# same but always warns if not among
oneofmust <- function (x, ...) {
	if ( any (x == unlist ( list (...)))) return (TRUE)
	warning (x, " should be among ", paste (unlist ( list (...)), collapse = " "))
	FALSE
	}
# same but warns if among
oneofmusnt <- function (x, ...) {
	if ( !any (x == unlist ( list (...)))) return (TRUE)
	warning (x, " should not be among ", paste (unlist ( list (...)), collapse = " "))
	FALSE
	}

# this is useful several times
semiwarn <- function (s) {
	if (length( grep (";", s, fixed = TRUE))) {
		message ("KbaseKit: semicolon interpreted as separator")
		TRUE
	} else FALSE
	}

############################################
### unexported globals that hold "state"
############################################

#.auth <- ""
#.server <- ""
# using global options kbase.auth and kbase.server for now

############################################
### check for servers and auth key upon
### package loading
###
### the mechanism to preserve an auth
### key is to assign it to a global symbol.
### the user then may ensure it is saved
### for the next session
###
### .auth is used and hidden, whereas 
### kbase.auth.key is exported and not used
############################################
.onAttach <- function (libname, pkgname) {
#options()["kbase.server"] <<- .kbServers()$prod
.kbase.server <<- .kbServers()$prod
success <- TRUE
# packageStartupMessage ("KbaseKit: checking servers") 
# ...
if (success) packageStartupMessage ("KbaseKit: default server set: ", .kbGetServer())

if (length (grep (".kbase.auth.key", ls (1, all = TRUE), fixed = TRUE))) {
	packageStartupMessage ("KbaseKit: using existing auth key" )
#	options (kbase.auth = .kbase.auth.key)
	.kbase.auth <<- .kbase.auth.key
}
else {
	packageStartupMessage ( "KbaseKit: auth key not set" )
#	options (kbase.auth = "")
	.kbase.auth <<- ""
}
#set print.default
#set download.file.method
options (warn = 1)
packageStartupMessage( "KbaseKit: this package sets global option(s): warn = 1")

packageStartupMessage (paste (
"\nThis code is under active development.",
"Feedback, feature suggestions, and bug reports are welcome.",
#"Test codes are in the installation directory.",
#"Usage examples are included with the documentation,",
"A good place to start is: package?KbaseKit", 
sep="\n"))
}
### hooks unused for now:
### .onLoad <- function (libname, pkgname) { }
### .onUnload <- function (libname) { }
.Last.lib <- function (libpath) { 
#options (kbase.server = NULL)
#options (kbase.auth = NULL)
rm (.kbase.auth, .kbase.server)
}

### the server can be viewed and set manually
.kbGetServer <- function () { 
# getOption (kbase.server) }
.kbase.server }
.kbSetServer <- function ( s ) { 
#options (kbase.server = s) }
.kbase.server <<- s }

### simple interface to get and set the auth key.  it is set automatically upon loading, if it exists
.kbGetAuth <- function() { 
#getOption (kbase.auth) }
.kbase.auth }
.kbSetAuth <- function (key = NULL, save = FALSE) {
if (is.null (key) ) {
	message( "KbaseKit: Enter authorization key on a single line: " )
	key <- readLines (n=1, ok=TRUE, warn=FALSE)
}
if (save) {
	warning ("KbaseKit: authorization key set in global environment may be saved to .RData file")
	.kbase.auth.key <<- key
}
# options (kbase.auth = key)
.kbase.auth <<- key
}


############################################
### we export a routine for "raw" calls to
### the API as alternative when the high-level 
### routines break.  idea for this is to be
### unsophisticated but robust.  perhaps
### eventually to be implemented more carefully 
### with RCurl.  this is also the foundation
### for higher level calls
############################################
#
# implementation may need changing.
# the desired behavior is:
# if (server wants to send a file(s))
#	if (toFile == NULL)
#		if (server indicates a name(s))
#			save with that name(s)
#		else
#			save with name(s) generated by temp.file
#	else
#		save named as toFile
# else
#	if (toFile == NULL)
#		return text object
#	else
#		write to file named as toFile
#
# and,
# always return file name(s) if result is a file
#
.kbCallRaw <- function (call, toFile = NULL) {
	if (!length (grep ("?", call, fixed = TRUE))) conj = "?"
	else conj = "&"
	urlStr <- paste (.kbGetServer(), call, conj, "auth=", .kbGetAuth(), sep = "")
	message ("KbaseKit: .kbCallRaw: ", urlStr)
	if (!is.null (toFile)) {
		download.file (urlStr, toFile, quiet = TRUE)
		toFile
		}
	else readLines (urlStr, warn = FALSE)
	}

############################################
### these are routines intended for general use
############################################

### get a list of all resources of one type.  
### a convenience function for no-parameter calls
kbListAll <- function (resource) {
	if ( !warnParams (resource, c ("project", "sample", "library", "annotation", "metagenome"))) return ()
	x <- fromJSON (.kbCallRaw (resource), simplify = TRUE, asText = TRUE)
	invisible (switch (resource, 
		project = as.vector (x$projects),
		sample = as.vector (x$samples),
		library = as.vector (x$librarys),
		annotation = as.vector (x),
		metagenome = as.vector (x$metagenomes)))			### return R-friendly object	
	}

# perform an md5 lookup in the specified annotation database
# ... it seems this is not yet implemented in the API
# ... so, this is untested ...
kbAnnotationLookup <- function (md5, namespace) {
	md5 <- chomp (md5)
	namespace <- chomp (namespace)
	if (length (namespace) > 1) {
		warning ("KbaseKit: only one namespace may be specified")
		return ()
		}
	x <- character( length (md5))
	for (j in 1:length (md5))
		x [ j ] <- fromJSON (.kbCallRaw ( paste ("annotation?md5=", md5 [ j ], sep = "")))
	x
	}

### search metagenomes in mgrast for specified criteria
### ... fix me ...
kbSearchMetagenomes <- function (resource, attribute = NULL, value = NULL) {
	warning( "KbaseKit: unimplemented function" )
### ... .kbCallRaw etc ...
	}

# specify a resource and get it back in an R-friendly form
# this routine handles API calls WHICH REQUIRE AN ID
# it is an interface, not a wrapper, meaning the defaults and
# interaction schema of the API are reconsidered
#
# returns object(s) if object(s) retrieved
# returns filename(s) if file(s) retrieved
# return filenames visibly and objects invisibly
#
# ... write to file (in first call, recursive call, or raw call?)
# ... also, when to notify of file writes?
# ... can an organism name or functional annotation contain a semicolon?? ...
# ... add prefixes such as "mgp" etc.
# ... consistent return value on failure
# ... checking of actual content of JSON object
kbGet <- function( 
	resource,				# single value
	ID,						# multiple value
	namespace = NULL,		# multiple
	annoType = NULL,		# single
	seqType = NULL,			# single
	org = NULL,				# multiple
	func = NULL,			# multiple
	md5 = NULL,				# multiple
	param = NULL,			# any string, will be directly to the call
	toFile = NULL ) {

# for "abundance" calls, there are naturally multiple 
# IDs in a single API call.  this function also allows
# multiple IDs for other resource calls, but handles them by 
# recursion, making one API call per ID.

# so first we check up on the ID parameter.
# it is natural to look at how output is requested
# (with toFile) at the same time

# we allow flexible specification of the ID parameter.
# a vector is natural to R scripting, while
# semicolon-separated is natural to interactive use
IDs <- chomp (ID)

# and we also accept semicolon-separated filenames
semiwarn (toFile)
toFile <- chomp (toFile)

if (length (IDs) > 1) {
	if ( oneof (resource, "project", "sample", "library", "metagenome")) {
		x = list ()
# for text-ish resources we allow either one filename
# per ID, or a single filename (meaning: append all to one)
		if (!is.null (toFile) && length (toFile) != 1 && length (toFile) != length (IDs)) {
			warning ("KbaseKit: call requires single filename or one filename per ID"); 
			return ()
			}
		deststr <- NULL
		for (j in 1:length (IDs)) {
			x [[j]] <- kbGet (resource, IDs [j], namespace, annoType, seqType, org, func, md5, param)
			if (length (toFile) > 1) {
				sink (file = toFile [j]) ; print (x [[j]]) ; sink()
				deststr <- paste (" to ", toFile [j])
				}
			message ("KbaseKit: fetched ", resource, " ", IDs [j], deststr)
			}
		if (length (toFile) == 1) {
			sink (file = toFile) ; print (x) ; sink()
			message ("KbaseKit: wrote to ", toFile)
			}
		if (!is.null (toFile)) return (toFile)
		return (invisible (x))
		}
	if (oneof (resource, "sequenceSet", "reads")) {
# for resources received as files,
# we require one filename per ID,
# or else no filenames (meaning: use defaults)
		if (!is.null (toFile) && length (toFile) != length (IDs)) {
			warning ("KbaseKit: call requires one filename per ID");
			return ()
			}
# NB. we allow that a single call might return more than one file
# ... that is why the length of outFiles is unknown
# ... and we have to use horrible style here
		outFiles <- character(0)
		for (j in 1:length (IDs)) {
			ff <- kbGet (resource, IDs [j], namespace, annoType, seqType, org, func, md5, param, toFile [j] )
			message ("KbaseKit: fetched ", resource, " ", IDs [j], " to ", ff)
			outFiles <- c (outFiles, ff)
			}
		return (outFiles)
		}
	if (resource != "abundance") {
		warning ("KbaseKit: multiple ID\'s not implemented for specified resource"); 
		return ()
		}
	}

# from here on we are in the case of single of no
# filename, and in the case of a single ID, except
# if resource is "abundance"

# code below is repeated unnecessarily when the function
# recurs, but we live with that inefficiency

# we require retrieval to disk of certain resources
if (oneof (resource, "sequenceSet", "reads") && is.null (toFile)) {
	message ("KbaseKit: using default filename for file resource")
	toFile <- paste (resource, ".", ID, sep = "")
}

# we now check up on parameters carefully, while
# constructing the API call

# make sure the resource type is valid
if ( !oneofmust (resource, "project", "sample", "library", "metagenome", "subset", "sequenceSet", "sequences",  "reads", "abundance")) return ()

# ... warn of functionality not yet implemented ...
if ( !oneofmusnt (resource, "sequenceSet", "reads")) return ()

# warn of parameters specified but not appropriate to the call
if ( !is.null( switch (resource,
		project = c (namespace, annoType, seqType, org, func, md5),
		sample = c (namespace, annoType, seqType, org, func, md5),
		library = c (namespace, annoType, seqType, org, func, md5),
		metagenome = c (namespace, annoType, seqType, org, func, md5),
		subset = c (seqType, md5),
		sequenceSet = c (namespace, annoType, seqType, org, func, md5),
		sequences = NULL,
		reads = c (namespace, annoType, seqType, org, func, md5),
		abundance = c (seqType, org, func, md5), NULL))) {
	warning ("KbaseKit: invalid parameter for specified resource")
	return ()
	}

# now make strings for other provided parameters
#	namespace --- multiple (subset, sequences), single (abundance)
#	annoType --- single (subset, sequences, abundance)
#	seqType --- single (sequences)
#	org --- multiple (subset, sequences)
#	func --- multiple (subset, sequences)
#	md5s --- multiple (sequences), single (for annotation; there called "md5")

namespace <- chomp (namespace)
# ... check here for valid values of namespace ...
if (resource == "abundance" && length (namespace) > 1) {
	warning ("KbaseKit: abundance must be taken against a single namespace")
	return ()
	}
nsStr <- if (is.null (namespace)) NULL
	else paste ("source", chomp (namespace), collapse = "&", sep = "=")

if ( length (annoType) > 1) {
	warning ("KbaseKit: only one annotation type may be specified")
	return ()
	}
annoTypeStr <- if (is.null (annoType)) NULL
	else if ( !oneofmust (annoType, "organism", "functional")) return()
	else paste ("type", annoType, sep = "=")

if ( length (seqType) > 1) {
	warning ("KbaseKit: only one sequence type may be specified")
	return ()
	}
seqTypeStr <- if (is.null (seqType)) NULL
	else if ( !oneofmust (seqType, "dna", "protein")) return ()
	else paste ("seq", seqType, sep = "=")

orgStr <- if (is.null (org)) NULL
	else paste ("organism", chomp (org), collapse = "&", sep = "=")
funcStr <- if (is.null (func)) NULL
	else paste ("function", chomp (func), collapse = "&", sep = "=")
md5Str <- if (is.null (md5)) NULL
	else paste ("md5s", chomp (md5), collapse = "&", sep = "=" )

callStr <- paste (nsStr, annoTypeStr, seqTypeStr, orgStr, funcStr, md5Str, sep = "")
resourceStr <- paste (switch (resource, abundance = "matrix", resource), "/", sep="")		# a tweak
IDstr <- paste (glom (IDs), "/", sep = "")
paramStr <- paste (param, "?", sep = "")				# a string passed directly

# the API call needs no post-processing in case of
# a file resource (and we return the filename as
# returned by .kbCallRaw)
if (oneof (resource, "sequenceSet", "reads"))
	return (.kbCallRaw (paste (resourceStr, IDstr, paramStr, callStr, sep = ""), toFile))
else
	x <- .kbCallRaw (paste (resourceStr, IDstr, paramStr, callStr, sep = ""))

# otherwise, we process what we have received
y <- fromJSON (x, simplify = TRUE, asText = TRUE)
message ("KbaseKit: ", length (unlist (y)), " elements (before reduction)")

# x is used for the return value of this function
# y is used for the JSON object received
#
# rearrange and rename fields of text resources nicely
if (oneof (resource, "project", "sample", "library", "metagenome", "sequences")) {
	x <- simpleJSONReduction (y)
# ... this sometimes chokes when the retrieved object is incomplete ...
	switch (resource, 
		project = { names (x)[3] <- "kbase" },
		sample = { names (x)[2] <- "kbase" },
		library = { names (x)[2] <- "kbase" },
		metagenome = { names (x)[2] <- "kbase" })
}
# process abundance matrix nicely from out of BIOM format
# ... the eventual plan is to implement an RBIOM class ...
if (resource == "abundance") {
	if (y$matrix_type == "sparse") {
# first we make a full matrix via the provided sparse matrix
# remembering that BIOM indices start at zero
		n <- length (y$data)
		spM <- matrix (unlist (y$data), nrow = n, ncol = 3, byrow = TRUE)
		M <- as.matrix (sparseMatrix (i = 1 + spM [,1], j = 1 + spM [,2], x = spM [,3], dims = y$shape))
# extract the column names that we expect
		n <- y$shape [2]
		s <- character (n)
		for (j in 1:n)  s[j] <- (y$columns [[j]] ["id"])
		colnames (M) <- s
# what the row names should be is debatable
# ... perhaps I should check with others ...
# here, we collapse the taxonomy to a single string
		s <- character (n <- y$shape [1])
		for (j in 1:n)  s[j] <- paste (y$rows [[j]] $metadata$taxonomy, collapse = ";", sep = "")
		rownames (M) <- s
		x <- M
	} else warning ("KbaseKit: non-sparse matrix received; not implemented")
}
# process subsets nicely into a data.frame
else if (resource == "subset") {
	M <- cbind (data.frame (organism = names (y), row.names = NULL, stringsAsFactors = FALSE), count = unlist (lapply (y, length), use.names = FALSE))
	x <- data.frame ( organism = as.vector (with (M, rep (organism, count))), 
		md5 = unlist (lapply (y, names)), 
		count = unlist (y),
		row.names = NULL, stringsAsFactors = FALSE)
}
# ... process sequences nicely ...
else if (resource == "sequences") {
}

# assign appropriate class to the return object
# ... do other resources need to be here? ...
class (x) <- switch (resource,
	project = "kbProject",
	sample = "kbSample",
	library = "kbLibrary",
	metagenome = "kbMetagenome",
	class (x))

# we return filenames visibly and objects invisibly
# ... this is definitely not how output should be done!
# ... probably there should be a "write" method defined
# ... for each class of returned object.  or, they should
# ... not even be objects, but just character vectors, simpler types
if (!is.null (toFile)) {
	sink (file = toFile) ; print (x) ; sink()
	message( "KbaseKit: wrote to ", toFile )
	toFile
	}
else invisible (x)
}

# at one point I thought: "reads" retrieves multiple 
# files per ID (with different extensions), whereas
# "sequenceSet" retrieves only one, but I think that
# turns out to be false.

### if output is to disk, name output files 
### sensibly and according to the given parameter
#if (resource == "reads") {
#	if (toFile == NULL)
#		message ("KbaseKit: using default filenames for multiple file resource")
#	else
#		message ("KbaseKit: using specified filename as prefix for multiple file resource")
#	toFile = ....
#}

# ...may again need this kind of scanning of the parameter string ...
#
#	abundance = (if (1 == length (grep ("format/plain", paramStr, fixed = TRUE))) {
#		temp.file <- tempfile()
#		writeLines (x, temp.file)
#		y <- read.table (temp.file, header = TRUE, row.names = 1, sep = "\t", quote = "", comment.char = "")
#		unlink (temp.file)
#		y } else x),
#	x)

simpleJSONReduction <- function (x) {
	empty <- vapply(x, function (x) (length(x) == 0), TRUE)
	single <- vapply(x, function (x) (length(x) == 1), TRUE)
	y <- x[ !(single | empty)]
	y$more <- as.list (unlist (x [single], use.names = TRUE))
	y
	}

############################################
### default methods for our simple classes
############################################

# yeah this is a big hack

print.kbProject <- function (x, ...) { 
	cat (paste (format (strtrim (names (x$kbase),35), width=35), strtrim (x$kbase, 40), collapse = "\n", sep = "  "), "\n")	
	cat (paste (format (strtrim (names (x$metadata),35), width=35), strtrim (x$metadata, 40), collapse = "\n", sep = "  "), "\n")
	cat (paste (format ("samples", width=35), strtrim (paste (P$samples, collapse = " "), 40), sep = "  "), "\n")
}
summary.kbProject <- function (object, ...) { 
	cat("MG-RAST ID:            ", object$kbase$id,"\n")
	cat("MG-RAST sample ID(s):  ", object$samples, "\n")
	cat("study title:           ", object$metadata["study_title"], "\n")
	cat("study id:              ", object$metadata["id"], "\n")
	cat("project name:          ", object$metadata["project_name"], "\n")
	cat("public:                ", object$metadata["public"], "\n")
}
print.kbSample <- function (x, ...) {
	cat (paste (format (strtrim (names (x$kbase),35), width=35), strtrim (x$kbase, 40), collapse = "\n", sep = "  "), "\n")	
	cat (paste (format (strtrim (names (x$metadata),35), width=35), strtrim (x$metadata, 40), collapse = "\n", sep = "  "), "\n")
}
# maybe should not assume people are using fixed-width fonts
summary.kbSample <- function (object, ...) { 
	cat("MG-RAST ID:             ", object$kbase$id, "\n")
	cat("MG-RAST metagenome ID:  ", object$kbase$metagenome, "\n")
	cat("MG-RAST library ID(s):  ", object$kbase$libraries, "\n")
	cat("title:                  ", object$metadata["TITLE"], "\n" )
	cat("study id:               ", object$metadata["study_id"], "\n" )
	cat("sample name:            ", object$metadata["sample_name"], "\n" )
	cat("sample id:              ", object$metadata["sample_id"], "\n" )
	cat("public:                 ", object$metadata["public"], "\n")
}
print.kbLibrary <- function (x, ...) { 
	cat (paste (format (strtrim (names (x$kbase),35), width=35), strtrim (x$kbase, 40), collapse = "\n", sep = "  "), "\n")	
	cat (paste (format (strtrim (names (x$metadata),35), width=35), strtrim (x$metadata, 40), collapse = "\n", sep = "  "), "\n")
}
summary.kbLibrary <- function (object, ...) { summary.default(object) }
print.kbMetagenome <- function (x, ...) { 
	cat (paste (format (strtrim (names (x$kbase),35), width=35), strtrim (x$kbase, 40), collapse = "\n", sep = "  "), "\n")	
	cat (paste (format (strtrim (names (x$metadata),35), width=35), strtrim (x$metadata, 40), collapse = "\n", sep = "  "), "\n")
}
summary.kbMetagenome <- function (object, ...) { 
	cat("MG-RAST ID:          ", object$kbase$id, "\n")
	cat("MG-RAST project ID:  ", object$kbase$project, "\n")
	cat("project name:        ", object$metadata["project-description_project_name"], "\n" )
	cat("metagenome name:     ", object$metadata["project-description_metagenome_name"], "\n" )
	cat("public:              ", object$metadata["public"], "\n")
}
print.kbSubset <- function (x, ...) { print.default(x) ; print ("class method") }
summary.kbSubset <- function (object, ...) { summary.default(object) }
print.kbAbundance <- function (x, ...) { print.default(x) ; print ("class method") }
summary.kbAbundance <- function (object, ...) { summary.default(object) }


############################################
### hardcoded "parameters" of the package
############################################

### provides some valid API parameters for testing conveniently
.kbTestParams <- function () {
	list (
		project = "92", 
		sample = "mgs3482", 
		library = "mgl3482.4",
		metagenome = c("4443360.3","4443361.3","4443362.3","4443363.3","4443364.3","4443365.3","4443366.3","4443367.3","4443368.3"),
		subset = "", 
		sequenceSet = "", 
		sequences = "", 
		annotation = "", 
		reads = "", 
		abundanceProfile = "mgm4440282.3", 
		abundance = "mgm4440282.3",
		md5 = "5c6cdf00b3b2509879f412d55582af1a",
		matrix = "",
		query = "")
}

### lists relevant servers
.kbServers <- function() {
	list (
		prod = "http://api.metagenomics.anl.gov/",
		prod2 = "http://metagenomics.anl.gov/api.cgi",
		dev = "http://dev.metagenomics.anl.gov/api.cgi/",
		dev2 = "http://dunkirk.mcs.anl.gov/~paczian/mgrastv3/api.cgi/",
		dev3 = "http://dev.metagenomics.anl.gov/api_new.cgi",
		shock = "http://shock.mcs.anl.gov")
	}

# valid namespaces for MG-RAST
# ... this is close, but not exactly right ...
.kbNamespaces <- function ( ) {
	list (M5NR = "M5NR", SwissProt = "SwissProt", GenBank = "GenBank", IMG = "IMG", SEED = "SEED",
	TrEMBL = "TrEMBL", RefSeq = "RefSeq", PATRIC = "PATRIC", eggNOG = "eggNOG", KEGG = "KEGG", RDP = "RDP",
	Greengenes = "Greengenes", LSU = "LSU", SSU = "SSU") 
	}



############################################
### stats tools
############################################
#
# general paradigm:
#
# we have functions to (1) calculate, and to (2) produce images.
# each accepts a filename or object as input
# and returns the result and/or writes it to file

# plotPCO did not seem to do anything 
# (specifically, it did not plot)
# so I dispensed with it

# inputs: matrix
# returns: dist
#
# implements various distances.  transpose, since we go by columns.
kbDist <- function (M, method = "bray-curtis") {
	suppressPackageStartupMessages (require(ecodist))
	if (class (M) != "kbAbundance") warning( "KbaseKit: function argument should be class \"kbAbundance\"")
	if (any (method == c ("bray-curtis", "jaccard", "mahalanobis", "sorensen", "difference")))
		distance (t (M), method = method) 
	else dist (t (M), method = method)
}

# inputs: filename or matrix
# returns: filename, or list of "values" (numeric), "vectors" (matrix), "dist" (dist)
#
# compute distance, scaled eigenvalues, and eigenvectors
# ... consider better way to combine?  c, cbind, data.matrix ...
kbPCO <- function (x, method = "bray-curtis", toFile = NULL, distToFile = NULL) {
	suppressPackageStartupMessages (require(ecodist))

	M <- if (class(x) == "character") data.matrix (read.table (x, row.names=1, header=TRUE, sep="\t", comment.char="", quote=""))
		else as.matrix (x)
#	if (class (M) != "kbAbundance") warning( "KbaseKit: function argument should be class \"kbAbundance\"")
	D <- kbDist (M)
	P <- pco (D)
	scaled <- P$values / sum (P$values)
	names (scaled) <- paste ("PCO", 1:length(scaled), sep = "")
# should both rows and columns be labeled, here?
	dimnames (P$vectors) [[1]] <- dimnames (M) [[2]]
	dimnames (P$vectors) [[2]] <- dimnames (M) [[2]]
	if (!is.null (toFile)) {
		save (P, D, file = toFile)
		toFile
		}
	else list (values = scaled, vectors = P$vectors, dist = D)
	}

# inputs: filename or matrix
# returns: filename or pcaRes
kbPCA <- function (x, N, toFile = NULL ) {
	suppressPackageStartupMessages (require (pcaMethods))

	M <- if (class(x) == "character") data.matrix (read.table (x, row.names=1, header=TRUE, sep="\t", comment.char="", quote=""))
		else as.matrix (x)
	P <- pca (M, nPcs = N)
	if (!is.null (toFile)) {
		save (P, file = toFile)
		toFile
		}
	else P
	}

# inputs: filename or matrix
# returns: filename or NULL
# if no output file specified, attempts to display image
kbPlotPCA <- function (x, N, toFile = NULL ) {
#	, PC1="PC1", PC2="PC2", image_title = image_out, figure_width = 950, figure_height = 950,
# c("color1","color2", ... ,"color_n")  e.g. c("red","red","red")
#	points_color = "red", figure_res = NA, lab_cex= 1, axis_cex = 1, points_text_cex = .8) {

	PC1="PC1"; PC2="PC2"; image_title = "Principal Component Analysis"; figure_width = 700; figure_height = 700;
	points_color = "orange"; figure_res = NA; lab_cex= 1; axis_cex = 1; points_text_cex = .8;

	suppressPackageStartupMessages (require (pcaMethods))
	suppressPackageStartupMessages (require (Cairo))

	M <- if (class(x) == "character") data.matrix (read.table (x, row.names=1, header=TRUE, sep="\t", comment.char="", quote=""))
		else as.matrix (x)
	P <- kbPCA (M, N)
quartz()
#	CairoJPEG (toFile, width = figure_width, height = figure_height, pointsize = 12, res = figure_res, units = "px")
    plot(P@loadings[,PC1],
         P@loadings[,PC2],
         cex.axis = axis_cex,
         cex.lab = lab_cex,
         main = image_title,
         type = "p",
         col = points_color,
         xlab = paste(PC1, "R^2 =", round(my_pcaRes@R2[PC1], 4)),
         ylab = paste(PC2, "R^2 =", round(my_pcaRes@R2[PC2], 4))
         )
    if (points_color != 0)
		points (P@loadings[,PC1], P@loadings[,PC2], col = points_color, pch=19, cex=2 )
# color in the points if the points_color option has a value other than NA
# pch, integer values that indicate different point types (19 is a filled circle)
	text (P@loadings[,PC1], P@loadings[,PC2], labels=rownames(P@loadings), cex = points_text_cex)
#	write.table(P@R2,        file=file_out, sep="\t", col.names=FALSE, row.names=TRUE, append = FALSE)
#	write.table(P(my_pcaRes), file=file_out, sep="\t", col.names=FALSE, row.names=TRUE, append = TRUE)
	dev.off (dev.cur ())
	}

kbDendrograms <- function (file_in, file_out_column =  "col_clust", file_out_row    =  "row_clust",
	dist_method           = "euclidean", # ("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
	clust_method          = "ward",  # ("ward", "single", "complete", "average", "mcquitty", "median", "centroid")
	produce_figures       = FALSE,
	col_dendrogram_width  = 950, col_dendrogram_height = 500,
	row_dendrogram_width  = 950, row_dendrogram_height = 500,
	output_files_prefix   = "my_dendrograms") {
#     This script will perform dendrogram analysis (x and y) of input data
#    using the selected distance/dissimilarity  metric and the selected
#    clustering method.
#    Two ouputs, for sorting the data by row and/or column
#USAGE: MGRAST_dendrograms(
#                              file_in = \"file\",               # input data file, no default,            
#                              file_out_column = \"col_clust\",  # output column clustering indeces, default = \"col_clust\"
#                              file_out_row = \"row_clust\",     # output row clustering indeces,    default = \"row_clust\"
#                              dist_method = (choose from one of the following options)
#                                         # distance/dissimilarity metric, default = \"bray-curtis\"

#                                         \"euclidean\" | \"maximum\"     | \"canberra\"    |
#                                         \"binary\"    | \"minkowski\"   | \"bray-curtis\" |
#                                         \"jacccard\"  | \"mahalanobis\" | \"sorensen\"    |
#                                         \"difference\"
#
#                              clust_method = (choose one of the following options)
#                                          # clustering  method, default = \"ward\"
#
#                                         \"ward\"      | \"single\"      | \"complete\" |
#                                         \"mcquitty\"  | \"median\"      | \"centroid\" |
#                                         \"average\" 
	}

kbPreprocessing <- function(file_in,     # name of the input file (tab delimited text with the raw counts)
                                             file_out = "preprocessed_data",    # name of the output data file (tab delimited text of preprocessed data)

                                             produce_fig = FALSE,     # boolean - to produce a figure (TRUE) or not (FALSE)
                                             image_out = "my_boxplots",   # name of the output image (the boxplot figure)
                                             raw_data_boxplot_title      = "raw data",   # title for the top (raw data boxplot)
                                             centered_data_boxplot_title = "log2(x+1) & centered per sample, scaled 0 to 1 over all samples", # title for the lower (preprocesed data) boxplot
                                             figure_width                = 950,                      # usually pixels, inches if eps is selected; png is default
                                             figure_height               = 1000,                     # usually pixels, inches if eps is selected; png is default
                                             figure_res                  = NA                       # usually pixels, inches if eps is selected; png is default      
                                             ) {
	}

kbSuggestTest <- function (data_file, groups_file, data_type=c("raw", "normalized"), 
	paired = FALSE, file_out="suggest_stat_test.out") {
	}

kbSampleMatrix <- function(file_name, num_perm = 1, perm_type = "sample_rand", 
	write_files = FALSE, perm_dir = "./permutations/", verbose = FALSE, debug = FALSE) {
	}

# data_in is a comma separted list of data points
# groups_in is a comma separted list of group indeces for the points in data_in
kbDoStats <- function (data_file, groups_file, data_type = c("raw", "normalized"),
	sig_test = c("t-test-paired", "Wilcoxon-paired","t-test-un-paired","Mann-Whitney_un-paired-Wilcoxon","ANOVA-one-way", "Kruskal-Wallis"),
	file_out = "my_stats") {
	}


############################################
### the RBIOM class definition
############################################

#as.matrix.RBIOM
#as.sparseMatrix.RBIOM
#as.data.frame.RBIOM

# this should be in onAttach I think:


# this class corresponds to the JSON-based BIOM format
# in general, the desired behavior is:
#	an instance is constructed from text, URL, or file, assumed as JSON
#	an instance is written out as JSON
#	in numerical contexts it behaves as a matrix
#	it can be summarized as plain text


setClass ("RBIOM",	
	representation ("matrix",		# JSON type of each field is noted:
		id = "character",			# string or null
		format = "character",		# string or null
		format_url = "character",	# string
		type = "character",			# string
		generated_by = "character",	# string
		date = "character",			# datetime, ISO 8601 format
		rows = "list",				# list of objects
		columns = "list",				# list of objects
		matrix_type = "character",			# string
		matrix_element_type = "character",	#string
		shape = "integer"				# list of ints
#		, comment = "character"		# omitted, causing trouble - maybe a reserved word?
		),
	prototype ( matrix( NA, nrow=0, ncol=0),
		id = character(0),
		format = character(0),
		format_url = "http://www.biom-format.org/",
		type = c ("OTU table", "Pathway table", "Function table", "Ortholog table", "Gene table","Metabolite table", "Taxon table"),
		generated_by = character(0),
		date = character(0),
		rows = list(),
		columns = list(),
		matrix_type = c ("sparse", "dense"),
		matrix_element_type = c ("int", "float", "unicode"),
		shape = as.integer (c (0, 0))
#		, comment = character(0)
		)
#	, sealed = TRUE
#	contains = c ("matrix")
	)

function () {
m <- matrix (c (5,6,7,8), nrow = 2, ncol = 2)
new ("RBIOM")

initialize.RBIOM <- function () { ... }
summary.RBIOM <- function (x) { 
	print (attr(x,"shape"))
	}

setMethod ("initialize")
setMethod ("summary", signature = c (x = "RBIOM"), summary.RBIOM)
}

