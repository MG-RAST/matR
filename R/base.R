
# bad style alert - need to change print and summary methods
# they should return an object, and that object should have a print method


seeDoc <- function (f) {
	outfile <- tempfile (fileext = ".html")
	browseURL (tools::Rd2HTML (f, outfile))
}


### documentation pages
### "examples" in documentation
### "tests" directory in package with test code
### list definitions contained in this file:  <nouns, verbs>


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

if (0 != length (grep (".kbase.auth.key", ls (1, all = TRUE), fixed = TRUE))) {
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
.kbGetServer <- function () { # getOption (kbase.server) }
.kbase.server }
.kbSetServer <- function ( s ) { #options (kbase.server = s) }
.kbase.server <<- s }

### simple interface to get and set the auth key.  it is set automatically upon loading, if it exists
.kbGetAuth <- function() { #getOption (kbase.auth) }
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
### unsophisticated but robust.
### eventually, implement more carefully 
### with RCurl
############################################
.kbCallRaw <- function (call, toFile = NULL) {
if (0 == length (grep ("?", call, fixed = TRUE))) conj = "?"
else conj = "&"
urlStr <- paste (.kbGetServer(), call, conj, "auth=", .kbGetAuth(), sep = "")
message ("KbaseKit: .kbCallRaw: ", urlStr)

if (is.null (toFile)) readLines (urlStr, warn = FALSE)
else download.file (urlStr, toFile, quiet = TRUE)
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

### perform an md5 lookup in the specified annotation database
### it seems this is not yet implemented in the API
kbAnnotationLookup <- function (md5, namespace) {
warning( "KbaseKit: unimplemented function" )
}

### search metagenomes in mgrast for specified criteria
kbSearchMetagenomes <- function (resource, attribute = NULL, value = NULL) {
### fix me
warning( "KbaseKit: unimplemented function" )
return ()
### .kbCallRaw ()
}

### specify a resource and get it back in an R-friendly form
### here the design idea is:
### this routine handles all API calls WHICH REQUIRE AN ID
### it is an interface, not a wrapper,
### meaning defaults and interaction schema of the API are reconsidered
kbGet <- function( 
	ID,						### vector, definitely
	resource,
	namespace = NULL,		### vector?
	annoType = NULL,
	seqType = NULL,
	org = NULL,				### vector
	func = NULL,			### vector
	md5 = NULL,				### vector
	param = NULL,			### param is any string, passed directly
	toFile = NULL ) {

### make sure the resource type is valid
if ( !warnParams( resource, c("project", "sample", "library", "metagenome", "subset", "sequenceSet", "sequences",  "reads", "abundance"))) return ()

### warn of funtionality not yet implemented
if ( any (resource == c ("sequenceSet", "reads"))) { warning ("KbaseKit: unimplemented function"); return () }

### it is nice to accept IDs in various forms, such as:
### 123 c(123,456) "123;456" c("123","456")
### so we do some checking and tidying here
tidyID <- ID
### fix me .... also, need withPrefix ??
IDstr <- paste (paste (as.character (tidyID), collapse = ";", sep = ""), "/", sep="")
if ( 1 == length (grep (";", IDstr, fixed = TRUE)) &&
	any (resource == c ("project", "sample", "library", "metagenome", "subset", "sequenceSet", "sequences", "reads"))) { 
		warning ("KbaseKit: multiple ID\'s not yet implemented for specified resource"); return () }

### and the API identifies resources slightly differently
resourceStr <- paste (switch (resource, abundance = "matrix", resource), "/", sep="")
paramStr <- paste ( paste (param, collapse = "/", sep = ""), "?", sep="")

annoTypeStr <- if (!is.null (annoType)) paste ("type=", annoType, "&", sep="")
seqTypeStr <- if (!is.null (seqType)) paste ("seq=", seqType, "&", sep="")
orgStr <- if (!is.null (org)) paste ("organism=", org, "&", sep="")
funcStr <- if (!is.null (func)) paste ("function=", func, "&", sep="")
md5Str <- if (!is.null (md5)) paste ("md5s=", md5, "&", sep="")
nsStr <- if (!is.null (namespace)) paste ("source=", namespace, "&", sep="")

### use provided parameters that belong in the specified resource call
callStr <- switch (resource,
	subset = paste (annoTypeStr, nsStr, orgStr, funcStr, sep = ""),
###	sequenceSet = 
	sequences = paste (annoTypeStr, nsStr, seqTypeStr, orgStr, funcStr, md5Str, sep = ""),
###	reads = 
	abundance = paste (annoTypeStr, nsStr, sep = ""), "")

### and warn of unused parameters
if ( !is.null( switch (resource,
		project = c (namespace, annoType, seqType, org, func, md5),
		sample = c (namespace, annoType, seqType, org, func, md5),
		library = c (namespace, annoType, seqType, org, func, md5),
		metagenome = c (namespace, annoType, seqType, org, func, md5),
		subset = c (seqType, md5),
		sequenceSet = c (namespace, annoType, seqType, org, func, md5),
		sequences = NULL,
		reads = c (namespace, annoType, seqType, org, func, md5),
		abundance = c (seqType, org, func, md5), NULL)))
	warning ("KbaseKit: invalid parameter ignored in API call")

x <- .kbCallRaw (paste (resourceStr, IDstr, paramStr, callStr, sep = ""), toFile = toFile)

y <- try (fromJSON (x, simplify = TRUE, asText = TRUE), silent = TRUE)
if ( !inherits (y, "try-error")) {
	message ("KbaseKit: ", length (unlist (y)), " elements (before reduction)")
	x <- simpleJSONReduction (y)
} else message ("KbaseKit: received object is not JSON (may be ok)")

reducedx = switch (resource, 
				   project = { names (x)[3] <- "kbase"; x },
				   sample = { names (x)[2] <- "kbase"; x },
				   library = { names (x)[2] <- "kbase"; x },
				   metagenome = { names (x)[2] <- "kbase"; x },
	subset = x,
###	sequenceSet = 
	sequences = x,
###	reads = 

#	N <- matrix(0, nrow = length (M$rows), ncol = length (M$columns) )
#	for (j in 1:length (M$data))
#		N [1 + M$data [[j]][1], 1 + M$data [[j]][2]] <- M$data [[j]][3]

#	unlist(M$columns)[names(unlist(M$columns))=="id"]
#	unlist(M$rows)[names(unlist(M$rows,recursive=TRUE))=="metadata.taxonomy7"]

	
#	rows[[1:4000]]$metadata[[1]]$taxonomy
#	rows[[1:4000]]$id		# not metagenome ID, something else
	
#	columns[[1:3]][2]		# is metagenome ID
#	data[[1:4000]][1:3]		# data

	abundance = (if (1 == length (grep ("format/plain", paramStr, fixed = TRUE))) {
		temp.file <- tempfile()
		writeLines (x, temp.file)
		y <- read.table (temp.file, header = TRUE, row.names = 1, sep = "\t", quote = "", comment.char = "")
		unlink (temp.file)
		y } else x),
	x)

class (reducedx) <- switch (resource,
	project = "kbProject",
	sample = "kbSample",
	library = "kbLibrary",
	metagenome = "kbMetagenome",
	subset = "kbSubset",
	abundance = "kbAbundance", "")
# sequenceSet
# sequences
# reads

invisible (reducedx)
}


simpleJSONReduction <- function (x) {
empty <- vapply(x, function (x) (length(x) == 0), TRUE)
single <- vapply(x, function (x) (length(x) == 1), TRUE)
y <- x[ !(single | empty)]
y$more <- as.list (unlist (x [single], use.names = TRUE))
y
}


############################################
### default methods for these simple classes
############################################

# use something like this:  str(P,nchar.max=5,list.len=15,vec.len=5,give.attr=FALSE)
# or:  ls.str()
# or: as.matrix(P$more)
# matrix format is very nice for this
# bear in mind the goal to vectorize resource fetching
#
# for print formattings options, see:
#   options, print, print.default, print.data.frame, format
# relevant params are: 
#   right=, max=, quote=

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
### import and export from BIOM format
############################################

fromBIOM <- function (file, keepSparse = TRUE) { 
}

#Constructs a text object in BIOM format from given data
#Names of the rows and columns of \code{obj} must exist, and they are used to populate the appropriate fields of the \code{BIOM} object.
#Type should be one of \code{OTU table}, \code{"Pathway table"}, \code{"Function table"}, \code{"Ortholog table"}, \code{"Gene table"}, \code{"Metabolite table"}, \code{"Taxon table"}
#If \code{toFile} is specified, the object is written, otherwise returned.
toBIOM <- function (obj, type, ID = NULL, comment = "", asSparse = TRUE, toFile = NULL) { 
list(
"id" = ID,
"comment" = comment,
"format" = "BIOM v. xxxx",
"format_url" = "http://www.qiime.org/svn_documentation/documentation/biom_format.html",
"type" = type,
"generated_by" = "KbaseKit v. 0.9",
#"date" = ...,
#"rows" = ,
#"columns" = ,
#"matrix_type" = if (sparse) "sparse" else "dense",
#matrix_element_type = switch ( ) { = "int", = "float", = "str"},
#shape = ,
data = NULL )
}


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
shock = "http://shock.mcs.anl.gov")
}

### valid namespaces for MG-RAST
.kbNamespaces <- function ( ) {  
list (M5NR = "M5NR", SwissProt = "SwissProt", GenBank = "GenBank", IMG = "IMG", SEED = "SEED", TrEMBL = "TrEMBL", RefSeq = "RefSeq", PATRIC = "PATRIC", eggNOG = "eggNOG", KEGG = "KEGG", RDP = "RDP", Greengenes = "Greengenes", LSU = "LSU", SSU = "SSU") 
}

### a few little things
.shew <- function (a) { print (paste (typeof (a), mode (a), class (a))); summary (a) }
.shh <- suppressPackageStartupMessages
.hold <- function (messg = NULL, cmd = NULL) { message (messg); if (!is.null (cmd)) writeLines (paste (options ("prompt"), cmd), sep=""); invisible (readLines (n=1, warn=FALSE))}

warnParams <- function (param, okValues) {
if (any (param == okValues)) return (TRUE)
warning ("KbaseKit: incorrect parameter should be among: ", paste (okValues, collapse = " "))
return (FALSE)
}











############################################
### stats tools
############################################



#     This script will perform dendrogram analysis (x and y) of input data
#    using the selected distance/dissimilarity  metric and the selected
#    clustering method.
#    Two ouputs, for sorting the data by row and/or column
#SAGE: MGRAST_dendrograms(
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
kbDendrograms <- function(
                                file_in,
                                file_out_column =  "col_clust",
                                file_out_row    =  "row_clust",
                                dist_method           = "euclidean", # ("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
                                clust_method          = "ward",  # ("ward", "single", "complete", "average", "mcquitty", "median", "centroid")
                                
                                produce_figures       = FALSE,
                                col_dendrogram_width  = 950,
                                col_dendrogram_height = 500,
                                row_dendrogram_width  = 950,
                                row_dendrogram_height = 500,
                                output_files_prefix   = "my_dendrograms"
                                )
  
{

# load packages
  #suppressPackageStartupMessages(library(matlab))      
  suppressPackageStartupMessages(library(ecodist))
  #suppressPackageStartupMessages(library(Cairo))
  #suppressPackageStartupMessages(library(gplots))
  
}



# data_in is a comma separted list of data points
# groups_in is a comma separted list of group indeces for the points in data_in
kbDoStats <- function (data_file,
                              groups_file,
                              data_type = c("raw", "normalized"),
                              sig_test = c(
                                "t-test-paired", "Wilcoxon-paired",
                                "t-test-un-paired", "Mann-Whitney_un-paired-Wilcoxon",
                                "ANOVA-one-way", "Kruskal-Wallis"
                                ),
                              file_out = "my_stats")
  
{
  library(stats)
  library(nlme)
}


# implements various distances.  transpose, since we go by columns.
# returns class dist
kbDist <- function (M, method = "bray-curtis") {
	suppressPackageStartupMessages (require(ecodist))
	if (class (M) != "kbAbundance") warning( "KbaseKit: function argument should be class \"kbAbundance\"")
	if (any (method == c ("bray-curtis", "jaccard", "mahalanobis", "sorensen", "difference")))
		distance (t (M), method = method) 
	else dist (t (M), method = method)
}

# compute distance, scaled eigenvalues, and eigenvectors
# and returns as a list
# returns in the same format as base::eigen() namely, a list with components "values" and "vectors" 
# consider better way to combine? ... c cbind data.matrix
kbPCO <- function (M, method = "bray-curtis") {
	suppressPackageStartupMessages (require(ecodist))
	if (class (M) != "kbAbundance") warning( "KbaseKit: function argument should be class \"kbAbundance\"")
	D <- kbDist (M)
	P <- pco (D)
	scaled <- P$values / sum (P$values)
	names (scaled) <- paste ("PCO", 1:length(scaled), sep = "")
# should both rows and columns be labeled, here?
	dimnames (P$vectors) [[1]] <- dimnames (M) [[2]]
	dimnames (P$vectors) [[2]] <- dimnames (M) [[2]]
	list (values = scaled, vectors = P$vectors, dist = D)
}


kbPlotPCO <- function(
file_in,
input_dir = "./",
output_PCoA_dir = "./",
print_dist = FALSE,
output_DIST_dir = "./",
dist_method = "bray-curtis",
headers = FALSE
) {

}
	
	
kbPlotPCA <- function(file_in,
                             file_out = "my_pca",
                             
                             num_PCs = 2,

                             produce_fig = FALSE,
                             PC1="PC1",
                             PC2="PC2",
                             
                             image_out = "my_pca",
                             image_title = image_out,
                             figure_width = 950,
                             figure_height = 950,
                             points_color = "red",  #c ("color1","color2", ... ,"color_n")  e.g. c("red","red","red")
                             figure_res = NA,
                             lab_cex= 1,
                             axis_cex = 1,
                             points_text_cex = .8)
  
{
  suppressPackageStartupMessages(library(Cairo))
  suppressPackageStartupMessages(library(pcaMethods))
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
                                             ) 

{ suppressPackageStartupMessages(library(Cairo)) }


kbSuggestTest <- function (data_file, groups_file, data_type=c("raw", "normalized"), paired = FALSE, file_out="suggest_stat_test.out")
{ }


kbSampleMatrix <- function(file_name, num_perm = 1, perm_type = "sample_rand", write_files = FALSE, perm_dir = "./permutations/", verbose = FALSE, debug = FALSE)
{ }



#buildHTMLDocs <- function () {
#	for (d in docs) tools::Rd2HTML (d, paste ("./html/", unlist (strsplit (d, ".", fixed = TRUE)) [1], ".html", sep=""), Links = tools::findHTMLlinks ())
#}

