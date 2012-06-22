
############################################
### DATA OBJECTS AVAILABLE UPON LOADING matR
############################################

sources <- list (
	m5rna = "m5rna", rdp = "RDP", greengenes = "Greengenes", lsu = "LSU", ssu = "SSU",				# rna
	nog = "NOG", cog = "COG", ko = "KO", go = "GO", subsystems = "Subsystems",						# ontology
	m5nr = "m5nr", swissprot = "SwissProt", genbank = "GenBank", img = "IMG", seed = "SEED",		# protein
	TrEMBL = "TrEMBL", refseq = "RefSeq", patric = "PATRIC", eggnog = "eggNOG", kegg = "KEGG")
orgLevels <- c ("domain", "phylum", "class", "order", "family", "genus", "species", "strain")
funcLevels <- c ("level1", "level2", "level3", "function")
samp <- list (
	project = "92",
	project2 = "102",
	sample = "mgs3482",
	library = "mgl3482.4",
	ids = c ("4443360.3","4443361.3", "4443362.3"),
	ids2 = c ("4441679.3", "4441680.3", "4441682.3", "4441695.3", "4441696.3", "4440463.3", "4440464.3"),
	ids3 = c ("4492992.3", "4492991.3", "4492990.3", "4492989.3", "4492988.3", "4492987.3", "4492986.3", "4492985.3", "4492984.3", "4492983.3", "4492982.3", "4492981.3", "4492980.3")
	)

############################################
### Package configuration information resides in
### this closure.  In particular this forces 
### the auth key to be entered per-session, 
### and discourages it from being stored 
### in .RData or .Rhistory
#############################################

mConfig <- (function () {
# communications
	authX <- ""
	serverX <- ""
	serversX <- list (
		prod = "http://api.metagenomics.anl.gov/",
		prod2 = "http://metagenomics.anl.gov/api.cgi/",
		dev = "http://dev.metagenomics.anl.gov/api.cgi/",
		dev2 = "http://dunkirk.mcs.anl.gov/~paczian/mgrastv3/api.cgi/",
		dev3 = "http://dev.metagenomics.anl.gov/api_new.cgi/",
		shock = "http://shock.mcs.anl.gov")

	network <- TRUE								# ...possibly useful (though not at present)
	lastURLX = ""								# .mCallRaw sets this at every call in case of error
	verboseX <- FALSE

# file input and output
	pathX <- "./"								# all filenames are prefaced with this
# for read.table
	impX <- list (
		type = "default",						# "text", "binary", "default"
		header = TRUE,
		sep = "\t",
		quote = "",
		row.names = 1)
# for write.table
	expX <- list (
		type = "default",						# "text", "binary", "default"
		append = FALSE,
		quote = "",
		sep = "\t")

# # global graphical defaults; a poor man style sheet
	parX <- list (
		device = "Cairo",						# "native", "Cairo"
		reuseDevice <- FALSE,					# open a new window for each graphical rendering?
# for Cairo
		file = "out.png",
		type = "png",							# default file type: "png","jpg","pdf","ps"
# for par (general purpose)
		bg = "black",
		fg = "white",
		las = 0,
		col.main = "blue",
		col = "red",
		pointsize = 12,
		units = "px",
# for various plotting and rendering
		main = "Some Science",
		width = 400,
		height = 400,
		cx = 1, cy = 1,
		pch = 19,
# for pca
		plab = "$ids",							# an encoded metadata reference?
		pcol = "red",
# for heatmap
		row.dendr = TRUE,
		col.dendr = FALSE)

	getAuth <- function () { authX }
	setAuth <- function () { message ("Enter auth key on a single line:"); authX <<- readLines (n = 1, warn = FALSE) }
	server <- function (s = NULL) if (is.null (s)) serverX else serverX <<- s
	servers <- function () serversX
	verbose <- function (t = NULL) if (is.null (t)) verboseX else verboseX <<- t
	lastURL <- function (s = NULL) if (is.null (s)) lastURLX else lastURLX <<- s
	path <- function (s = NULL) if (is.null (s)) pathX else pathX <<- s
	imp <- function (...) {
		L <- list (...)
		if (length (L) == 0) impX
		else impX <<- resolveMerge (L, impX)
		}
	exp <- function (...) {
		L <- list (...)
		if (length (L) == 0) expX
		else expX <<- resolveMerge (L, expX)
		}
	par <- function (...) {
		L <- list (...)
		if (length (L) == 0) parX
		else parX <<- resolveMerge (L, parX)
		}

	list (getAuth = getAuth,
		setAuth = setAuth,
		server = server, 
		servers = servers, 
		verbose = verbose,
		lastURL = lastURL,
		path = path,
		imp = imp,
		exp = exp,
		par = par)
	}) ()

### ... some way of saving options and importing on attach, 
### ... either an independent config file, or .Rprofile, or using options()
