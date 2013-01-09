
############################################
### DATA OBJECTS AVAILABLE UPON LOADING matR
###
### there is some subtlety regarding the availabilty
### of different kinds of objects during installation, 
### testing, demos, and interactive runtime.
############################################

# These data need to be available to matR and to the user.
# Note: objects declared here are not available to package code 
# until runtime (an empirical discovery).

view.params <- list (
	entry = c ("counts", "normed.counts", "no.singletons", "normed.no.singletons", "evalue", "length", "percentid"),
	annot = c ("function", "organism"),
	level = list (taxa = c ("domain", "phylum", "class", "order", "family", "genus", "species", "strain"),
								`function` = c ("level1", "level2", "level3", "function")),
	`source` = list (rna = c ("M5RNA", "RDP", "Greengenes", "LSU", "SSU"),
									 ontology = c ("NOG", "COG", "KO", "Subsystems"),
									 protein = c ("M5NR", "SwissProt", "GenBank", "IMG", "SEED", "TrEMBL", "RefSeq", "PATRIC", 
									 						 "eggNOG", "KEGG")))

# possibly parameters should be specified in full for each view, here (and possibly not)
default.views <- list (
	raw = c (entry = "counts"),
	nrw = c (entry = "normed.counts"),
	nos = c (entry = "no.singletons"),
	nns = c (entry = "normed.no.singletons"))

id.ex <- list (
	project = "92",
	project2 = "102",
	sample = "mgs3482",
	library = "mgl3482.4",
	ids = c ("4443360.3","4443361.3", "4443362.3"),
	ids2 = c ("4492992.3", "4492991.3", "4492990.3", "4492989.3", "4492988.3", "4492987.3", "4492986.3", "4492985.3", "4492984.3", "4492983.3", "4492982.3", "4492981.3", "4492980.3"))

############################################
### Session configuration information resides in
### this closure.  In particular this forces 
### the auth key to be entered per-session, and discourages
### it from being stored in .RData or .Rhistory.
### A significant improvement would be to allow _other_
### options to be stored from session to session.
#############################################

msession <- (function () {
# communications
	auth.X <- ""
	getAuth <- function () { auth.X }
	setAuth <- function () { message ("Enter auth key on a single line:"); auth.X <<- readLines (n = 1, warn = FALSE); auth.X}

# hopefully the need for this will go away.
# in the early days, the API server was constantly moving
	server.X <- ""
	servers.X <- list (
		prod = "http://api.metagenomics.anl.gov/",
		prod2 = "http://metagenomics.anl.gov/api.cgi/",
    test = "http://test.metagenomics.anl.gov/api.cgi/",
		dev = "http://dev.metagenomics.anl.gov/api.cgi/",
		dev2 = "http://dunkirk.mcs.anl.gov/~paczian/mgrastv3/api.cgi/",
		dev3 = "http://dev.metagenomics.anl.gov/api_new.cgi/",
		shock = "http://shock.mcs.anl.gov",
		api2 = "http://api.metagenomics.anl.gov/api2.cgi/",
		api2test = "http://test.metagenomics.anl.gov/api2.cgi/")
	server <- function (s = NULL) if (is.null (s)) server.X else server.X <<- s
	servers <- function () servers.X

# currently unused.  idea is to be aware of when we are offline.
	network <- TRUE								# ...possibly useful (though not at present)

# for API debugging, we maintain a stack of the last 10 API URLs attempted
	urls.X <- rep ("", 10)
	urlsn.X <- 1
	urls <- function (s = NULL) {
		if (is.null (s)) 
			c (urls.X [urlsn.X:1], 
				 if (urlsn.X != 10) urls.X [10:(urlsn.X + 1)]
				 else NULL)
		else {
			urlsn.X <<- urlsn.X + 1
			if (urlsn.X == 11) urlsn.X <<- 1
			urls.X [urlsn.X] <<- s
		}
	}

# extra messages are printed throughout the code for debugging, if requested
	verbose.X <- FALSE
	verbose <- function (t = NULL) if (is.null (t)) verbose.X else verbose.X <<- t
	
# path to be used as directory for all files
	path.X <- ""
	path <- function (s = NULL) if (is.null (s)) path.X else path.X <<- s

# "import" configuration, mostly for read.table
	imp.X <- list (
		type = "default",						# "text", "binary", "default"
		header = TRUE,
		sep = "\t",
		quote = "",
		row.names = 1)
	imp <- function (...) {
		L <- list (...)
		if (length (L) == 0) imp.X
		else imp.X <<- resolveMerge (L, imp.X)
	}

# "export" configuration, mostly for write.table
	exp.X <- list (
		type = "default",						# "text", "binary", "default"
		append = FALSE,
		quote = FALSE,
		sep = "\t",
		na = "NA",
		row.names = TRUE,
		col.names = TRUE)
	exp <- function (...) {
		L <- list (...)
		if (length (L) == 0) exp.X
		else exp.X <<- resolveMerge (L, exp.X)
	}

# global graphical defaults, for a poor man style sheet
	par.X <- list (
		file = NA,
		type = "png",
		title = "matR",
# 		canvas = "white",
# 		bg = "white",
# 		fg = "black",
#     col = "blue",
# 		col.main = "blue",
		pointsize = 10,
		units = "in",
		width = 5,
		height = 5,
		pch = 20)
	par <- function (...) {
		L <- list (...)
		if (length (L) == 0) par.X
		else par.X <<- resolveMerge (L, par.X)
	}

	debug <- function () {
		file.name <- paste (getwd(), "/matR-debug-", 
												gsub ("[^[:alnum:]+]", format (Sys.time (), "%m-%b-%Hh%Mm%Ss-%Z"), repl = "-"), ".txt", sep = "")
		sink (file = file.name)
		cat ("\n-----------------------------------------------------------------------R VERSION\n")
		print (R.Version())
		cat ("\n------------------------------------------------------------------------PACKAGES\n")
		print (installed.packages () [,c ("Package", "Version", "Built")])
		cat ("\n----------------------------------------------------------------------------URLs\n")
		print (msession$urls())
		cat ("\n---------------------------------------------------------------WORKSPACE (SHORT)\n")
		print (ls())
		cat ("\n----------------------------------------------------------------WORKSPACE (LONG)\n")
		print (ls.str())
		if (file.exists(".Rprofile")) {
			cat ("\n-------------------------------------------------------------------------PROFILE\n")
			cat (readLines (".Rprofile"), sep = "\n")
		}
		cat ("\n-------------------------------------------------------------------------HISTORY\n")
		t <- tempfile()
		savehistory (t)
		cat (readLines (t), sep = "\n")
		unlink (t)
		sink ()
		cat (
			"Please email a description of the problem to mg-rast@metagenomics.anl.gov, along with this file:", 
			file.name,
			"Note: this file contains session information such as your command history.",
			"If you have related privacy concerns, please review it before emailing.\n", sep = "\n")
		}

# the user interface to session configuration is via these functions
	list (getAuth = getAuth,
				setAuth = setAuth,
				server = server, 
				servers = servers, 
				verbose = verbose,
				urls = urls,
				path = path,
				imp = imp,
				exp = exp,
				par = par,
				debug = debug)
	}) ()

### ... some way of saving options and importing on attach, 
### ... either an independent config file, or .Rprofile, or using options()
