
############################################
### DATA OBJECTS AVAILABLE UPON LOADING matR
############################################

samp <- list (
	project = "92",
	project2 = "102",
	sample = "mgs3482",
	library = "mgl3482.4",
	ids = c ("4443360.3","4443361.3", "4443362.3"),
	ids2 = c ("4492992.3", "4492991.3", "4492990.3", "4492989.3", "4492988.3", "4492987.3", "4492986.3", "4492985.3", "4492984.3", "4492983.3", "4492982.3", "4492981.3", "4492980.3"))
guts <- c (cow.rumen.1 = "4441679.3", cow.rumen.2 = "4441680.3", cow.rumen.3 = "4441682.3",
			fish.gut.1 = "4441695.3", fish.gut.2 = "4441696.3",
			mouse.lean = "4440463.3", mouse.obese = "4440464.3")
waters <- c ("4440424.3", "4440423.3", "4440439.3", "4440422.3", "4440412.3", "4440414.3", "4440440.3", "4440413.3", "4440411.3", 
				"4443681.3", "4443682.3", "4443684.3", "4443679.3", "4443683.3", "4443680.3", "4441096.3", "4442583.3", "4441095.3", 
				"4443750.3", "4443762.3", "4443749.3", "4443746.3", "4443747.3", "4443745.3")
names (waters) <- c (paste ("fresh", 1:15, sep = ""), paste ("spring", 1:9, sep = ""))



############################################
### Package configuration information resides in
### this closure.  In particular this forces 
### the auth key to be entered per-session, 
### and discourages it from being stored 
### in .RData or .Rhistory
#############################################

mconfig <- (function () {
# communications
	auth.X <- ""
	server.X <- ""
	servers.X <- list (
		prod = "http://api.metagenomics.anl.gov/",
		prod2 = "http://metagenomics.anl.gov/api.cgi/",
    test = "http://test.metagenomics.anl.gov/api.cgi/",
		dev = "http://dev.metagenomics.anl.gov/api.cgi/",
		dev2 = "http://dunkirk.mcs.anl.gov/~paczian/mgrastv3/api.cgi/",
		dev3 = "http://dev.metagenomics.anl.gov/api_new.cgi/",
		shock = "http://shock.mcs.anl.gov")

	network <- TRUE								# ...possibly useful (though not at present)
	lastURL.X = ""								# .mCallRaw sets this at every call in case of error
	verbose.X <- FALSE

# file input and output
	path.X <- ""									# all filenames are prefaced with this
# for read.table
	imp.X <- list (
		type = "default",						# "text", "binary", "default"
		header = TRUE,
		sep = "\t",
		quote = "",
		row.names = 1)
# for write.table
	exp.X <- list (
		type = "default",						# "text", "binary", "default"
		append = FALSE,
		quote = FALSE,
		sep = "\t",
		na = "NA",
		row.names = TRUE,
		col.names = TRUE)

# # global graphical defaults; a poor man style sheet
	par.X <- list (
		file = NA,
		type = "png",
		title = "matR",
		canvas = "white",
		bg = "white",
		fg = "black",
    col = "blue",
		col.main = "blue",
		pointsize = 10,
		units = "in",
		width = 5,
		height = 5,
		pch = 20)

	getAuth <- function () { auth.X }
	setAuth <- function () { message ("Enter auth key on a single line:"); auth.X <<- readLines (n = 1, warn = FALSE); auth.X}
	server <- function (s = NULL) if (is.null (s)) server.X else server.X <<- s
	servers <- function () servers.X
	verbose <- function (t = NULL) if (is.null (t)) verbose.X else verbose.X <<- t
	lastURL <- function (s = NULL) if (is.null (s)) lastURL.X else lastURL.X <<- s
	path <- function (s = NULL) if (is.null (s)) path.X else path.X <<- s
	imp <- function (...) {
		L <- list (...)
		if (length (L) == 0) imp.X
		else imp.X <<- resolveMerge (L, imp.X)
		}
	exp <- function (...) {
		L <- list (...)
		if (length (L) == 0) exp.X
		else exp.X <<- resolveMerge (L, exp.X)
		}
	par <- function (...) {
		L <- list (...)
		if (length (L) == 0) par.X
		else par.X <<- resolveMerge (L, par.X)
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
