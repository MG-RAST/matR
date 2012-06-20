############################################
### PACKAGE INITIALIZATION / LOADING
############################################


.onAttach <- function (libname, pkgname) {

# Ignore this stuff which belongs to an older concept:
#
#	packageStartupMessage ("matR: set server prior to loading with session option \"matRServer\"")
#	.APIServer <<- options ("matRServer")$matRServer
#	if (is.null (.APIServer)) .APIServer <<- .APIServers$prod
#	packageStartupMessage ("matR: server: ", .APIServer)

	mConfig$server (mConfig$servers()$prod)
	packageStartupMessage ("matR: server: ", mConfig$server())


# ... would be good to test the network here ...
	packageStartupMessage ("matR: not checking network access")

# ... are there other options we should set?
# ... for instance: print.default, download.file.method ?
	options (warn = 1, width = 150, timeout = 300, digits = 2)
	packageStartupMessage( "matR: this package sets global option(s): warn = 1, width = 150, timeout = 300, digits = 2")

# Also ignore this stuff, also an older concept:
#
# this approach (commented out) is insecure
# since setting the option will remain in .Rhistory
# which quite possibly has insecure permissions:
#
#	.APIAuth <<- options ("matRAuth")$matRAuth
#	if (is.null (.APIAuth)) {
#		.APIAuth <<- ""
#		packageStartupMessage ( "matR: no auth key" )
#		}
#	else packageStartupMessage ("matR: using auth key: ", .APIAuth )
#
# we could alternatively prompt for the auth key directly
# (probably a highly non-standard thing to do DURING
# installation of a package), which may create a nuisance
# for batch scripting ...
#	cat ("matR: enter auth key on a single line [none]: ")
#	.APIAuth <<- readLines (n = 1, warn = FALSE)
#	packageStartupMessage ("matR: not checking auth key")

# Instead configuration information including auth key
# resides in a closure object defined in matR/data
	packageStartupMessage ("matR: set your auth key with mConfig$auth()")

	packageStartupMessage (paste (
		"\nThis code is under active development.",
		"Feedback, feature suggestions, and bug reports are welcome.",
		"A few things to try:   demo(matR)   package?matR   ?AbundanceRetrieval   utils::example(orgMatrix)", 
		sep = "\n"))

	needed <- missingPackages ()
	if (length (needed) > 0)
		packageStartupMessage (paste (
			"\nFunctionality will be limited by missing package(s):", needed,
			"To install, try: dependencies()", 
			sep = "\n"))
	}

############################################
### hooks that are unused for now:
### .onLoad <- function (libname, pkgname) { }
### .onUnload <- function (libname) { }
############################################

.Last.lib <- function (libpath) { 
# these variables belong to an older concept
#
#	rm (.APIAuth, .APIServer)
	}

# these have been replaced by mConfig$server() and mConfig$auth()
#
# .getAPIServer <- function () { .APIServer }
# .getAPIAuth <- function() { .APIAuth }


dependencies <- function () {
	needed <- missingPackages ()
	if (length (needed) > 0) {
		message ("Suggested package(s) from CRAN, Bioconductor, and/or R-Forge missing from this installation: \n", needed)
		message ("Just press <return> to choose from known repository mirrors (this will set session options),")
		message ("or enter repository URLs to search (one per line, blank line to end):")
		repos <- scan (what = "character", quiet = TRUE)
		if (length (repos) == 0) {
			message ("Press <return>, then choose a CRAN mirror")
			chooseCRANmirror (graphics = FALSE)
			message ("Press <return>, then choose a Bioconductor mirror")
			chooseBioCmirror (graphics = FALSE)
			message ("Press <return>, then follow the prompt to select CRAN, Bioconductor, and R-Forge as session repositories")
			setRepositories (graphics = FALSE)
			install.packages (needed)
			}
		else install.packages (needed, repos = repos)
		needed <- missingPackages ()
		if (length (needed) == 0) message ("\nSuccess!  All suggested packages are installed.")
		else message ("\nSuggested package(s) could not be installed: \n", needed)
		}
	else message ("All suggested packages are installed.")
	message ("You may want to run update.packages() if you haven\'t lately.")
	}

missingPackages <- function () { 
	P <- installed.packages ()
#	D <- paste (P ["matR", c ("Imports", "Suggests")], collapse = " ")		# how to check my own name?
#	D less P			# return as character; character(0) for none
	}

