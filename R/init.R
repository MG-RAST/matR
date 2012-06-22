
############################################
### PACKAGE INITIALIZATION / LOADING
############################################

.onAttach <- function (libname, pkgname) {
	mConfig$server (mConfig$servers()$prod)
	packageStartupMessage ("matR: server: ", mConfig$server())
# ... would be good to test the network here ...
	packageStartupMessage ("matR: not checking network access")
# ... are there other options we should set?
# ... for instance: print.default, download.file.method ?
	options (warn = 1, width = 150, timeout = 300, digits = 2)
	packageStartupMessage (
		"matR: this package sets global option(s): warn = 1, width = 150, timeout = 300, digits = 2")
# Configuration information including auth key resides in a closure
# defined in matR/data.  This avoids an auth key ending up in .Rhistory or .RData
	packageStartupMessage ("matR: set your auth key with mConfig$auth()")

	packageStartupMessage (paste (
		"\nThis code is under active development.",
		"Feedback, feature suggestions, and bug reports are welcome.",
		"A few things to try:   demo(matR)   package?matR   ?AbundanceRetrieval   utils::example(orgMatrix)", 
		sep = "\n"))
	needed <- missingPackages ()
	if (length (needed) > 0)
		packageStartupMessage (paste (
			"\nFunctionality will be limited by missing package(s):\n  ", paste (needed, collapse = " "),
			"\nTo install, try: dependencies()", sep = ""))
	}

.Last.lib <- function (libpath) { 
# nothing to see here
	}

### we want to facilitate use of this package in parts, and also make installation easy.
### therefore we minimize strict dependencies in the DESCRIPTION file, check for libraries
### as necessary, and provide a function to install dependencies
dependencies <- function () {
	needed <- missingPackages ()
	if (length (needed) > 0) {
		message ("Suggested package(s) from CRAN, Bioconductor, and/or R-Forge are missing:")
		message ("  ", paste (needed, collapse = " "))
		message ("Press <return> to choose among known repository mirrors (this will set session options),")
		message ("or, provide repository URLs to search (one per line, blank line to end):")
		repos <- scan (what = "character", quiet = TRUE)
		if (length (repos) == 0) {
			cat ("\n")
			chooseCRANmirror (graphics = FALSE)
			cat ("\n")
			chooseBioCmirror (graphics = FALSE)
			cat ("\n")
			setRepositories (graphics = FALSE)
			message ("Please wait...")
			install.packages (needed)
			}
		else install.packages (needed, repos = repos)
		needed <- missingPackages ()
		if (length (needed) == 0) 
			message ("\nSuccess!  All suggested packages are installed.",
				"\nQuit and restart R for changes to take effect.")
		else message ("\nSuggested package(s) could not be installed: \n  ", paste (needed, collapse = " "))
		}
	else message ("All suggested packages are installed.\n",
			"You may want to run update.packages() if you haven\'t lately.")
	}

missingPackages <- function () { 
	P <- installed.packages ()
# how to check my own name?  that would be slicker
	D <- strsplit (paste (P ["matR", c ("Imports", "Suggests")], collapse = " "), "[^[:alnum:]\\.]+") [[1]]
	D [!sapply (D, function (x) { x %in% rownames(P) })]
	}
