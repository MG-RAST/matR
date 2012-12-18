
############################################
### PACKAGE INITIALIZATION / LOADING
############################################

### we want to facilitate use of this package in parts, and also make installation easy.
### therefore we minimize actual dependencies in the DESCRIPTION file, check for libraries
### as necessary, and provide a function to install suggested "dependencies"

.onAttach <- function (libname, pkgname) {
	mconfig$server (mconfig$servers()$api2)
	packageStartupMessage (
    "matR: metagenomics analysis tools for R (", packageVersion ("matR"), ")\n")
	options (warn = 1, width = 150, timeout = 300, digits = 2)
	packageStartupMessage (
		"access private data--\tmconfig$setAuth()\n",
		"demos and tutorials--\tdemo(package=\"matR\")\n",
    "example metagenomes--\tdata(package=\"matR\")\n",
    "general help & info--\tpackage?matR")
	pkgs <- hazPackages()
	if (!all (pkgs)) packageStartupMessage (
	  "Suggested package(s) missing: ", paste (names (pkgs) [!pkgs] , collapse = " "), "\n",
	  "Use dependencies() to install them")
	}

dependencies <- function (prompt = TRUE) {
	need <- !hazPackages()
	if (any (need)) {
		message (
      "Suggested package(s) missing: ", paste (names (need) [need], collapse = " "), "\n")
		if (prompt) {
			chooseBioCmirror (graphics = FALSE)
			cat ("\n")
			chooseCRANmirror (graphics = FALSE)
			cat ("\n")
			setRepositories (graphics = FALSE)
			cat ("\n")
			}
		install.packages (names (need) [need])
		haz <- hazPackages()
		if (all (haz)) message (
		  "\nSuccess!  All suggested packages have been installed.\n",
		  "Quit and restart R for changes to take effect.")
		else message (
		  "\nSuggested package(s) could not be installed: ", paste (names (haz) [!haz], collapse = " "))
		}
	else message (
	  "All suggested packages appear to be installed.")
	}

hazPackages <- function() {
  me <- packageDescription ("matR")
  need <- unlist (strsplit (c (me$Imports, me$Suggests), "[^[:alnum:]\\.]+"))
  sapply (need, function (x) length (find.package (x, quiet = TRUE)) > 0)
}

.Last.lib <- function (libpath) { 
}
