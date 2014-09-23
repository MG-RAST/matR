
#----------------------------------------------------
#  Package initialization
#
#----------------------------------------------------

.onAttach <- function (libname, pkgname) {

	packageStartupMessage (tagline ())

	options (warn = 1, timeout = 300, digits = 2)
	pkgs <- hazPackages()
	if (!all (pkgs)) packageStartupMessage (
	  "Suggested package(s) missing: ", paste (names (pkgs) [!pkgs] , collapse = " "),
	  "\nUse dependencies() to install them")
	}

.Last.lib <- function (libpath) { 
	}
