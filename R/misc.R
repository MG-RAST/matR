
############################################
### MISC ROUTINES NEEDED ACROSS THE PACKAGE
############################################

# Remember we are namespace operator for functions from Matrix, stats, nlme
# ERROR HANDLING / SIGNALING IN GENERAL NEEDS TO BE MORE IN THE PROPER IDIOM
# need to change print and summary methods
# they should return an object, and that object should have a print method
# documentation pages


# this is good for inspecting objects
shew <- function (a) { 
	cat ("typeof =", typeof (a), "  mode =", mode (a), "  class =", class (a), "\nsummary:\n")
	print ("dimensions = ", dim (a))
	summary (a) 
	}

# abbreviations are nice
shh <- suppressPackageStartupMessages

# this is good for building demos
hold <- function (messg = NULL, cmd = NULL) { 
	message (messg); 
	if (!is.null (cmd)) writeLines (paste (options ("prompt"), cmd, sep=""))
	invisible (readLines (n=1, warn=FALSE))
	}

# open documentation page in a browser
seeDoc <- function (f) {
	outfile <- tempfile (fileext = ".html")
	browseURL (tools::Rd2HTML (f, outfile))
	}

# I forgot why exactly I wanted this
buildHTMLDocs <- function (docs) {
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
		message ("matR: semicolon interpreted as separator")
		TRUE
		} 
	else FALSE
	}

### we want to facilitate use of this package in parts
### therefore we minimize strict dependencies in the DESCRIPTION file
### and check in every function
gotLib <- function (P) {
	if ( !library (P, character.only = TRUE, quietly = TRUE, warn.conflicts = TRUE, logical.return = TRUE)) {
		message ("matR: call requires package ", P)
		FALSE
		}
	else TRUE
}
