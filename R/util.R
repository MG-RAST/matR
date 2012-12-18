
############################################
### MISC ROUTINES NEEDED ACROSS THE PACKAGE
###
### generally we err on the side of moving
### simple helper functions into this file,
### out of the source file where they might
### mainly be used
###
### simple stuff only here
############################################

### two functions for demos: each will step through a script, line by line.
### this one reads the file as text and echoes each line exactly.
### each command must fit on a line, and blank lines are simply echoed.
stepper <- function (file) {
	lines <- readLines (file)
	for (j in 1:length (lines))
		if (lines [j] != "") {
			cat (getOption ("prompt"), lines [j], sep = "")
			readLines (n = 1, warn = FALSE)
### NOTE: probably change eval() to occur in parent environment
			R <- withVisible (eval.parent (parse (text = lines [j])))
			if (R$visible && !is.null (R$value)) print (R$value)
			}
		else cat ("\n")
	}

### this one parses the whole file first.  commands may span lines.
### comments are not displayed.  commands are reformatted to standard appearance.
stepper2 <- function (file) {
	exprs <- parse (file = file)
	for (j in 1:length (exprs)) {
		cat (getOption ("prompt"), as.character (exprs [j]), sep = "")
		readLines (n = 1, warn = FALSE)
		eval (exprs [j])
		}
	}

demo2 <- function (s) stepper (paste (path.package ("matR"), "/demo/", s, ".R", sep = ""))


### custom matrix and list printing for general use and also for our class methods
matrixPrinter <- function (x, ...) {
	m <- min (nrow (x), 15)
	n <- min (ncol (x), 5)
	if (m > 0 && n > 0) {
# not fully sure why ths is necessary
# seems "rownames<-" is somwhat sloppily implemented for "Matrix"
		if (length (rownames (x)) > 0) rownames (x) <- abbrev (rownames (x), getOption ("width") / 3, "middle")
		print (x [1:m, 1:n])
		cat ("  <truncated from", nrow (x), "rows and", ncol (x), "columns>\n")
		}
	else cat ("  <zero rows and/or columns>\n")
	}

### Pretty printing of a restricted class of list structures, adaptive to screen width:
### the only atomic (non-list) elements in a traversal are length-one character vectors
### this is for metadata but also handy in general; assumes that listify() has been applied
listPrinter <- function (x) {
	n <- 0
	count <- function (x)
		if (length (x) > 0)
			for (j in 1:length (x)) {
				n <<- n + 1
				if (length (x [[j]]) > 1) count (x [[j]])
				}
	count (x)

	items <- character (n)
	k <- 1
	build <- function (x, prefix)
		if (length (x) > 0)
			for (j in 1:length (x))
				if (length (x [[j]]) == 1) {
					items [k] <<- x [[j]]
					names (items) [k] <<- 
						if (is.null (names (x) [j]) || is.na (names (x) [j]) || nchar (names (x) [j]) == 0)
							paste (prefix, "[[", j, "]]", sep = "")
						else paste (prefix, "$", names (x) [j], sep = "")
					k <<- k + 1
					}
				else {
					items [k] <<- ""
					names (items) [k] <<- 
						if (is.null (names (x) [j]) || is.na (names (x) [j]) || nchar (names (x) [j]) == 0)
							paste (prefix, "unnamed: [[", j, "]]", sep = "")
						else paste (prefix, "$", names (x) [j], sep = "")
					k <<- k + 1
					build (x [[j]], paste ("  ", prefix, sep = ""))
					}
	build (x, "")

	twoColPrint (items, "middle", "right")
	}

### convert metadata into a uniform, recursive structure:
###
### the only atomic (non-list) elements in a traversal are length-one character vectors
### This makes element access very simple, e.g.:
###   m$metadata$env_package$habitat
listify <- function (L) {
	if (length (L) > 0)
		for (j in 1:length (L))
			if (length (L [[j]]) == 0)							
				L [[j]] <- ""
			else if (length (L [[j]]) == 1)
				L [[j]] <- as.character (L [[j]]) [1]
			else if (! inherits (L [[j]], "list"))
				L [[j]] <- as.list (as.character (L [[j]]))
			else
				L [[j]] <- listify (L [[j]])
	L
	}

### abbreviates each element of a character vector
### to fit a given width, and adds "..." where 
### text is omitted (left, middle, or right)
abbrev <- function (s, n, where = "right") {
	toolong <- function (s, n) sapply (s, function (x) { nchar (x) > n - 3 }, USE.NAMES = FALSE)
	paste (strtrim (s, width = n - 3), 
		ifelse (toolong (s, n), "...", ""), 
		sep = "")
### ... need abbrev from middle and left ...
	}

### nicely prints a named character vector in two columns,
### one line per element, sensitive to screen dimensions
twoColPrint <- function (v, Labbrev, Rabbrev) {
	w1 <- getOption ("width") / 3
	w2 <- getOption ("width") - w1 - 1
	m <- matrix ( c (names (v), v), ncol = 2)
	m [,1] <- format (abbrev (m [,1], w1, Labbrev), width = w1)
	m [,2] <- format (abbrev (m [,2], w2, Rabbrev), width = w2)
	write.table (m, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE)
	}

### a handy object inspector, sometimes more handy than str()
shew <- function (a) { 
	cat ("class:", class (a), "\n")
	cat ("mode:", mode (a), "\n")
	cat ("typeof:", typeof (a), "\n")
	cat ("length:", length (a), "\n")
	cat ("dim:", dim (a), "\n")
	cat ("names of attributes:", names (attributes (a)), "\n")
	cat ("summary:\n")
	print (summary (a))
	cat ("you could also look at names(), rownames(), colnames(), and dimnames()\n")
	}

### just an abbreviation
shh <- suppressPackageStartupMessages

### this is good for building demos
hold <- function (msg = NULL, cmd = NULL) { 
	message (msg); 
	if (!is.null (cmd)) writeLines (paste (options ("prompt"), cmd, sep=""))
	invisible (readLines (n=1, warn=FALSE))
	}

### open documentation page in a browser
seeDoc <- function (f) {
	outfile <- tempfile (fileext = ".html")
	browseURL (tools::Rd2HTML (f, outfile))
	}

### I forgot why exactly I wanted this
buildHTMLDocs <- function (docs) {
	for (d in docs) tools::Rd2HTML (d, paste ("./html/", unlist (strsplit (d, ".", fixed = TRUE)) [1], ".html", sep=""), Links = tools::findHTMLlinks ())
	}

### split a character vector at all semicolons
chomp <- function (s) {
	if (is.null (s)) return (NULL)
	unlist (strsplit (paste (as.character (s), collapse = ";", sep = ""), ";", fixed = TRUE))
	}

### concatenates to a single string, separating by semicolons
### that is how the API wants to see multiple values of the same parameter
glom <- function (s) { 
	if (is.null (s)) return (NULL)
	paste (as.character (s), collapse = ";", sep = "")
	}

# clean up a vector of ids to standard format for the API,
# adding prefix as necessary ("mgp", etc).  optional argument
# is recycled to specify the resource of each id.
scrubIds <- function (ids, resources = c ("project", "library", "sample", "metagenome")) {
  names <- names (ids)
  ids <- strsplit (paste (ids, collapse = " "), "[^[:alnum:]\\.]+") [[1]]
  if (missing (resources)) resources <- "metagenome"
	resources <- rep (
		c (project = "mgp", library = "mgl", sample = "mgs", metagenome = "mgm")
      [match.arg (resources, several.ok = TRUE)],
		length.out = length (ids))
	scrub <- ifelse (substr (ids, 1, 3) %in% c ("mgp", "mgl", "mgs", "mgm"), ids, paste (resources, ids, sep = ""))
  names (scrub) <- names
  scrub
	}

# identify the kbase resources specified by a vector of ids
# prefixes of "mgp", "mgl", "mgs", "mgm" are understood
# any other prefix (including no prefix) results in "metagenome"
scrapeResources <- function (ids) {
	res <- match (substr (ids, 1, 3), c ("mgp", "mgl", "mgs", "mgm"))
	res [is.na (res)] <- 4
	c ("project", "library", "sample", "metagenome") [res]
	}

### tests first argument for equality with any of others
oneof <- function (x, ...) any (x == unlist ( list (...)))

### same but raises error if not among
oneofmust <- function (x, ...) {
	if ( any (x == unlist ( list (...)))) return (TRUE)
	stop (x, " should be among ", paste (unlist ( list (...)), collapse = " "))
	}

### same but raises error if among
oneofmusnt <- function (x, ...) {
	if ( !any (x == unlist ( list (...)))) return (TRUE)
	stop (x, " should not be among ", paste (unlist ( list (...)), collapse = " "))
	}

### this is useful several times
semiwarn <- function (s) {
	if (length( grep (";", s, fixed = TRUE))) {
		message ("matR: semicolon interpreted as separator")
		TRUE
		} 
	else FALSE
	}

### insist on loading a package
### we load additional packages _after_ matR on the search path
reqPack <- function (P) {
	if ( !library (P, pos = match("package:matR", search ()) + 1, character.only = TRUE, quietly = TRUE, 
		warn.conflicts = FALSE, logical.return = TRUE))
		stop ("matR: package ", P, " required")   ########## revisit this
	else TRUE
	}

### print an optional message according to verbosity configuration
optMessage <- function (s, ...) if (mconfig$verbose ()) message (s, ...)

### helps pluralize output text when appropriate
plur <- function (x) if (length (x) > 1) "s" else ""

### last element of a vector
lastof <- function (x) x [length (x)]

### checks for a obvious graphics type of
### a provided file name
grType <- function (fileName) {
	s <- lastof (strsplit (fileName, ".") [[1]])
	if (oneof (s, "jpeg", "pdf", "png", "ps")) s
	else "png"
	}

### merge two named lists, eliminating duplicates and giving priority to the first
### this is for resolving import / export / graphical parameters
resolveMerge <- function (first, second)
	append (first, second) [ !duplicated (c (names (first), names(second))) ]

### further specialized list-combining function for use in "render" methods
resolveParList <- function (call, object, defaults)
  resolveMerge (call, resolveMerge (object, resolveMerge (defaults, mconfig$par())))

xcall <- function (fun, ..., with = list(), without = character ()) {
	call <- append (append (list (fun), list (...)), with)
	call [without] <- NULL
	if (mconfig$verbose()) message (paste (names (call [-1]), ":", as.character (call [-1]), sep = "", collapse = "  "), "\n")
	eval (as.call (call))
}
