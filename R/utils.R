
#---------------------------------------------------------------
#  package name, requires preprocessing source with:
#	 sed s/XXXBUILDXXX/$commit/g matR/R/init.R > init.Rtemp
#	 mv init.Rtemp matR/R/init.R
#
#---------------------------------------------------------------

tagline <- function () {
	ss <- " XXXBUILDXXX"
	if (substr (ss, 2, 9) == "XXXBUILD") ss <- ""

	paste0 ("matR: metagenomics analysis tools for R (", packageVersion("matR"), ss, ")")
	}

#---------------------------------------------------------------
#  stylish warnings
#
#---------------------------------------------------------------

warning <- function (...) {
	base::warning ("matR: ", ...,  call.=FALSE)
	}

#---------------------------------------------------------------
# miscellaneous little things for nicer expression
#
#---------------------------------------------------------------

collapse <- function (x, ..., sep = " ") {
	paste(x, ..., , sep = sep, collapse = sep)
	}

#---------------------------------------------------------------------------------
#  readSet() -- utility to read IDs, possibly with metadata, from a file in matR standard format.  returns data.frame or character vector
#  scrubSet() -- clean up a specification of "ids"
#  scrapSet() -- return vector of "resources"
#  sampleSets() -- return file names of ids, some with metadata
#  buildSets() -- create the .rda included with the matR package.
#
#  Set 1		7 metagenomes (in three groups)
#  Set 2		24 metagenomes (in two groups)
#  Set 3		32 metagenomes (in three groups)
#  Set 4		16 metagenomes (in three groups)
#  Set 5		1606 metagenomes (HMP)
#  Set 6		4 projects of various sizes
#  Set 7		10 metagenomes and 3 projects
#
#---------------------------------------------------------------------------------

readSet <- function (file) {
	df <- read.table(file, header=F, sep="\t", colClasses="character", stringsAsFactors=T)
	if(ncol(df) > 1) {
		colnames(df) <- df[1, , drop=T]
		rownames(df) <- df[,1]
		df[-1, -1, drop=F]
	} else df[, , drop=T]
	}

scrubSet <- function (IDs, resources = "metagenome") {
	IDs <- unlist (sapply (as.list (IDs), strsplit, "[^[:alnum:]\\.]+"))
	names <- names (IDs)
	prefixes <- rep (resources, length.out = length (IDs))
	prefixes <- match.arg (prefixes, c ("metagenome", "project"), TRUE)
	scrubbed <- paste (ifelse (substr (IDs, 1, 3) %in% c("mgm", "mgp"), "",
														 c (metagenome = "mgm", project = "mgp") [prefixes]), 
										 IDs, sep = "")
	names (scrubbed) <- names (IDs)
	scrubbed
	}

scrapeSet <- function (ids) {
	res <- match (substr (ids, 1, 3), c ("mgm", "mgp"))
	res [is.na (res)] <- 1
	c ("metagenome", "project") [res]
	}

sampleSets <- function () {
	paste (file.path (path.package ("matR"), "extdata", "set-"), 1:7, ".tsv", sep="")
	}


buildSets <- function (file="sample-sets.rda") {
	ee <- new.env()
	ff <- sampleSets()

	li <- lapply (mget (paste0 ("li", 1:4), inherits=TRUE), biom)
	mapply (assign, paste0 ("yy", 1:length(li)), li, MoreArgs = list(ee))

	li <- list()
	li[[1]] <- biomRequest (file = ff[1], request="function", group_level="level2")
	li[[2]] <- biomRequest (file = ff[2], request="organism", group_level="phylum")
	li[[3]] <- biomRequest (file = ff[3], request="function", group_level="level1", evalue=1)
	li[[4]] <- biomRequest (file = ff[4], request="organism", group_level="domain", source="Greengenes")
#		li[[5]] <- biomRequest (file = ff[5], request="organism", group_level="domain")
	li[[5]] <- biom(list())
#		li[[6]] <- biomRequest (file = ff[6], "organism")
	li[[6]] <- biom(list())
#		li[[7]] <- biomRequest (file = ff[7], "organism")
	li[[7]] <- biom(list())
	mapply (assign, paste0 ("xx", 1:length(li)), li, MoreArgs = list(ee))


	save (list=ls(ee), file=file, envir=ee)
	message (
		"Created file \"", file, "\" in ", getwd(),
		".\nFor package build move to \"matR/data\"")
	file
	}

#----------------------------------------------------
#  utilities to handle "suggested dependencies".
#  official dependencies are minimized to remove obstacles to installation.
#
#----------------------------------------------------

hazPackages <- function() {
	me <- packageDescription ("matR")
	need <- unlist (strsplit (c (me$Imports, me$Suggests), "[^[:alnum:]\\.]+"))
	sapply (need, function (x) length (find.package (x, quiet = TRUE)) > 0)
	}

dependencies <- function (prompt = TRUE) {
	need <- !hazPackages()
	if (any (need)) {
		message("matR uses other software.. blah blah blah.. here is what we\'ll do")
		message("Suggested package(s) missing: ", collapse (names(need) [need]), "\n")
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
		if (all (haz)) {
			message("\nAll suggested packages have been installed.\nNow quit and restart R.")
		} else message("\nPackage(s) could not be installed: ", collapse (names(haz) [!haz]))
	} else 
		message ("All suggested packages appear to be installed.")
	}

#---------------------------------------------------------------------------------
# 'stepping' functions to help with demo'ing.
#
#  read the file as text and echoes each line exactly.
#  each command must fit on a line, and blank lines are simply echoed.
#---------------------------------------------------------------------------------

stepper <- function (file) {
	lines <- readLines (file)
	for (j in 1:length (lines))
		if (lines [j] != "") {
			cat (getOption ("prompt"), lines [j], sep = "")
			readLines (n = 1, warn = FALSE)
# NOTE: probably change eval() to occur in parent environment
			R <- withVisible (eval.parent (parse (text = lines [j])))
			if (R$visible && !is.null (R$value)) print (R$value)
			}
		else cat ("\n")
	}

#---------------------------------------------------------------------------------
#  parses the whole file first.  commands may span lines.
#  comments are not displayed.  commands are reformatted to standard appearance.
#---------------------------------------------------------------------------------

stepper2 <- function (file) {
	exprs <- parse (file = file)
	for (j in 1:length (exprs)) {
		cat (getOption ("prompt"), as.character (exprs [j]), sep = "")
		readLines (n = 1, warn = FALSE)
		eval (exprs [j])
		}
	}

demo.step <- function (s) {
	stepper (file.path (path.package ("matR"), "demo", paste0 (s, ".R")))
	}

#---------------------------------------------------------------------------------
# abbreviates each element of a character vector to fit a given width
#  adds "..." where text is omitted (left, middle, or right)
#---------------------------------------------------------------------------------

abbrev <- function (s, n, where = "right") {
	toolong <- function (s, n) sapply (s, function (x) { nchar (x) > n - 3 }, USE.NAMES = FALSE)
	paste (strtrim (s, width = n - 3), 
		ifelse (toolong (s, n), "...", ""), 
		sep = "")
### ... need abbrev from middle and left ...
	}

#---------------------------------------------------------------------------------
#  utilities for docs
#---------------------------------------------------------------------------------

seeDoc <- function (f) {
	outfile <- tempfile (fileext = ".html")
	browseURL (tools::Rd2HTML (f, outfile))
	}

buildHTMLDocs <- function (docs) {
	for (d in docs) tools::Rd2HTML (d, paste ("./html/", unlist (strsplit (d, ".", fixed = TRUE)) [1], ".html", sep=""), Links = tools::findHTMLlinks ())
	}


