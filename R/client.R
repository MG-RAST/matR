
#---------------------------------------------------------------------
#  Post and fulfill data requests
#
#---------------------------------------------------------------------

biomRequest <- function (IDs, request=c("function", "organism"), ..., blocking, wait=TRUE, quiet=FALSE, file=NULL, outfile=NULL) {

	request <- match.arg(request)

#-------read IDs in from file, if provided

	if (!is.null(file))
		IDs <- readSet(file)


#-------separate IDs from any included metadata

	if (is.data.frame (IDs)) {
		add.metadata <- IDs
		IDs <- rownames(IDs)
	} else add.metadata <- NULL

#-------clean up IDs

	IDs <- scrubSet(IDs)
	if (!all(scrapeSet(IDs) == "metagenome"))
		stop("only data retrieval by metagenome is currently implemented")

#-------set up requested blocking size

	if (missing (blocking))
		blocking <- length(IDs)

#-------set up the download scheme, and an environment to hold it

	req <- new.env (parent = globalenv())
	param <- append (list (resource='matrix', request=request, asynchronous=1), list(...))

	ledger <- data.frame (start = seq(1, length(IDs), blocking), stringsAsFactors = F)
	ledger$stop 		<- c (ledger$start[-1] - 1, length(IDs))
	ledger$requested 	<- FALSE
	ledger$ticket		<- ""
	ledger$file			<- ""

	ledger[1,"ticket"] <- do.call (call.MGRAST, append (param, list (id=IDs [ledger[1,"start"] : ledger[1,"stop"]]))) ["id"]
	ledger[1,"requested"] <- TRUE

	assign("IDs", IDs, req)
	assign("n", 1, req)
	assign("ledger", ledger, req)
	assign("param", param, req)
 	assign("add.metadata", add.metadata, req)
 	assign("outfile", outfile, req)

	if (!wait) {
		message("returning ticket for queued request; apply biom() to fulfill")
		invisible(req)
	} else biom(req)
	}


#---------------------------------------------------------------------
# data retrieval: function to fulfill requests
#
# "..." in prototype is just good practice for generics
# !quiet=TRUE is used to report on the download in complete detail
#---------------------------------------------------------------------

biom.environment <- function (x, wait=TRUE, ..., quiet=FALSE) {

	assign("quiet", quiet, x)
	assign("wait", wait, x)
	with (x, {
		repeat {
			yy <- call.MGRAST("status", "instance", id=ledger[n, "ticket"])
			if("data" %in% names(yy)) {
				message (ledger[n,"start"], " to ", ledger[n,"stop"], " received")
				tt <- tempfile()
				yy <- biom(yy$data)
				save (yy, file=tt)
				ledger[n, "file"] <- tt
				if (n == nrow(ledger)) break
				n <- n+1
				ledger[n,"ticket"] <- do.call (call.MGRAST, append (param, list (id=IDs [ledger[n,"start"] : ledger[n,"stop"]]))) ["id"]
				ledger[n,"requested"] <- TRUE
				}
			if(!wait) {
				warning("data retrieval with blocking will not complete with wait=FALSE")
				if (!quiet) print(ledger)
				stop("request cannot be fulfilled")
				}
			Sys.sleep(5)
			}

		if (!quiet) {
			message("assembling from:")
			print(ledger)
			}
		ll <- lapply(
				ledger[,"file"], 
				function (ff) { 
					load(ff); unlink(ff); yy
					})
		yy <- Reduce (merge.biom, ll)

#		if(!is.null("add.metadata")) columns(yy) <- add.metadata
		if(!is.null(outfile)) writeLines(as.character(yy), file=outfile)
		invisible(yy)
		})
	}


metadata.character <- function (x, detail=NULL, ..., quiet=TRUE, file=NULL) {
#-----------------------------------------------------------------------------------------
# get metadata without data.
#
# detail = NULL			for projects, returns metagnomes		(list, because one-to-many)
#						for metagenomes, returns projects		(named vector, because one-to-one)
#
# detail = TRUE			equivalent to "verbosity=minimal"		(data.frame, for both samples and projects)
#
# detail = c("minimal","verbose","full")				for projects		(data.frame)
# 		 = c("minimal","metadata","stats","full")		for metagenomes		(data.frame)
# relayed directly as "verbosity" to call.MGRAST()
#
# suppressWarnings() is necessary here since API doesn't return everything it says it will
#-----------------------------------------------------------------------------------------

	if (!is.null (file)) x <- readSet(file)
	x <- scrubSet(x)
	y <- scrapeSet(x) [1]

	if (is.null(detail) && y=="project") {
		f <- function (x) simplify2array (suppressWarnings (call.MGRAST (					# keep IDs, drop URLs
			"project", "instance", id=x, verbosity='full', quiet=quiet)) $ metagenomes,
			higher=FALSE) [1,]
		sapply (x, f, simplify=FALSE)											# sapply gives names to the result

	} else if (is.null(detail) && y=="metagenome") {
		f <- function (x) suppressWarnings (call.MGRAST (
			"metagenome", "instance", id=x, verbosity='min', quiet=quiet)) $ project [1]
		sapply (x, f)

	} else {
		if (isTRUE (detail)) detail <- "minimal"
		f <- function (x) suppressWarnings (call.MGRAST (
			y, "instance", id=x, verbosity=detail, ..., quiet=quiet))
		z <- list2df (sapply (x, f, simplify=FALSE))
		z$id <- z$url <- NULL											# get rid of some annoying things
		z$library2 <- z$project2 <- z$sample2 <- NULL					# same, for metagenome metadata only
		z
		}
	}


dir.MGRAST <- function (from, to, length.out=0, ..., quiet=TRUE) {
#-----------------------------------------------------------------------------------------
#  here we translate just a bit:
#  arg names "from", "to", "length.out" are familiar to R people,
#  as are indices starting at 1.
#
#  --> allow "from" and "to" to contain names; retrieve in chunks & provide assembly)
#-----------------------------------------------------------------------------------------

	if (missing (length.out))
		length.out <- to - from + 1
	else if (!missing (from))
		to <- from + length.out - 1
	else if (!missing (to))
		from <- to - length.out + 1

	args <- resolve (list (...), list(
		resource = "project",
		request = "query",
		verbosity = "minimal",
		order = "name",
		limit = length.out,
		offset = from - 1))
	y <- list2df (do.call (call.MGRAST, args) $ data)

#-----------------------------------------------------------------------------------------
#  make a data.frame.
#  content will vary according to "verbosity".
#  remove certain junk fields, and turn "status" (public/private) into a factor.
#  rownames of the data.frame will always be "mgpXX"
#-----------------------------------------------------------------------------------------
	rownames(y) <- y$id
	y$id <- y$created <- y$url <- y$version <- NULL
	y$status <- as.factor (y$status)
	y
	}


#-----------------------------------------------------------------------------------------
# search() has some defaults and allows opts
#
# --> would want no limit
#-----------------------------------------------------------------------------------------

search.MGRAST <- function (..., quiet=FALSE) {
#public.only=FALSE   [status],  
#any=FALSE			[match]
#verbosity=NA  ? .. no

# 	arg <- resolve (list (...), 
# 		resource = "metagenome",
# 		request = "query",
# 		verbosity = "minimal",
# 		status = "both",
# 		match = "all",
# 		offset = 0,
# 		limit = 50)
# 	y <- do.call (call.MGRAST, args)
# 	if (length (y) ...) 
# 		message ("limit reached; more results may be available with... follow on call")
# #  here must cleanup received list structure
# 	y
 	}
