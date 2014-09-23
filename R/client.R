
#---------------------------------------------------------------------
#  Post and fulfill data requests
#
#---------------------------------------------------------------------

biomRequest <- function (IDs, request=c("function", "organism"), ..., blocking, quiet=FALSE, wait=TRUE, file=NULL, outfile=NULL) {

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

biom.environment <- function (x, quiet=FALSE, wait=TRUE, ...) {

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

#---------------------------------------------------------------------
# data retrieval: get metadata only
#
# this will be a direct implementation of the "metadata" API call (?)
#---------------------------------------------------------------------

metadata.character <- function (IDs, quiet=FALSE, ...) {
	stop ("metadata retrieval is not implemented yet")
	}


samples.of.project <- function (x) {
	ll <- mGet ("project", scrubSet (x, "project"), verbosity = "full", enClass = FALSE) $ analyzed
	sapply (ll, '[', 1)
	}
