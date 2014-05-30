
#---------------------------------------------------------------------
#  data retrieval
#---------------------------------------------------------------------

biomRequest <- function (IDs, request="function", ..., blocksize=5, file=NULL) {
	if(!is.null(file)) {
		df <- read.table(x, header=F)
		if(ncol(df) > 1) {
			metadata <- df[-1,-1,drop=FALSE]
			rownames(metadata) <- df[-1,1,drop=FALSE]
			names(metadata) <- df[1,-1,drop=FALSE]
			IDs <- rownames(metadata)
		} else {
			IDs <- as.character(df[,1])
			metadata <- NULL
		}
	} else {
		IDs <- scrubsIDs(x)
		netadata <- NULL
	}


	blocks <- data.frame(block = 1 + (1:length(IDs)) %/% blocksize, 
		requestID=rep(character(0),length(IDs)),
		filename=rep(character(0),length(IDs)),
		stringsAsFactors=TRUE)
	rownames(blocks) <- IDs
	blocks[blocks$block == 1,"requestID"] <- call.MGRAST(resource="matrix", request=request, id=<first block>, asynchronous=1, ...)$id

	ee <- new.env(parent=baseenv())
	assign("blocks", blocks, envir=ee)
	assign("blocksize", blocksize, envir=ee)
	assign("metadata", metadata, envir=ee)
	assign("current", 1, envir=ee)
	message("returning ticket for queued request; use biom(...) to complete")
	return(ee)
	}

biom.environment <- function (x, ..., wait=TRUE) {
		repeat {
		xx <- call.MGRAST(resource="status", ...)
		if(<pending request is retrievable>) {
			<mssg & save it to a new tempfile>
			if(<all blocks are received>) {
				<assemble, add metadata, and return>
			} else {
				<request next block>
				}
			}
		if(!wait) stop("data is not complete: ", ..., "out of", ..., "metagenomes received")
		Sys.sleep(5)
		}
	}

metadata <- function (x, ...)
