
### "asFile" methods to export matR objects.
### 
### we want to have methods to import and export common objects.
### they should be complementary where appropriate.
###
### there should be a general-purpose list method allowing for either,
### concatenation of elements and a single file, or an equal-length list of filenames.
###
### the operations should be configurable, consulting export parameters (in this order):
###		(1) provided as arguments in the call
###		(2) set in the "exp" slot of the object, if any
###		(3) set in the global configuration "exp"
### 
### The point is to provide standardization and configurability for complex types.
###
### should return name(s) of written file.


setGeneric ("asFile", 
	def = function (x, fname, ...)
		save (x, file = fname))

setMethod ("asFile", "list", function (x, fname, ...)
		for (j in 1:length(x)) asFile (x [[j]], fname [j]))

setMethod ("asFile", "character", function (x, fname, ...)					# for lists of IDs (this may be stupid)
	stop ("matR: unimplemented method for class character"))

setMethod ("asFile", "matrix", function (x, fname, ...) {
	args <- list (...)
	p <- resolveMerge (args, mconfig$exp())
	fname <- paste (p$path, fname, sep = "")
	if (p$type != "binary")
		write.table (x, file = fname, append = p$append, quote = p$quote, sep = p$sep, na = p$na,
								 row.names = p$row.names, col.names = p$col.names)
	else
		save (x, file = fname)
	fname
})

setMethod ("asFile", "Matrix", function (x, fname, ...)
	asFile (as.matrix (x), fname, ...))

setMethod ("asFile", "collection", function (x, view = "count", fname, ...)
	asFile (x [[view]], fname, ...))

setMethod ("asFile", "pco", function (x, fname, ...) {
	args <- list (...)
	p <- resolveMerge (args, mconfig$exp())
	fname <- paste (p$path, fname, sep = "")
	write.table (x [[2]], file = fname, append = TRUE, quote = p$quote, sep = p$sep, na = p$na,
							 row.names = p$row.names, col.names = p$col.names)
	write.table (x [[3]], file = fname, append = TRUE, quote = p$quote, sep = p$sep, na = p$na,
							 row.names = p$row.names, col.names = p$col.names)
	write.table (as.matrix (x [[4]]), file = fname, append = TRUE, quote = p$quote, sep = p$sep, na = p$na,
							 row.names = p$row.names, col.names = p$col.names)
} )


### importing is a sort of separate problem
###
### for now we just have this function to read IDs in various formats:
###
### name ID
### name ID
### name ID
### 
### ID ID ID ID
### 
### ID
### ID
### ID

readIDs <- function (filename, ...) {
	y <- read.table (filename, colClasses = "character", ...)
	if (nrow (y) > 1)
		if (ncol (y) > 1) {
			res <- as.character (y [,2])
			names (res) <- as.character (y [,1])
			res
		}
	else as.character (y [,1])
	else unlist (y [1,], use.names = FALSE)
}


### just an idea:

reconcileTextParametersWithDefaults <- function (...) { }
reconcileGraphicsParametersWithDefaults <- function (...) { }
