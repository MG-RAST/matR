
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

asFile <- function (x, file, ...) save (x, file = file)

setGeneric ("asFile", function (x, file, ...) standardGeneric ("asFile"))		

setMethod ("asFile", "list", function (x, file, ...)
		for (j in 1:length(x)) asFile (x [[j]], file [j]))

setMethod ("asFile", "character", function (x, file, ...)					# for lists of IDs (this may be stupid)
	stop ("matR: unimplemented method for class character"))

setMethod ("asFile", "matrix", function (x, file, ...) {
	args <- list (...)
	p <- resolveMerge (args, mconfig$exp())
	file <- paste (p$path, file, sep = "")
	if (p$type != "binary")
		write.table (x, file = file, append = p$append, quote = p$quote, sep = p$sep, na = p$na,
								 row.names = p$row.names, col.names = p$col.names)
	else
		save (x, file = file)
	file
})

setMethod ("asFile", "Matrix", function (x, file, ...)
	asFile (as.matrix (x), file, ...))

setMethod ("asFile", "collection", function (x, view = "count", file, ...)
	asFile (x [[view]], file, ...))

#setMethod ("asFile", "pco", function (x, file, ...) {
# 	args <- list (...)
# 	p <- resolveMerge (args, mconfig$exp())
# 	file <- paste (p$path, file, sep = "")
# 	write.table (x [[2]], file = file, append = TRUE, quote = p$quote, sep = p$sep, na = p$na,
# 							 row.names = p$row.names, col.names = p$col.names)
# 	write.table (x [[3]], file = file, append = TRUE, quote = p$quote, sep = p$sep, na = p$na,
# 							 row.names = p$row.names, col.names = p$col.names)
# 	write.table (as.matrix (x [[4]]), file = file, append = TRUE, quote = p$quote, sep = p$sep, na = p$na,
# 							 row.names = p$row.names, col.names = p$col.names)
# } )

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

readIds <- function (file, ...) {
	y <- read.table (file, colClasses = "character", ...)
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
