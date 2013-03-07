
##################################################################################################################
### Here we define coercions between "matrix", "biom", "collection", "character", and files.
##################################################################################################################

##################################################################################################################
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
##################################################################################################################


setAs ("character", "collection",
			 def = function (from) collection (from))
setAs ("matrix", "collection",
			 def = 
			 	function (from) {
			 		attr (from, "...") <- ...
			 		cc <- new ("collection")
			 		cc@views <- list (view = from)
			 		cc@sel <- ...
			 	})
setAs ("biom", "collection",
			 def = function (from) {
			 	reqPack ("RJSONIO")
			 	s <- fromJSON (from, asText = TRUE, simplify = TRUE)
			 	m <- ...     # copy from mGet; then call this from there
			 	cc <- as.collection (m)
			 	metadata (cc) <- ...
			 	cc
			 },
			 replace = function (from, value) { })

setAs ("collection", "matrix",
			 def = function (from)
			 	from [[length (views (from)), plain = TRUE]])
setAs ("biom", "matrix",
			 def = function (from) {
			 	reqPack ("RJSONIO")
			 	s <- fromJSON (from, asText = TRUE, simplify = TRUE)
			 	m <- ...     # adapt from mGet
			 	m
			 })

setAs ("list", "biom",
			 def = function (from) { },
			 replace = function (from, value) { })
setAs ("character", "biom",
			 def = function (from) { },
			 replace = function (from, value) { })
setAs ("matrix", "biom",
			 def = function (from) { },
			 replace = function (from, value) { })
setAs ("collection", "biom",
			 def = function (from) { },
			 replace = function (from, value) { })

# as.collection <- function (from) as (from, "collection")
# as.biom <- function (from) as (from, "biom")
# as.matrix <- ....?

asFile <- function (x, file, ...) save (x, file = file)

setMethod ("asFile", "list", function (x, file, ...)
		for (j in 1:length(x)) asFile (x [[j]], file [j]))

setMethod ("asFile", "character", function (x, file, ...)					# for lists of IDs (this may be stupid)
	stop ("matR: unimplemented method for class character"))

setMethod ("asFile", "matrix", function (x, file, ...) {
	args <- list (...)
	p <- resolveMerge (args, msession$exp())
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

setMethod ("asFile", "collection", function (x, view = length (views (x)), file, ...)
	asFile (x [[view]], file, ...))

#setMethod ("asFile", "pco", function (x, file, ...) {
# 	args <- list (...)
# 	p <- resolveMerge (args, msession$exp())
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
