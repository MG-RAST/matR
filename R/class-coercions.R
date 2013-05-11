
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

from.biom <- function (x) {
	
}


# this function should return a list of components in maximally simple data structures
setAs ("biom", "list", 
			 def = function (from) {
			 	reqPack ("RJSONIO")
			 	bb <- fromJSON (from, asText = TRUE, simplify = TRUE)
			 	
			 	# bulletproof this - do a better check
# 			 	if (length (setdiff (c ("data", "rows", "columns"), names (y))) != 0) warning ("bad format in received resource")
# 			 	
# 			 	m <- matrix (unlist (bb$data), ncol = 3, byrow = TRUE)
# 			 	m <- as.matrix (Matrix::sparseMatrix (i = 1 + m [,1], j = 1 + m [,2], x = m [,3]))
# 			 	
# 			 	# bulletproof this
# 			 	rownames (m) <- sapply (y$rows, `[[`, i = "id")
# 			 	colnames (m) <- sapply (y$columns, `[[`, i = "id")
# 			 	
# 			 	# need to confirm this understanding of the format - can't rely on what was asked for; have to look for what is there
# 			 	s <- switch (name, `function` = "ontology", organism = "taxonomy", NULL)
# 			 	
# 			 	rh <- try(lapply(y$rows, function(x) unlist(x[[c("metadata", s)]])))
# 			 	if (inherits(rh, "try-error")) warning ("annotation hierarchy unavailable")
# 			 	hlen <- max(sapply(rh, length))
# 			 	attr(m, "rowhier") <- sapply(rh, `length<-`, hlen)
# 			 	if (hlen != 1) attr(m, "rowhier") <- t (attr(m, "rowhier"))
			 	
			 	# return list corresponding to biom format specification
#			 	list ( = , = , = ,)
			 })


setAs ("character", "collection",
			 def = function (from) collection (from))
setAs ("matrix", "collection",
			 def = 
			 	function (from) {
			 		if (is.null (colnames (from))) {
			 			warning ("samples are unidentified due to missing colnames")
			 			colnames (from) <- 1:ncol(from)
			 		}
			 		warning ("collection will have no metadata")
			 		warning ("view of collection will be unidentified")
			 		dummy.metadata <- character(0)
			 		class (dummy.metadata) <- "metadata"
			 		new ("collection",
			 				 views = list (data = from),
			 				 sel = new ("selection", ids = colnames (from), groups = factor(), metadata = dummy.metadata, 
			 				 					 ids.spec = character(0), resource.spec = character(0), metadata.extent = "none"))
			 	})
setAs ("biom", "collection",
			 def = function (from) {
			 	bb <- from.biom (from)
			 	new ("collection",
			 			 view = list (data = bb$data),
			 			 sel = new ("selection",
			 			 					 ids = colnames (from), 
			 			 					 groups = factor(0), 
			 			 					 metadata = character(0), 
			 			 					 id.spec = character(0), 
			 			 					 resource.spec = character(0), 
			 			 					 metadata.extent = "none"))
			 })

setAs ("collection", "matrix",
			 def = function (from)
			 	from [[length (views (from)), plain = TRUE]])
setAs ("biom", "matrix",
			 def = function (from) {
			 	from.biom (from) $ data
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
