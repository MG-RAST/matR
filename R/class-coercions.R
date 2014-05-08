
##################################################################################################################
### Here we define coercions between "matrix", "biom", "collection", "character", and files.
##################################################################################################################

#####################################################################################
# For coercion I would rather have this syntax but don't yet really understand it.
#
# as.collection <- function (from) as (from, "collection")
# as.biom <- function (from) as (from, "biom")
# as.matrix <- ....?
#####################################################################################

setAs ("character", "biom",
			 function (from) {
			 	reqPack (RJSONIO)
			 	from <- fromJSON (from [1], asText = TRUE, simplify = TRUE)
			 	if (!all (c ("data", "rows", "columns") %in% names (from))) stop ("attempt to coerce non-biom object to biom")
			 	class (from) <- "biom"
			 	from
			 })

setAs ("list", "biom",
			 function (from) {
			 	if (!all (c ("data", "rows", "columns") %in% names (from))) stop ("attempt to coerce non-biom object to biom")
			 	class (from) <- "biom"
			 	from
			 })

#####################################################################################

setAs ("biom", "list", 
			 function (from) { 
			 	class (from) <- "list"
			 	from
			 })
setAs ("biom", "matrix", 
			 function (from) {
			 	m <- matrix (unlist (from$data), ncol = 3, byrow = TRUE)
			 	m <- as.matrix (Matrix::sparseMatrix (i = 1 + m [,1], j = 1 + m [,2], x = m [,3]))
			 	try (rownames (m) <- sapply (from$rows, `[[`, i = "id"))
			 	try (colnames (m) <- sapply (from$columns, `[[`, i = "id"))
			 	m
			 })
setAs ("biom", "collection",
### !!! THIS SHOULD CHANGE SOONER OR LATER, TO PRESERVE METADATA FROM THE BIOM OBJECT
			 function (from) as (as (from, "matrix"), "collection"))

#####################################################################################

# setAs ("matrix", "biom",
# 			 def = function (from) {
# ### THIS IS GOING TO TAKE SOME TIME; MUST cf. BIOME FORMAT...
# 			 	})
setAs ("matrix", "collection",
			 def = 
			 	function (from) {
			 		if (is.null (colnames (from))) {
			 			warning ("samples are unidentified due to missing colnames")
			 			colnames (from) <- 1:ncol(from)
			 		}
			 		warning ("collection will have no metadata")
			 		warning ("view of collection will be unidentified")
			 		dummy.metadata <- character()
			 		class (dummy.metadata) <- "metadata"
			 		new ("collection",
			 				 views = list (x = function (want.dummy = FALSE) from),
### !!! AM I POSSIBLY REORDERING THE COLUMNS INCORRECTLY, HERE?
			 				 sel = new ("selection", ids = colnames (from), groups = factor(), metadata = dummy.metadata, 
			 				 					 ids.spec = character(), resource.spec = character(), metadata.extent = "none"))
			 	})

#####################################################################################

setAs ("collection", "matrix",
			 def = function (from)
			 	from [[length (views (from)), plain = TRUE]])
# setAs ("collection", "biom",
# 			 def = function (from) { 
# ### THIS IS GOING TO TAKE SOME TIME; MUST cf. BIOME FORMAT...
# 			 	})



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

### !!! NOT THE RIGHT WAY TO ESTABLISH A DEFAULT METHOD
# asFile <- function (x, file, ...) save (x, file = file)

# setMethod ("asFile", "list", function (x, file, ...)
# 	for (j in 1:length(x)) asFile (x [[j]], file [j]))

setMethod ("asFile", "character", 
					 function (x, file, ...) {
					 	write.table (data.frame (x), file = file, sep = "\t", quote = FALSE, 
					 							 row.names = if (!is.null (names (x))) TRUE else FALSE, col.names = FALSE)
					 	file
					 })

setMethod ("asFile", "matrix", 
					 function (x, file, ...) {
# 	args <- list (...)
# 	p <- resolveMerge (args, msession$exp())
# 	file <- paste (p$path, file, sep = "")
# 	if (p$type != "binary")
# 	  ...
# 	else
# 		save (x, file = file)
					 	write.table (x, file = file, quote = FALSE, sep = "\t",
					 							 row.names = TRUE, col.names = TRUE)
					 	file
					 })


# I REALLY WANT THIS TO WORK, BELOW !
# name <- as.character (substitute (x, parent.frame (6)))
# assign (name, x)
# save (list = name, file = file)
#
# f <- function (x, view = length (views (x)), file, ...) {
# 	name <- as.character (substitute (x))
# 	print(name)
# }
	
setMethod ("asFile", "collection", 
					 function (x, view = length (views (x)), file, ...) {
					 	if (is.null (view)) save (x, file = file)
					 	else asFile (x [[view, plain = TRUE]], file, ...)
					 	file
					 	})

setMethod ("asFile", "pco", 
					 function (x, file, ...) {
# MORE CLEVER TO CHECK IF .Rda IS PRESENT
					 	suff <- ".Rda"
					 	file.values <- paste (file, ".values", suff, sep = "")
					 	file.vectors <- paste (file, ".vectors", suff, sep = "")
					 	write.table (data.frame (x$values), file = file.values, sep = "\t", quote = FALSE, 
					 							 row.names = TRUE, col.names = FALSE)
					 	write.table (x$vectors, file = file.vectors, sep = "\t", quote = FALSE,
					 							 row.names = TRUE, col.names = TRUE)
					 	c (file.values, file.vectors)
					 })

###
### importing is a sort of separate problem
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
###

# Kevin update: 5-8-14
# first column has ids, second names for them, third plus are ignored
readIDs <- function (filename, ...) {
  y <- read.table (filename, colClasses = "character", sep="\t", ...)
  if (nrow (y) > 1) {
    if (ncol (y) > 1) {
      if (ncol (y) > 1) { warning("Your list has more than two columns, only the first two are used") }
      res <- as.character (y [,1])
      names (res) <- as.character (y [,2])
      res <- res[ order(res) ]
      res
    } else {
      res <- as.character (y [,1])
      res <- res[ order(res) ]
      res
    }
  } else {
    print("There was just one id in your list?")
    res <- unlist (y [1,], use.names = FALSE)
    res
  }
}

