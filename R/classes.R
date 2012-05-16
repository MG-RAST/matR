
############################################
### CLASS AND METHOD DEFINITIONS
### FOR USER-FACING (and hidden) OBJECTS
############################################

setClass ("mProjectMeta", representation (ID = "character"), contains = "list")
setClass ("mSampleMeta", representation (ID = "character"), contains = "list")
setClass ("mMetagenomeMeta", representation (ID = "character"), contains = "list")

setClass ("mMatrix", representation (data = "Matrix", meta = "list"), prototype (data = Matrix(0), meta = list("")))
	
setMethod ("initialize", "mMatrix",
	function(.Object, d, m) {
		.Object@data <- Matrix::Matrix (d)
		.Object@meta <- m
		.Object
	})


############################################
### simple stuff to print important 
### metadata cleanly, and to gracefully 
### evolve in the long-term with changes
### to the metadata hierarchy
###
### the right way is not to print but to 
### return a printable object
### 
### and of course do not assume people are
### using fixed width fonts
############################################


abbrev <- function (s, n) {
	toolong <- function (s, n) sapply (s, function (x) { nchar (x) > n - 3 }, USE.NAMES = FALSE)
	paste (strtrim (s, width = n - 3), 
		ifelse (toolong (s, n), "...", ""), 
		sep = "")
	}


print.mProjectMeta <- function (x, ...) {
	w1 <- getOption ("width") / 3
	w2 <- getOption ("width") - w1

	nam <- abbrev (names (x$metadata), w1)
	val <- abbrev (x$metadata, w2)
	pri <- matrix (c (format (nam, width = w1), format (val, width = w2)), ncol = 2)
	write.table (pri, quote = FALSE, row.names = FALSE, col.names = FALSE)
	cat ("samples: ", paste (x$kbase, collapse = " "), "\n")
	cat ("metagenomes: ", paste (x$analyzed, collapse = " "), "\n")
	}

summary.mProjectMeta <- function (object, ...) {
	cat ("< metadata of project", object@ID, ">\n")
	}

print.mSampleMeta <- function (x, ...) {
	}

summary.mSampleMeta <- function (object, ...) {
	cat("< metadata of sample", object@ID, ">\n")
	}

print.mMetagenomeMeta <- function (x, ...) {
	}

summary.mMetagenomeMeta <- function (object, ...) {
	cat ("< metadata of metagenome", object@ID, ">\n")
	}

print.mMatrix <- function (x, ...) {
		m <- min (nrow (x@data), 15)
		n <- min (ncol (x@data), 5)
		rownames (x@data) <- abbrev( rownames (x@data), getOption ("width") / 3)
		print (x@data [1:m, 1:n])
		cat ("<truncated>\n")
	}

summary.mMatrix <-	function (object, ...) {
		cat ("< abundance matrix with", dim (object@data) [1], "rows (taxa) and", dim (object@data) [2], "columns (samples) >\n")
		cat ("< metagenomes:", paste (colnames (object@data), collapse = " "), ">\n")
	}


