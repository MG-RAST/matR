
#################################################
### "asFile" is a generic function for exporting
### matR objects, the natural opposite of constructing
### an object of the same type from a filename or connection.
###
### asFile(x, fname, ...) : creates fname containing objects
### represented according to the current configuration of export parameters.
###
### when x is a list, fname should be a vector of the same length,
### and each item will be written to a separate file
###
### To decide how to write an object, asFile consults (in this order) 
### export parameters that are:
###		(1) provided as arguments in the call
###		(2) set in the "exp" slot of the object, if any
###		(3) set in the global matR option, "exp"
### 
### The point is to provide standardization with 
### customizability for writing possibly complex
### types as text and binary files.
###
### returns name(s) of written file
###
### complementary import functionality should be provided by
### functions accepting a parameter of class "connection"
###
### asFile (<file>) to rewrite in different formats?  well, that is cute...
#################################################

setGeneric ("asFile", 
	def = function (x, fname, ...)
		save (x, file = fname))

setMethod ("asFile",
	"list",
	function (x, fname, ...)
		for (j in 1:length(x)) asFile (x [[j]], fname [j]))

setMethod ("asFile",					# for lists of IDs (this may be stupid)
	"character",
	function (x, fname, ...)
		stop ("matR: unimplemented method for class character"))

setMethod ("asFile",
	"rlist",
	function (x, fname, ...)
		stop ("matR: unimplemented method for class rlist"))

setMethod ("asFile",
	"matrix",
	function (x, fname, ...) {
		args <- list (...)
		p <- resolveMerge (args, mconfig$exp())
		fname <- paste (p$path, fname, sep = "")
		if (p$type != "binary")
			write.table (x,
				file = fname,
				append = p$append,
				quote = p$quote,
				sep = p$sep,
				na = p$na,
				row.names = p$row.names,
				col.names = p$col.names)
		else
			save (x, file = fname)
		fname
		})

setMethod ("asFile",
	"Matrix",
	function (x, fname, ...)
		asFile (as.matrix (x), fname, ...))

setMethod ("asFile", 
	"mmatrix",
	function (x, fname, ...)
		asFile (as.matrix (x), fname, ...))

#setMethod ("asFile", 
#	"RBIOM",
#	function (x, fname, ...) { } )
#setMethod ("asFile", 
#	"pcaRes",
#	function (x, fname, ...) { } )
### writing PCA to file:  need to concatenate two tables into single object.  snippet from original:
###	if (! is.null (asFile)) {
###		write.table (P@R2,	file = asFile, sep = "\t", col.names = FALSE, row.names = TRUE, append = FALSE)
###		write.table (P (my_pcaRes), file = asFile, sep = "\t", col.names = FALSE, row.names = TRUE, append = TRUE)
#		}
#setMethod ("asFile", 
#	"pca",
#	function (x, fname, ...) { } )
#setMethod ("asFile", 
#	"dendrogram",
#	function (x, fname, ...) { } )	
#setMethod ("asFile",
#	"heatmap",
#	function (x, fname, ...) { } )

# just an idea:
reconcileTextParametersWithDefaults <- function (...) { }
reconcileGraphicsParametersWithDefaults <- function (...) { }
