
#################################################
### "toFile" is a generic function for exporting
### matR objects, the natural opposite of constructing
### an object of the same type from a filename or connection.
###
### toFile(x, ...) : creates file with matR-standard representations of objects (maybe "as.file" or does that mislead?)
### accepts parameters matR defines: "fileType", "inPath", "outPath"
### accepts usual parameters R defines for text I/O: "sep", "quote", "fill", etc.
### looks at default text I/O parameters set in mConfig
###
### To decide how to write an object, a particular
### call uses
###   (1) global matR options,
###   (2) properties of the object,
###   (3) its actual arguments
### with the latter always taking precedence.
### 
### The point is to provide standardization with 
### customizability for writing possibly complex
### types as text.
###
### returns name of written file
###
### reciprocal functionality fromFile() should be implemented directly
### and also in functions that accept a parameter of class "connection"
### ... does fromFile _autodetect_ what it reads, or is it told what
### ... kind to read?
###
#################################################

setGeneric ("toFile", 
	def = function (x, ...) { standardGeneric ("toFile") },
	useAsDefault = FALSE)
setMethod ("toFile",			# for lists of IDs (this may be stupid)
	"character",				# toFile (<file>) to rewrite in different formats??
	function (x, ...) { } )
setMethod ("toFile",
	"rlist",
	function (x, ...) { } )
setMethod ("toFile",
	"matrix",
	function (x, ...) { } )
setMethod ("toFile",
	"Matrix",
	function (x, ...) { } )
setMethod ("toFile", 
	"mmatrix",
	function (x, ...) { } )
#setMethod ("toFile", 
#	"RBIOM",
#	function (x, ...) { } )
setMethod ("toFile", 
	"pcaRes",
	function (x, ...) { } )


# writing PCA to file
# need to concatenate two tables into single object
#	if (! is.null (toFile)) {
#		write.table (P@R2,	file = toFile, sep = "\t", col.names = FALSE, row.names = TRUE, append = FALSE)
#		write.table (P (my_pcaRes), file = toFile, sep = "\t", col.names = FALSE, row.names = TRUE, append = TRUE)
#		}


#setMethod ("toFile", 
#	"mPCoA",
#	function (x, ...) { } )
#setMethod ("toFile", 
#	"dendrogram",
#	function (x, ...) { } )	
#setMethod ("toFile",
#	"mHeatmap",
#	function (x, ...) { } )

reconcileTextParametersWithDefaults <- function (...) { }
reconcileGraphicsParametersWithDefaults <- function (...) { }



############################################
### These functions "wrap the file system" around
### matrix (and other?) objects.  The idea is, in a 
### standard way, to minimize the difference between 
### working with different representations of the
### same object, i.e., in memory and on disk
### (and maybe, on the network).
### This idea is perhaps to be implemented by
### class methods, and for various classes other
### than matrix.
############################################

fsUnwrap <- function (x) {
	if (class (x) == "character") data.matrix (read.table (x, row.names= 1 , header = TRUE, sep = "\t", comment.char = "", quote = ""))
	else as.matrix (x)
	}

fsWrap <- function (x, toFile = NULL) {
	if (! is.null (toFile)) {
		if (class (x) == "matrix") write.table (x, file = toFile, sep = "\t", col.names = NA, row.names = TRUE, quote = FALSE)
		else save (x, file = toFile)
		toFile
		}
	else x
	}


