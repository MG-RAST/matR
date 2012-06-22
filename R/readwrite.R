
#################################################
### "asFile" is a generic function for exporting
### matR objects, the natural opposite of constructing
### an object of the same type from a filename or connection.
###
### asFile(x, ...) : creates file with matR-standard representations of objects (maybe "as.file" or does that mislead?)
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

setGeneric ("asFile", 
	def = function (x, ...) { standardGeneric ("asFile") },
	useAsDefault = FALSE)

setMethod ("asFile",			# for lists of IDs (this may be stupid)
	"character",				# asFile (<file>) to rewrite in different formats?  well, that is cute...
	function (x, ...) { } )
setMethod ("asFile",
	"rlist",
	function (x, ...) { } )
setMethod ("asFile",
	"matrix",
	function (x, ...) { } )
setMethod ("asFile",
	"Matrix",
	function (x, ...) { } )
setMethod ("asFile", 
	"mmatrix",
	function (x, ...) { } )
#setMethod ("asFile", 
#	"RBIOM",
#	function (x, ...) { } )
#setMethod ("asFile", 
#	"pcaRes",
#	function (x, ...) { } )
### writing PCA to file:  need to concatenate two tables into single object.  snippet from original:
###	if (! is.null (asFile)) {
###		write.table (P@R2,	file = asFile, sep = "\t", col.names = FALSE, row.names = TRUE, append = FALSE)
###		write.table (P (my_pcaRes), file = asFile, sep = "\t", col.names = FALSE, row.names = TRUE, append = TRUE)
#		}
#setMethod ("asFile", 
#	"mPCoA",
#	function (x, ...) { } )
#setMethod ("asFile", 
#	"dendrogram",
#	function (x, ...) { } )	
#setMethod ("asFile",
#	"mHeatmap",
#	function (x, ...) { } )

# just an idea:
reconcileTextParametersWithDefaults <- function (...) { }
reconcileGraphicsParametersWithDefaults <- function (...) { }
