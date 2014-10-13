library(matR)
N.examples <- 1:4
ExList <- mget (paste0 ("xx", N.examples), inherits=TRUE)

#-----------------------------------------------------------------------------------------
#  CRAN tests
#-----------------------------------------------------------------------------------------






#-----------------------------------------------------------------------------------------
#  DEVEL tests and feature demonstrations
#-----------------------------------------------------------------------------------------
	
#-----------------------------------------------------------------------------------------
#  merge()
#-----------------------------------------------------------------------------------------

f <- function (x, y) {					# function to check integrity of merged object
	z <- merge (x,y)
	applyBiomMethods (z)
	message ("rows:\t", nrow(x), "(x)\t", nrow(y), "(y)\t", nrow(z), "(merge)\t", 
		length (intersect (rownames(x), rownames(y))), "(in common)\n")
	message ("cols:\t", ncol(x), "(x)\t", ncol(y), "(y)\t", ncol(z), "(merge)\t", 
		length (intersect (colnames(x), colnames(y))), "(in common)\n")
	}

for (j in matrix2list (t (combn (N.examples, 2)))) {
	x <- ExList [[j [1]]]
	y <- ExList [[j [2]]]
	f(x,y) ; f(y,x)						# merge each pair
	}

#-----------------------------------------------------------------------------------------
#  rows() and columns()
#-----------------------------------------------------------------------------------------

for (x in ExList) {
	str (rows (x))						# all row/column annotations
	str (columns (x))

	str (rows (x, "a"))					# many matches
	str (columns (x, "a"))

	str (rows (x, "syzygy"))			# no match							# not sure this result is correct
	str (columns (x, "syzygy"))
	}	

#-----------------------------------------------------------------------------------------
#  rows<-() and columns<-()
#-----------------------------------------------------------------------------------------
for (x in ExList) {
	rows (x, "junk") <- 1:nrow(x)		# assign data of adequate length
	columns (x, "junk") <- 1:ncol(x)

	rows (x, "junk") <- 1:2				# assign too-short data
	columns (x, "junk") <- 1:2
	}

#-----------------------------------------------------------------------------------------
#  subselection
#-----------------------------------------------------------------------------------------
for (x in ExList) {
	x [1:20, ]							# just test a few random things
	x [, c(1,2,4)]
	x [1:20, c(1,2,4)]
	
	x [1,]								# but the special case of one index, in particular
	x [,1]
	}

#-----------------------------------------------------------------------------------------
#  dimnames<-()
#-----------------------------------------------------------------------------------------
for (x in ExList) {
	rownames(x) <- 1:nrow(x)			# assign data of adequate length
	colnames(x) <- 1:ncol(x)

	rownames(x) <- 1					# assign too-short data
	colnames(x) <- 1
	}
