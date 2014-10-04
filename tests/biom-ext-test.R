library(matR)

dim(xx1)
dim(xx2)
dim(xx3)
dim(xx4)

length (intersect (colnames(xx1), colnames(xx2)))
length (setdiff (colnames(xx1), colnames(xx2)))
length (setdiff (colnames(xx2), colnames(xx1)))
length (intersect (rownames(xx1), rownames(xx2)))
length (setdiff (rownames(xx1), rownames(xx2)))
length (setdiff (rownames(xx2), rownames(xx1)))

length (intersect (colnames(xx1), colnames(xx3)))
length (setdiff (colnames(xx1), colnames(xx3)))
length (setdiff (colnames(xx3), colnames(xx1)))
length (intersect (rownames(xx1), rownames(xx3)))
length (setdiff (rownames(xx1), rownames(xx3)))
length (setdiff (rownames(xx3), rownames(xx1)))

length (intersect (colnames(xx2), colnames(xx4)))
length (setdiff (colnames(xx2), colnames(xx4)))
length (setdiff (colnames(xx4), colnames(xx2)))
length (intersect (rownames(xx2), rownames(xx4)))
length (setdiff (rownames(xx2), rownames(xx4)))
length (setdiff (rownames(xx4), rownames(xx2)))

#-----------------------------------------------------------------------------------------
#  merge()
#-----------------------------------------------------------------------------------------
applyBiomMethods (merge (xx1,xx2))
applyBiomMethods (merge (xx2,xx1))
applyBiomMethods (merge (xx1,xx3))
applyBiomMethods (merge (xx3,xx1))
applyBiomMethods (merge (xx1,xx4))
applyBiomMethods (merge (xx4,xx1))
applyBiomMethods (merge (xx2,xx3))
applyBiomMethods (merge (xx3,xx2))
applyBiomMethods (merge (xx2,xx4))
applyBiomMethods (merge (xx4,xx2))
applyBiomMethods (merge (xx3,xx4))
applyBiomMethods (merge (xx4,xx3))

#-----------------------------------------------------------------------------------------
#  rows() and columns()
#-----------------------------------------------------------------------------------------
rows (xx2)
dim (rows (xx2))
dimnames (rows (xx2))
class (rows (xx2))
rows (xx2,"1")
rows (xx2,"2")
levels (rows (xx2,"1") [[1]])
dim (rows (xx2,"1"))
dimnames (rows (xx2,"1"))
class (rows (xx2,"1"))

columns (xx1)
dim (columns (xx1))
dimnames (columns (xx1))
class (columns (xx1))
columns (xx1,"host_common_name")
dim (columns (xx1,"host_common_name"))
dimnames (columns (xx1,"host_common_name"))
class (columns (xx1,"host_common_name"))
columns (xx1,"samp_store_temp")
columns (xx1,"collection_time")
columns (xx1,"altitude")

#-----------------------------------------------------------------------------------------
#  subselection
#-----------------------------------------------------------------------------------------
xx1 [1:20, ]
xx1 [, c(1,2,4)]
xx1 [1:20, c(1,2,4)]

#-----------------------------------------------------------------------------------------
#  rows<-() and columns<-()
#-----------------------------------------------------------------------------------------
rows (xx1, "junk") <- 1:nrow(xx1)
columns (xx1, "junk") <- 1:ncol(xx1)

#-----------------------------------------------------------------------------------------
#  rownames<-() and colnames<-()
#-----------------------------------------------------------------------------------------

