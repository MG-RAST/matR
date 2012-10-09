
options (width=200)
library(matR)

###
### MG-RAST IDs
###

mSamples() [1:100]
mMetagenomes() [1:100]

###
### Metadata Retrieval
###

someProjects <- list()
PIDs <- mProjects()
for (id in PIDs [10:20]) someProjects [[id]] <- mProjectMeta (id)
for (p in someProjects) summary (p)
print (someProjects [[1]])

someMetagenomes <- list()
MGIDs <- c ("4441679.3", "4441680.3", "4441682.3", "4441695.3", "4441696.3", "4440463.3", "4440464.3")
for (id in MGIDs) someMetagenomes [[id]] <- mMetagenomeMeta (id)
for (m in someMetagenomes) summary (m)
print (someMetagenomes [[1]])

###
### Diversity Retrieval
###

aa <- orgMatrix ("4441679.3;4441680.3;4441682.3;4441695.3;4441696.3;4440463.3;4440464.3")
print (aa)
bb <- orgMatrixEvalue ("4441679.3;4441680.3;4441682.3;4441695.3;4441696.3;4440463.3;4440464.3")
print (bb)
cc <- orgMatrixLength ("4441679.3;4441680.3;4441682.3;4441695.3;4441696.3;4440463.3;4440464.3", level = "phylum")
print (cc)
dd <- list (
	m5nr = orgMatrix ("4441679.3;4441680.3;4441682.3;4441695.3;4441696.3;4440463.3;4440464.3", source = "m5nr" ),
	RefSeq = orgMatrix ("4441679.3;4441680.3;4441682.3;4441695.3;4441696.3;4440463.3;4440464.3", source = "RefSeq" ),
	Greengenes = orgMatrix ("4441679.3;4441680.3;4441682.3;4441695.3;4441696.3;4440463.3;4440464.3", source = "Greengenes" ))

###
### Retrieval by function is not yet implemented, but we will also show:
### funcMatrix ()
### funcMatrixEvalue ()
### funcMatrixLength ()
### funcMatrixPercentID ()
###

###
### Normalization of an abundance matrix
###

mNormalize (aa@data) [1:20,]

###
### Some statistical tests with metagenome groupings (wait for it....)
###

mStats (aa@data, c (1,1,1,2,2,3,3), sig_test = "ANOVA-one-way") [1:20,]
# mStats (aa@data, c (1,1,1,2,2,3,3), sig_test = "t-test-paired")
# mStats (aa@data, c (1,2,1,2,3,3,3), sig_test = "t-test-un-paired")
# mStats (aa@data, c (1,1,1,2,2,2,3,3,3), sig_test = "Kruskal-Wallis")

###
### Different similarity measures
###

mDist (aa@data, "bray-curtis")
mDist (aa@data, "euclidean")
mDist (aa@data, "minkowski")

###
### Results from PCA in numerical form
###

mPCA (aa@data, 5)

###
### Results from PCoA with similarity measure specified
###

mPCO (aa@data, method = "jaccard")

###
### Permute the abundance matrix (for significance testing)
###

mPermutations (aa@data, type = "sample") [1:20,]
mPermutations (aa@data, type = "dataset") [1:20,]

###
### Visualize the PCA
###

mPlotPCA (aa@data, 2, imgFile = "pca.png")
system ("open pca.png")

