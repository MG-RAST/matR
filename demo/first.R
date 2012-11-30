
### We look at seven metagenomes that come from cow rumen,
### fish gut, and mouse gut:

guts

### these MG-RAST IDs and related abundance data come preloaded with matR
### as examples.  See the "collections" demo to learn how to retrieve
### your own samples.

M <- Guts ; M

### this collection has five "views" which show:
### (1) raw abundance counts of functional annotations, (2) normalized counts,
### (3) average e-value, (4) average read length, and (5) average percent identity.

M$count

M$normed

### metadata is also part of the collection:

metadata (M)

### and specific items of interest can be accessed, this way:

metadata (M) [c ("4440464.3", "project_description")]

### data from the collection can be exported in CSV or tab-separated form:

asFile (M$count, file = "guts_raw_counts.csv", sep = ",")

### and boxplots give a simple visual summary of diversity within the samples:

render (M, view = "normed")

### similarity between samples can be quantified by various distance metrics,
### producing a triangular matrix of pairwise measurements:

dist (M, view = "normed", method = "euclidean")

dist (M, view = "normed", method = "bray-curtis")

### Next we use euclidean distance for a principal coordinates analysis (PCoA)
### to compare samples in a space of reduced dimension:

P <- pco (M, view = "normed", method = "euclidean")

### the analysis was plotted immediately, and the numerical results were
### also saved in the object P (which we could use for further analysis, 
### or export to a file):

P

### we can assign an explicit grouping to the samples, used for various
### purposes.  One feature is that they are automatically distinguished by color:

groups (M) <- c (1,1,1,2,2,3,3)
pco (M, view = "normed")

### the clustering is according to our expectations, of course.  In a heatmap of the
### normalized counts, more detail appears in relation to specific functional annotations:

heatmap (M, view = "normed", cexCol = 0.8)

### note that, as required for the dendrogram (tree diagram), samples are
### reordered in the lower margin.

