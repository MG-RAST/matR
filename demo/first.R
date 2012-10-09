
### we look at seven metagenomes that come from, respectively,
### cow rumen (3), fish gut (2), lean (1) and obese (1) mouse gut

guts

### these are MG-RAST IDs that come preloaded with matR to help get started.
### for analysis, we download the related abundance data:

M <- collection (guts)

M

### this collection has five "matrix views" which show, respectively:
### (1) raw abundance counts of functional annotations, (2) normalized counts,
### (3) average e-value, (4) average read length, and (5) average percent identity.
### we can browse the data, as follows:

M$count

M$normed

### metadata is also part of the collection object:

metadata (M)

### and specific items of interest can be directly accessed within the metadata hierarchy:

j <- c ("4440464.3", "metadata", "project", "data", "project_description")
metadata (M) [[j]]

### data from the collection is easy to export in comma-separated or tab-separated form (and then
### import into Excel or another program) like this:

asFile (M$count, fname = "guts_raw_counts.txt", sep = "\t")

### and applying to the collection a general-purpose visual rendering function
### shows as boxplots simple summaries of requested matrix views

render (M, views = c ("count", "normed"))

### that image clearly shows the utility of normalization.  We could also
### customize the image a bit with graphical parameters, and save it to a file.

render (M, views = c ("count", "normed"), toFile = "guts_summaries.png", main = c ("Raw Counts", "Normalized Counts"))

### similarity between samples may be quantified by various numerical methods,
### producing a lower-triangular matrix of pairwise measurements

dist (M$normed, method = "euclidean")

dist (M$normed, method = "bray-curtis")

### every functional annotation gives one dimension of difference between samples.
### a principal coordinates analysis (PCoA) uses a dissimilarity matrix, such as those
### just computed, to reduce the dimension of the comparison space.  For this PCoA,
### we use euclidean distance

P <- pco (M$normed, method = "euclidean")

P

### the analysis is saved in the object P, and visual rendering of P will plot each sample
### according to selected principal coordinates.  Here we use the first and second coordinates,
### clustering those that are most similar, overall.  First, we create a vector of colors to reflect
### the grouping we know exists

groupcolors <- c (rep ("brown", 3), rep ("blue", 2), rep ("orange", 2))
groupcolors
guts

render (P, main = "PCoA of seven metagenomes", col = groupcolors, labels = names (guts))

### clustering according to our expectations is very clear.  In a heatmap visualization of the normalized
### abundance matrix, more detail appears in relation to specific functional annotations, as follows
### (several system messages may appear next, from loading additional packages)

render (heatmap (M$normed), toFile = "guts_HD.jpg", labRow = NA, labCol = names (guts), col_lab_mult = 1.2, margins = c (9,1), main = "heatmap")

### note that the samples are reordered in the lower margin.  That is
### necessary for the dendrogram (tree diagram).

