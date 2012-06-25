
### we look at seven metagenomes that come from, respectively,
### cow rumen (3), fish gut (2), lean (1) and obese (1) mouse gut

guts

### these are MG-RAST IDs that comes preloaded with matR to help get started.
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

### data from the collection is easy to export, like this:

asFile (M$count, fname = "guts_raw_counts.txt", sep = "\t")

### and applying to the collection a general-purpose visual rendering function
### shows as boxplots simple summaries of requested matrix views

render (M, views = c ("count", "normed"))

### that image clearly shows the utility of normalization.  We could also
### customize the image a bit with graphical parameters, and save it to a file.

render (M, views = c ("count", "normed"), fname = "guts_summaries.png", main = c ("Raw Counts", "Normalized Counts"), bg = "tan")

### similarity between samples may be quantified by various numerical methods,
### producing a lower-triangular matrix of pairwise measurements

dist (M$normed, method = "euclidean")

dist (M$normed, method = "bray-curtis")

### every functional annotation gives one dimension of difference between samples.
### a principal coordinates analysis uses a dissimilarity matrix, such as those
### just computed, to reduce the dimension of the comparison space

P <- pco (M$normed, method = "euclidean")

P

### the analysis is saved in the object P, and visual rendering of P will plot each sample
### according to its first and second principal coordinates, clustering those that are, overall,
### most similar.  first, we create a vector of colors to reflect the grouping we know exists.

groupcolors <- c (rep ("brown", 3), rep ("blue", 2), rep ("orange", 2))
groupcolors
guts

render (P, main = "PCoA on seven metagenomes", col = groupcolors, labels = names (guts))

### clustering according to our expectations is very clear.  In a heatmap visualization of the normalized
### abundance matrix, more detail appears in relation to specific functional annotations, as follows
### (several system messages may appear next, from loading additional packages)

heatmap (M$normed, image_out = "guts_HD.jpg", labRow = NA, labCol = NULL, col_lab_mult = 1, margins = c (8,1), image_title = "heatmap")

### the visualization was saved to a file which we now open

system ("open guts_HD.jpg")

### next we create a larger collection for a more elaborate analysis, now involving 24 metagenomes.
### fifteen come from a fresh water sample, the others from a hot spring (these take a bit longer to retrieve)

waters

W <- collection (waters)

### again we look at a principal coordinates analysis

render (pco (W$normed), main = "PCoA analysis, fresh vs. spring water samples", col = c (rep ("blue", 15), rep ("red", 9)))

### clustering is still apparent, although less clearly than in the previous example
### (and it is the second principal coordinate that differentiates the two groups).
### we can again create a heatmap visualization:

heatmap (W$normed, image_out = "waters_HD.jpg", labRow = NA, labCol = NULL, col_lab_mult = 1, margins = c (8,1), image_title = "heatmap")
system ("open waters_HD.jpg")

### a statistical test such as Kruskal-Wallis can help identify the most
### significant rows (annotations) and sharpen the picture.  We separate the samples
### into groups as required by the test:

grouping <- c (rep ("a", 15), rep ("b", 9))
results <- doStats (W$normed, grouping, "Kruskal-Wallis")

### a few rows from the test results look like this:

results [1:10,]

### from the p-value column, we select rows passing a significance threshold,
### and select the corresponding part of the original matrix

pvals <- results [ , 4]
subW <- W$normed [names (pvals) [pvals > 0.5], ]

### comparing dimensions of the original and subselection matrices shows what
### proportion of the original functional annotations are retained:

dim (W$normed)
dim (subW)

### finally, a heatmap of the subselected matrix highlights rows of interest more clearly than before:

heatmap (subW, image_out = "waters_sub_HD.jpg", labRow = NA, labCol = NULL, col_lab_mult = 1, margins = c (8,1), image_title = "subselection")
system ("open waters_sub_HD.jpg")
