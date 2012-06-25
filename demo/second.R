
library(matR)

# first, we look at seven metagenomes that come from, respectively:
# cow rumen (3), fish gut (2), lean (1) and obese (1) mouse gut

guts

# these are MG-RAST IDs that comes preloaded with matR to help get started.
# for analysis, we download the related abundance data:

M <- collection (guts)
M

# this collection has five "matrix views" which show, respectively:
# (1) raw abundance counts of functional annotations, (2) normalized counts,
# (3) average e-value, (4) average read length, and (5) average percent identity.
# we can browse the data, as follows:

M$count
M$normed

# the data is easy to export

asFile (M$count, fname = "guts_raw_counts.txt", sep = "\t")

# applying to the collection a general-purpose visual rendering function
# shows as boxplots simple summaries of requested matrix views

render (M, views = c ("count", "normed"))

# that image clearly shows the utility of normalization.
# specifying graphical parameters, we can customize the image a bit and save it to a file

render (M, views = c ("count", "normed"), file = "guts_summaries.png", main = c ("Raw Counts", "Normalized Counts"), bg = "yellow", fg = "blue", ...)
system ("open guts_summaries.png")

# similarity between samples may be quantified by various numerical methods,
# producing a lower-triangular matrix of pairwise measurements

dist (M$normed, method = "euclidean")
dist (M$normed, method = "bray-curtis")

# every functional annotation gives one dimension of difference between samples.
# a principal coordinates analysis uses a dissimilarity matrix, such as those
# just computed, to reduce the dimension of the space of variation

pco (M, method = "euclidean")
P <- .Last.value

# now the analysis is saved to an object P, and visual rendering of P will plot each sample 
# according to its first and second principal coordinates, clustering those that are, overall,
# most similar

render (P)

# clustering here is quite clear, 
# more detail appears in a heatmap visualization of the normalized abundance matrix, as follows
# (serveral system messages may appear next, from loading additional packages)

heatmap (M$normed, image_out = "guts_HD.jpg", labRow = NA, labCol = NULL, col_lab_mult = 1, margins = c (8,1), image_title = "Some Persuasive Science")

# the visualization was saved to a file which we now open

system ("open guts_HD.jpg")

# now we create a larger collection for a more elaborate analysis, this time involving 24 metagenomes.
# fifteen come from a fresh water sample, the others from a hot spring.

waters
W <- collection (waters)

# again we look at a principal coordinates analysis

render (pco (W$normed))

# and a heatmap visualization

heatmap (M$normed, image_out = "waters_HD.jpg", labRow = NA, labCol = NULL, col_lab_mult = 1, margins = c (8,1), image_title = "Some Persuasive Science")

# clustering in the pco was less clear, so we apply a statistical significance test,
# generating a p-value for each row (annotation).  First, we create a variable to 
# divide the samples into two groups:

groups <- c (rep ("a", 15), rep ("b", 9))

results <- doStats (M$normed, groups, sig_test = "Kruskal-Wallis")

# we are interested in the p-value for each row, seen in the fourth column, as follows

results [1:10,]








### inspection

sapply (M@data, dim)
colSums (as.matrix (M$count))


### sub-selection

pco(W)
pco(W$normed)
render(.Last.value)
doStats
doStats(M$normed,c(rep(1,15),rep(2,9),sig_test="Kruskal-Wallis")
doStats(M$normed,c(rep(1,15),rep(2,9)),sig_test="Kruskal-Wallis")
doStats(G$normed,c(rep(1,15),rep(2,9)),sig_test="Kruskal-Wallis")
doStats(W$normed,c(rep(1,15),rep(2,9)),sig_test="Kruskal-Wallis")
.Last.value->res
class(res)
dim(res)
dimnames(res
)
sapply(M$data,dim)
sapply(W@data,dim)
res[1:10,]
sigres <- res[,4]
sigres
sigres >.5
sigres[sigres>.5]
sigres[sigres>.5]->SIGRES
dim(SIGRES)
length(SIGRES)
M$normed
W$normed
W$normed [names(SIGRES),]
.Last.value -> subsetW
dim(subsetW)
render(pco(subsetW))
render(pco(W$normed))
render(pco(subsetW))
dev.new()
render(pco(subsetW))
render(pco(subsetW))
dev.new
dev.new()
render(pco(W$normed))
pco(subsetW)
render(pco(G$normed))
q()
ls()
res
ls()
?history
history()
history(max=50)
