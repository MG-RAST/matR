### we create a collection for a multistep analysis involving 24 metagenomes.
### these take some time to retrieve.  Fifteen come from a fresh water sample, the others
### from a hot spring

waters

W <- collection (waters)

### we look at a principal coordinates analysis

render (pco (W$normed), main = "PCoA analysis, fresh vs. spring water samples", col = c (rep ("blue", 15), rep ("red", 9)))

### clustering is apparent, although not absolute, and it is the second
### principal coordinate that differentiates the two groups.
### we can create a heatmap visualization:

H <- heatmap (W$normed)

render (H, toFile = "waters_HD.png", labRow = NA, labCol = names (waters), col_lab_mult = 1.2, margins = c (8,1), main = "heatmap of 24 water samples")

### a statistical test such as Kruskal-Wallis can help identify the most
### significant rows (annotations) and sharpen the picture.  We separate the samples
### into groups and perform the test:

grouping <- c (rep ("a", 15), rep ("b", 9))
results <- sigtest (W$normed, grouping, "Kruskal-Wallis")

### a few rows from the test results look like this:

results [1:10,]

### from the p-value column, we select rows passing a significance threshold,
### and select the corresponding part of the original matrix

pvals <- results [ , 4]
subW <- W$normed [names (pvals) [pvals < 0.05], ]

### comparing dimensions of the original and subselection matrices shows what
### proportion of the original functional annotations are retained:

dim (W$normed)
dim (subW)

### finally, a heatmap of the subselected matrix highlights rows of interest more clearly than before:

render (heatmap (subW), toFile = "waters_sub_HD.jpg", labRow = NA, labCol = names (waters), col_lab_mult = 1.2, margins = c (8,1), main = "subselection")

### this analysis could continue by setting more restrictive p-value thresholds, like this:

subW01 <- W$normed [names (pvals) [pvals < 0.01], ]
subW003 <- W$normed [names (pvals) [pvals < 0.003], ]

### or by examining factors in metadata.  Subgroups discernible
### in the heatmap can in fact be associated to metadata in this case.

