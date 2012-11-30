
### For a multistep analysis, we use a collection of 24 metagenomes.
### Fifteen come from a fresh water sample, the others from a hot spring

W <- Waters ; W

### we look at a principal coordinates analysis, with
### samples colored by group:

groups (W) <- c (rep (1, 15), rep (2, 9))
col <- groups (W)
levels (col) <- c ("blue", "red")
col <- as.character (col)
pco (W, comp = c (1,2), main = "fresh vs. spring water samples", col = col)

### clustering is apparent, although not absolutely, and it is the second
### principal coordinate that differentiates the two groups.  We can also look
### at the PCoA in three dimensions:

pco (W, comp = c (1,2,3), main = "fresh vs. spring water samples", color = col)

### next we create a heatmap visualization:

H <- heatmap (W)

### a statistical test such as Kruskal-Wallis can help identify the most
### significant rows (annotations) and sharpen the picture.  We separate the samples
### into groups and perform the test:

results <- sigtest (W, test = "Kruskal-Wallis")

### a few rows from the test results look like this:

head (results$sd)

head (results$mean)

head (results$stat)

### from the p-value column, we can identify rows passing a significance threshold,
### and select the corresponding part of the original matrix

Wsub <- W$normed [results$stat$p.value < 0.05, ]

### comparing number of rows between the original and subselection matrices
### shows what proportion of the original functional annotations are retained:

nrow (W$normed)
nrow (Wsub)

### and a heatmap of the subselected rows highlights areas of interest more clearly than before:

heatmap (W, rows = (results$stat$p.value < 0.05))

### this analysis could continue by setting more restrictive p-value thresholds, like this:

heatmap (W, rows = (results$stat$p.value < 0.005))
heatmap (W, rows = (results$stat$p.value < 0.0005))

### and here are the functions represented in the heatmap with highest granularity:

rownames (results$stat) [results$stat$p.value < 0.0005]

### We could also continue by examining factors in metadata.  In fact, in this case,
### associations between metadata and subgroups discernible in the heatmap would be found.
