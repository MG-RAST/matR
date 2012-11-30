
# ABOUT STATISTICAL AND VISUALIZATION TOOLS

# A very large library of statistical tools including many relevant to metagenomics
# is available in the R language.   This demo shows how to apply standard R routines, 
# and customized matR functions, to some datasets that are included with the matR package.

# Boxplots can be used to summarize diversity in a metagenome collection.  With the following
# command, we see boxplots for raw and normalized abundance counts:
render (Whalebone)
render (Marine)
render (Waters)

# We can see a PCoA this way:
pco (Coral, comp = c (1,2))

# It is a little more interesting when the samples are named, as in the following collection:
names (Waters)
pco (Waters, comp = c (1,2))

# And we can group them as well:
groups (Waters) <- c (rep (1, 15), rep (2, 9))
pco (Waters, comp = c (1,2), col = c (rep ("blue", 15), rep ("red", 9)))

# Here are some samples from three distinct hospital environments:
Hospital

metadata (Hospital) [c ("env_package", "sample_name")]

metadata (Hospital) ["ventilation_type"]

# We can group them accordingly and view the PCoA:
g <- factor (metadata (Hospital) ["ventilation_type"])
col <- g
levels (col) <- c ("red", "blue", "orange")
col <- as.character (col)
pco (Hospital, comp = c (1,2), col = col)

# That's a bit messy, so we can get rid of the labels:
pco (Hospital, comp = c (1,2), col = col, labels = "")

# And more detail is visible in three dimensions:
pco (Hospital, comp = c (1,2,3), color = col, labels = "")

# Also, we can see more by looking beyond the first three principal components:
pco (Hospital, comp = c (2,3,4), color = col, labels = "")
pco (Hospital, comp = c (3,4,5), color = col, labels = "")
pco (Hospital, comp = c (4,5,6), color = col, labels = "")

# A heatmap also gives a quick visual summary, as well as suggestions for further analysis:
heatmap (Hospital)

# From a different metagenome collection, compare this heatmap of
# raw counts (shown first) with the heatmap normalized counts (shown second):
heatmap (Guts, view = "count")
heatmap (Guts, view = "normed")

# And here are couple others:
heatmap (Whalebone, cexCol = 0.8)
heatmap (Waters)

# The following routine is the starting point for detailed
# statistical analysis.  It performs a specified statistical test
# on the provided collection, according to a given grouping of samples.
# This puts many numerical parameters at our disposal, usable for
# further analysis.

groups (Coral) <- c (rep (1, 6), rep (2,7))
res <- sigtest (Coral, test = "Kruskal-Wallis")

# Try demo2("second") for more about this.

