
# ABOUT STATISTICAL AND VISUALIZATION TOOLS

# An immense library of statistical tools including very many relevant to metagenomics
# is available in the R language.   This demo shows how to apply standard R routines, 
# and customized matR functions, to some datasets that are included with the matR package.

# Boxplots can be used to summarize diversity in a metagenome collection.  With the following
# command, we see boxplots for raw and normalized abundance counts:
render (Marine)
render (Waters)
render (Whalebone)

graphics.off()

# We can see a PCoA this way:
pco (Coral)

# It is a little more interesting when the samples are named, as in the following collection:
pco (Waters)

# And of course we want to group them as well:
names (Waters)
groups (Waters) <- c (rep (1, 15), rep (2, 9))
pco (Waters)

# Here are some samples from three distinct hospital environments:
Hospital
unlist (metadata(Hospital)[,c("metadata","env_package","data","sample_name")])
unlist (metadata(Hospital)[,c("metadata","env_package","data","ventilation_type")])

# We can group them accordingly and view the PCoA:
groups (Hospital) <- c (rep (1,5), rep (2,4), rep (3,4))
pco (Hospital)

# More detail is visible in three dimensions:
pco (Hospital, comp = c (1,2,3))

# And also by looking beyond the first three principal components:
pco (Hospital, comp = c (2,3,4))
pco (Hospital, comp = c (3,4,5))
pco (Hospital, comp = c (4,5,6))

# A heatmap also gives a quick visual summary, as well as suggestions for further analysis:
heatmap (Hospital)

# Compare the heatmap of raw counts (shown first) with the heatmap of normalized counts (shown second):
heatmap (Guts, view = "count")
heatmap (Guts, view = "normed")

# And here are couple others:
heatmap (Whalebone)
heatmap (Waters)

# The following routine is the starting point for detailed
# statistical analysis.  It performs a specified statistical test
# on the provided collection, according to a given grouping of samples.
# This puts many numerical parameters at our disposal, usable for
# further analysis.
res <- sigtest (Coral$count, c (rep (1, 6), rep (2,7)), "Kruskal-Wallis")


