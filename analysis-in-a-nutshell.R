#' ---
#' title: "R-based analysis in a nutshell"
#' author: ""
#' date: ""
#' ---
#' 
#' This tutorial shows the flavor of metagenomic analysis in R with a short "toy" example.
#' The analysis workflow presented here appears simple, but it is quite powerful.
#' These half-dozen commands can be modified in many ways for more specific situations.
#' Separate tutorials describing usage details for each are available (or will be soon).
#' 
#' An actual analysis would start with a file of metagenome IDs, one per line.
#' Here, for demonstration purposes, we first create such a file.

library(matR)
writeLines (colnames (xx1), "my_ids.txt")

#' The next command requests a matrix of abundance counts of function annotations
#' at Subsystems level 2 for the specified metagenomes.
#' Many variations of this command are available.
#' Note that the request may take several minutes to complete.

zz <- biomRequest(file="my_ids.txt", request="function", group_level="level2", evalue=1)

#' Technically, the returned object `zz` is not a `matrix` but a classed `biom` object.
#' Such objects carry metadata that can enrich analyses.
#' As a first step, it is usually effective to transform the raw abundance counts, as follows.

zz0 <- transform (zz, t_Threshold, t_Log)

#' Both columns (metagenomes) and rows (annotations) of `biom` objects have metadata.
#' The entirety of metadata, or a selection of specific entries (as below), can be examined.

columns (zz0, "host_common_name|samp_store_temp|ncbi_id")

#' Plotting principal coordinates is a standard visualization, shown next.
#' Note how the command uses metadata to determine features of the plot.
#' Details of the `princomp.biom()` function are available in a separate tutorial,
#' to help obtain exactly the desired graphical output.

princomp (zz0, map=c(col="host_common_name", pch="samp_store_temp"), cex=1.5, labels="$$ncbi_id")

#' A computation of distances between samples underlies the principal coordinates plot.
#' That computation, or a groupwise-distance (as here), can be examined directly,
#' and various distance metrics are available.

distx (zz0, groups="$$host_common_name")

#' A statistical assessment of the significance for group differentiation
#' of each annotation can be made.
#' Here we simultaneously filter the results at a p-value threshold of 0.05.

pp <- (rowstats (zz0, groups="$$material") $ p.value < 0.05)
rownames (zz0) [pp]

#' A heatmap restricted to the identified functions neatly shows group differentiation.
#' In this image, annotations are labeled by their top-level place in the function hierarchy,
#' rather than by the specific function names listed just above.

image (zz0 [pp,], labCol="$$ncbi_id", labRow="$$ontology1", margins=c(5,10))

#' Fine control of the heatmap image, including labeling and colors,
#' is described in a separate tutorial for the `image.biom()` function.
