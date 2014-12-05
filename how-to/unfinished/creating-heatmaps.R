#' ---
#' title: "Heatmap examples using image.biom()b"
#' author: ""
#' date: ""
#' ---

#' The generic `image()` function of R is defined by `matR` for class `biom` to produce a heatmap-dendrogram.
#' 
#' Here is a heatmap of raw values:
library(matR)
image (xx2)

#' But using a log transformation makes interesting things more apparent:
xx2t <- transform (xx2, t_Log)
image (xx2t, labCol="$$project.id")

#' Analysis restricted to Archaea:
image (xx2t, labCol="$$project.id", rows=rows(xx2t,"taxonomy1")=="Archaea")

#' Analysis restricted by significance test p-values:
p <- rowstats (xx2t, test="t-test-unpaired", groups="$$material") $ p.value
p [is.na(p)] <- p [is.nan(p)] <- FALSE
image (xx2t [rows = p < 0.05, ], labCol="$$material")



