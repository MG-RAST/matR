
library(matR)

guts

M <- collection (guts)
M

sapply (M@data, dim)
colSums (as.matrix (M$count))

M$count
M$normed

render (M, views = c ("count", "normed"))

render (M, views = c ("count", "normed"), file = "summary.png", main = c ("Raw Counts", "Normalized Counts"), bg = "yellow", fg = "blue", ...)
system ("open sample_distribs.png")

dist (M$normed, "bray-curtis")
dist (M$normed, "euclidean")

pco (M, method = "euclidean")
P <- .Last.value

render (P)

render (P, file = "pco.png")






W <- collection (waters)
W

sapply (M@data, dim)
colSums (as.matrix (M$count))

W$count
W$normed

render (W)




heatmap (M$normed)
