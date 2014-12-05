### Name: distx
### Title: Calculate distances with optional grouping and other features
### Aliases: distx distx.matrix distx.biom

### ** Examples

####  Euclidean distance between samples based on raw counts
distx (xx1)

####  alternate dissimilarity measure
distx (xx1, method="bray-curtis")

####  distance in log-transformed data
distx (transform (xx2, t_Log))

####  mean pairwise distance between biomes
distx (xx3, groups="$$biome", method="bray-curtis")



