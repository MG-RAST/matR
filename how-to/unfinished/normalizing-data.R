### Name: transform.biom
### Title: Apply mathematical transformations to BIOM data
### Aliases: transform transform.biom t_ColCenter t_ColScale t_Log
###   t_NA2Zero t_Threshold

### ** Examples

####  simple log-transform
transform (xx1, t_Log)

####  additional filters
transform (xx1, t_NA2Zero, t_Threshold, t_Log)

####  what is lost with more stringent filtering of low-abundance annotations
yy <- transform (xx2, t_NA2Zero, t_Threshold, t_Log)
zz <- transform (xx2, t_NA2Zero, t_Threshold=list(entry.min=5, row.min=10), t_Log)
setdiff (rownames (yy), rownames (zz))

####  each sample centered around zero; scaling columnwise by standard deviation
transform (xx4, t_NA2Zero, t_Threshold, t_Log, t_ColCenter, t_ColScale)

####  defining a new transformation that indicates presence / absence
t_Indicator <- function (x, ...) { ifelse (x,1,0) }
transform (xx1, t_Threshold = list(entry.min=5), t_Indicator)



