#' ---
#' title: "Handling metadata of biom objects"
#' author: ""
#' date: ""
#' ---

#' Objects of class biom have metadata for their rows and columns.

#'  Here are exact sampling locations returned in a data.frame:
columns (xx3, "latitude|longitude")

#'  Note that a data.frame is returned even in case of a single matching metadata field:
is.data.frame (columns (xx1, "sample.data.biome"))

#'  Project IDs and environmental package metadata -- note use of regex here and above:
colnames (columns (xx2, "project\\.id|^env_package"))

#'  Row metadata makes annotation hierarchy levels available, so typical row metadata has few components, and here just two:
names (rows (xx1))
rows (xx1, "ontology1")

#'  Here, the rownames and the (single) variable of the data.frame coincide:
rows (xx1, "ontology2")

#'  And variables are almost always coded as factors:
is.factor (columns (xx1, "sample.data.biome") [[1]])


### Name: BIOMmerge
### Title: Merge BIOM data
### Aliases: merge.biom

### ** Examples

####  merging requires only that all colnames be unique, so nonsense can be performed
merge (xx1, xx4)

####  a more likely example, based on applying different normalizations
aa <- transform (xx4 [,1:8], t_Threshold, t_Log)
bb <- transform (xx4 [,9:16], t_Threshold=list(entry.min=5), t_Log)
xx4_norm <- merge (aa, bb)



### Name: BIOMrename
### Title: Change row and column identifiers of BIOM data
### Aliases: dimnames<-.biom

### ** Examples

####  even if not particularly useful, this is allowed
yy <- xx4
dimnames (yy) <- list (letters [1:nrow(yy)], LETTERS [1:ncol(yy)])

####  more useful: renaming columns by codes taken from metadata
colnames (yy) <- columns (yy, "sample.data.sample_name")



### Name: BIOMsubset
### Title: Take part of (subset) BIOM data
### Aliases: [.biom

### ** Examples

####  explicit subsetting
xx3 [ , 1:8]
xx4 [c ("Bacteria", "Eukaryota"), c ("mgm4575333.3", "mgm4575334.3", "mgm4575335.3")]

####  keep only metagenomes from one biome
xx3 [ , columns (xx3, "biome") == "Tundra biome"]

####  keep only rows matching a search term
xx1 [grepl ("Protein secretion system", rownames(xx1)), ]



