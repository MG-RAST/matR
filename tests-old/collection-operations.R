####################################################################################
### test suite for operations on objects of class "collection"
####################################################################################

library (matR)

####################################################################################
### test: operations on collections
####################################################################################
# static elements of a collection:
samples (Guts)
projects (Guts)
views (Guts)
rownames (Guts, view = "raw")
rownames (Guts, view = "raw", sep = FALSE)
rownames (Guts, view = "raw", sep = TRUE)
rownames (Guts, view = "raw", sep = "\t")
# modifiable elements of a collection:
names (Guts)
groups (Guts)
viewnames (Guts)
metadata (Guts)
# assignments
nn <- names (Guts) ; names (Guts) <- as.character (1:7) ; names (Guts) <- nn
gg <- groups (Guts) ; groups (Guts) <- c (1,1,1,2,2,3,3) ; groups (Guts) <- gg
vv <- viewnames (Guts) ; viewnames (Guts) <- c ("a","b","c","d") ; viewnames (Guts) <- vv
# access views, as plain matrix
Guts$raw
Guts$nrm
Guts$nsc
Guts$nsn
# access views, potentially with attributes
Guts [["raw", full = FALSE, plain = FALSE]]
Guts [["raw", full = TRUE, plain = FALSE]]
Guts [["raw", full = TRUE, plain = TRUE]]
# add new views
Guts$taxa1 <- c (annot = "organism", level = "domain")
Guts$taxa2 <- c (annot = "organism", level = "phylum")
Guts$taxa3 <- c (annot = "organism", level = "class")
Guts$taxa4 <- c (annot = "organism", level = "order")
Guts$taxa5 <- c (annot = "organism", level = "family")
Guts$taxa6 <- c (annot = "organism", level = "genus")
Guts$taxa7 <- c (annot = "organism", level = "species")
Guts$taxa8 <- c (annot = "organism", level = "strain")
# extract subcollections
Cows <- Guts [c (1,2,3)]
Fish <- Guts [c (4,5)]
Mice <- Guts [c (6,7)]


####################################################################################
### test: metadata()[], metadata()[]<-
####################################################################################
# examples of flexibility of metadata indexing:
#   single index (length one);
#   single index (length one);
#   two indices (each length one);
#   same, but return value is formatted as data.frame;
#   single index (length two); and
#   three indices (each length two)
metadata (Guts) ["0464"]
metadata (Guts) ["body product"]
metadata (Guts) ["latitude", "longitude"]
metadata (Guts) ["latitude", "longitude", bygroup=TRUE]
metadata (Guts) [c("0464", "env package.data")]
metadata (Guts) [c("0464", "PI"), c("0464","seq "), c("0464","biome")]
# new metadata fields created by assignment:
host_abbrev <- factor (metadata (Guts) ["host_common_name"])
levels (host_abbrev) <- c ("c", "f", "m")
metadata (Guts) ["host_abbrev", bygroup = TRUE] <- as.character (host)

