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
