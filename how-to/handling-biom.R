#' ---
#' title: "Handling biom objects and metadata"
#' author: ""
#' date: ""
#' ---

also import from other sources.

#' This tutorial discusses slightly technical programming issues,
#' and will be most easily understood by experienced R users.
#' Still, the gist is not too complicated.
#' The topic, here, is how matR data objects relate to the built-in data types of R.
#' Annotation information is retrieved by the `biomRequest()` function as a `biom` object,
#' essentially a `matrix` with additional metadata annotations for the rows and columns.

#' `xx1` to `xx4` are example objects included with the matR package.  
#' Here is how to inspect sampling locations, contained in the column metadata of `xx3`.
#' The information is returned as a `data.frame`.

columns (xx3, "latitude|longitude")

#' Note that a `data.frame` is returned even in case of a single matching metadata field.

is.data.frame (columns (xx1, "sample.data.biome"))

#' Desired metadata fields are specified by a _regular_expression_.
#' An explanation of regular expressions in R can be found with `?regex`.
#' Below, project IDs and all environmental package metadata are selected.

colnames (columns (xx2, "project\\.id|^env_package"))

#' Row metadata typically consists of (only) annotation hierarchy levels.
 available, so typical row metadata has few components, and here just two:
This example also shows that the xxx can be omitted entirely, to show all metadata.

names (rows (xx1))
rows (xx1, "ontology1")

#' Here, the rownames and the (single) variable of the `data.frame` coincide:

rows (xx1, "ontology2")

#' Metadata variables are almost always coded as `factors`.

is.factor (columns (xx1, "sample.data.biome") [[1]])


#' Leaving metadata aside, the `biom` and `matrix` classes are similar.
#' `rownames()`, `colnames()`, `dimnames()`, and associated replacement functions all
#' can be applied to `biom` objects.

####  even if not particularly useful, this is allowed
yy <- xx4
dimnames (yy) <- list (letters [1:nrow(yy)], LETTERS [1:ncol(yy)])

####  more useful: renaming columns by codes taken from metadata
colnames (yy) <- columns (yy, "sample.data.sample_name")


#' Subsetting `biom` objects also works just as for the `matrix` class.

xx3 [ , 1:8]
xx4 [c ("Bacteria", "Eukaryota"), c ("mgm4575333.3", "mgm4575334.3", "mgm4575335.3")]

####  keep only metagenomes from one biome
xx3 [ , columns (xx3, "biome") == "Tundra biome"]

####  keep only rows matching a search term
xx1 [grepl ("Protein secretion system", rownames(xx1)), ]


#' It can be useful to merge two `biom` objects, but be careful.
#' Since the operation requires only that all `colnames()` from the two objects be distinct, 
#' it's possible to perform nonsense, as in this example,
#' where the merged objects are annotated at different hierarchy levels.

merge (xx1, xx4)

#' In this more likely example, the `merge.biom()` operation is used to facilitate
#' applying slightly varied normalizations to different metagenomes of a single
#' original `biom` object.

aa <- transform (xx4 [,1:8], t_Threshold, t_Log)
bb <- transform (xx4 [,9:16], t_Threshold=list(entry.min=5), t_Log)
xx4_norm <- merge (aa, bb)

Finally, it is always possible to convert back and forth between `biom` class and
the built-in data types of R.  Note an important difference between the following two commands.

as.matrix.biom.Rd

Although less familiar to R users, the second form is usually what is wanted.

as.biom.list etc
