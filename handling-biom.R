#' ---
#' title: "Handling biom objects and metadata"
#' author: ""
#' date: ""
#' ---

#' This tutorial discusses slightly technical programming issues,
#' and will be most easily understood by experienced R users.
#' Still, the gist is not too complicated.
#' The topic here is how matR data objects relate to the built-in data types of R.
#' Annotation information is retrieved by the `biomRequest()` function as a `biom` object,
#' essentially a `matrix` with additional metadata annotations for the rows and columns.
#' Such objects can also be created by importing from a file of data in [BIOM format](http://biom-format.org)
#' produced by other software.
#' `xx1` to `xx4` are example `biom` objects included with the matR package.
#' Here is an example showing part of the column metadata of `xx3`.
#' The information is returned as a `data.frame`.

columns (xx3, "latitude|longitude")

#' Even in case of a single field, a `data.frame` is returned.

is.data.frame (columns (xx3, "latitude"))

#' Metadata is selected by a _regular expression_ (see `?regex`).
#' The regex below selects project IDs plus all metadata related to the environmental package.

names (columns (xx2, "project\\.id|^env_package"))

#' The regex can be omitted to show all metadata.
#' Metadata for rows typically consists of annotation hierarchy levels only.

names (rows (xx1))

#' Note that in this example, 
#' the `rownames()` and the single variable of the `data.frame` coincide.

head (rows (xx1, "ontology2"), 9)

#' Metadata is almost always coded as a `factor`.

is.factor (columns (xx1, "sample.data.biome") [[1]])

#' The `biom` and `matrix` classes are similar aside from metadata.
#' Replacement functions, `dim()`, `rownames()`, `colnames()`, and `dimnames()`
#' can all be applied to `biom` objects.
#' This example renames columns with information taken from metadata.

yy <- xx4
colnames (yy) <- columns (yy, "sample.data.sample_name")

#' Subsetting also works in a familiar way.

xx3 [1:10,1:2]
xx4 [c("Bacteria", "Eukaryota"), c("mgm4575333.3", "mgm4575334.3", "mgm4575335.3")]

#' This command keeps metagenomes from only one biome.

summary (xx3 [ ,columns(xx3,"biome") == "Tundra biome"])

#' The command keeps only rows matching a search term.

summary (xx1 [grepl("Protein secretion system", rownames(xx1)), ])

#' It can be useful to merge two `biom` objects, but be careful.
#' Since the operation requires only that all `colnames()` of the two objects be distinct, 
#' it's possible to perform nonsense, as in this example
#' where the merged objects are annotated at different hierarchy levels.
#' (Look at the very end of the output!)

rownames (merge (xx1, xx4))

#' In this more likely example, a merge facilitates
#' differently normalizing metagenomes of a single original `biom` object.

#+ eval=FALSE
aa <- transform (xx4 [,1:8], t_Threshold, t_Log)
bb <- transform (xx4 [,9:16], t_Threshold=list(entry.min=5), t_Log)
xx4_norm <- merge (aa, bb)

#' Finally, it is always possible to convert back and forth between `biom` class and
#' the built-in data types of R.  Note an important difference between the following two commands.

as.matrix (xx1)
as.matrix (xx1, TRUE)

#' The second form is usually what is wanted.  `as.biom.list`, etc.
