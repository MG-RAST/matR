#' ---
#' title: "Handling biom objects and metadata"
#' author: ""
#' date: ""
#' ---

#' This tutorial discusses slightly technical programming issues,
#' and will be most easily understood by experienced R users.
#' Still, the gist is simple.
#' The topic is how matR data objects relate to the built-in data types of R.
#' Annotation information is retrieved by the `biomRequest()` function as a `biom` object,
#' essentially a `matrix` with additional metadata annotations for the rows and columns.
#' Such objects can also be created by importing from a file of data in [BIOM format](http://biom-format.org)
#' produced by other software.
#'
#' `xx1` to `xx4` are example `biom` objects included with the matR package.

summary (xx1)

#' Here is an example showing part of the column metadata of `xx3`.
#' The information is returned as a `data.frame`.

head (columns (xx3, "latitude|longitude"))

#' Metadata is selected by a _regular expression_.  See `?regex` for details.
#' The regex below selects project IDs plus all metadata related to the environmental package.

names (columns (xx2, "project\\.id|^env_package"))

#' The regex can be omitted to show all metadata.
#' Metadata for rows typically consists of annotation hierarchy levels only.

names (rows (xx1))

#' A `data.frame` is returned even in case of a single metadata field,
#' so note that in this example, 
#' the `rownames()` and the single variable of the `data.frame` coincide.

head (rows (xx1, "ontology2"))

#' Metadata fields are almost always coded as `factor`s.
#'
#' The `biom` and `matrix` classes are very similar, aside from metadata.
#' Replacement functions, `dim()`, `rownames()`, `colnames()`, and `dimnames()`
#' can all be applied to `biom` objects.
#' This example renames columns with information taken from metadata.

yy <- xx4
colnames (yy) <- columns (yy, "sample.data.sample_name") [[1]]

#' Subsetting also works in familiar ways.

xx3 [1:10,1:2]

#' Subsetting by row and column names:

xx4 [c("Bacteria", "Eukaryota"), c("mgm4575333.3", "mgm4575334.3", "mgm4575335.3")]

#' Subsetting to keep metagenomes from only one biome:

summary (xx3 [ ,columns(xx3,"biome") == "Tundra biome"])

#' Subsetting to keep only rows matching a search term.

summary (xx1 [grepl("Protein secretion system", rownames(xx1)), ])

#' It can be useful to merge two `biom` objects, but be careful.
#' Since the operation requires only that all `colnames()` of the two objects be distinct,
#' it's possible to perform nonsense, as in this example
#' where the annotations of the merged objects are entirely unrelated.
#' (One is taxonomic and the other is functional).

tail (rownames (merge (xx1, xx4)))

#' In this more likely example, merging facilitates
#' differently normalizing metagenomes of a single original `biom` object.

#+ eval=FALSE
aa <- transform (xx4 [,1:8], t_Threshold, t_Log)
bb <- transform (xx4 [,9:16], t_Threshold=list(entry.min=5), t_Log)
xx4_norm <- merge (aa, bb)

#' It is easy to convert between `biom` class and the built-in types of R,
#' but note that BIOM data is often stored in a sparse format.
#' Consequently, this variation of the `as.matrix()` command is usually best:

head (as.matrix (xx1, expand=TRUE))

#' For comparison, here is the result of omitting the `expand=` option.

head (as.matrix (xx1))

#' JSON text is the native format of BIOM data and can be obtained
#' with either of the following commands.
#' The latter outputs to a file which could be used by other software.

#+ eval=FALSE
as.character (xx1)
as.character (xx1, file="xx1.biom")

#' Similarly, the next command creates a `biom` object from 
#' BIOM data in a file (created by matR or other software).

#+ eval=FALSE
biom (file="xx1.biom")
