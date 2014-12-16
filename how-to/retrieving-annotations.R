#' ---
#' title: "Retrieving annotation information in BIOM format"
#' author: ""
#' date: ""
#' ---

####  several files demonstrate valid formats for ID input
demoSets()

readSet(file)

#' simple retrieval of annotation data

yy <- biomRequest (file=ff[1])
head (rows (yy))

#' many arguments can modify what is retrieved

yy <- biomRequest (file=ff[1], group_level="level1")
rownames (yy)

#' taxonomic annotations

yy <- biomRequest (file=ff[4], request="organism", group_level="phylum", source="Greengenes")

#' IDs can be given directly, while output can be to a file

biomRequest ("mgp9", request="function", outfile="mgp9.biom")
biomRequest ("mgm4441619.3 mgm4441620.3 mgm4441656.4", 
  request="function", outfile="mgp9.biom")

#' place an asynchronous request...

yy <- biomRequest ("mgp9", wait=FALSE)
#'  ...and receive the data when convenient
yy <- biom (yy)


The most common options for data retrieval are documented above.  
It's not possible to document all possible, here.
These commands will give further information about available options.

doc.MGRAST (3, head=c("matrix","function","parameters","options"))
doc.MGRAST (3, head=c("matrix","organism","parameters","options"))

naturally, users are requesting very large data downloads.
some requests may simply be too large.  we don't know exactly the threshold.
please be aware that this can take a long time to complete,
and/or your request may simply be too large.
what is max?

auth.MGRAST()
auth.MGRAST(file="my_key.txt")
