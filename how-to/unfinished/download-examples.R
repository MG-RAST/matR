### Name: BIOMretrieval
### Title: Get annotation information of samples as BIOM data
### Aliases: biomRequest biom.environment

### ** Examples

####  several files demonstrate valid formats for ID input
demoSets()

## Not run: 
##D ff <- demoSets()
##D 
##D ####  simple retrieval of annotation data
##D yy <- biomRequest (file=ff[1])
##D head (rows (yy))
##D 
##D ####  many arguments can modify what is retrieved
##D yy <- biomRequest (file=ff[1], group_level="level1")
##D rownames (yy)
##D 
##D ####  taxonomic annotations
##D yy <- biomRequest (file=ff[4], request="organism", group_level="phylum", source="Greengenes")
##D 
##D ####  IDs can be given directly, while output can be to a file
##D biomRequest ("mgp9", request="function", outfile="mgp9.biom")
##D biomRequest ("mgm4441619.3 mgm4441620.3 mgm4441656.4", 
##D   request="function", outfile="mgp9.biom")
##D 
##D ####  place an asynchronous request...
##D yy <- biomRequest ("mgp9", wait=FALSE)
##D ####  ...and receive the data when convenient
##D yy <- biom (yy)
## End(Not run)

####  full detail for available options
doc.MGRAST (3, head=c("matrix","function","parameters","options"))
doc.MGRAST (3, head=c("matrix","organism","parameters","options"))
doc.MGRAST (3, head=c("matrix","feature","parameters","options"))



