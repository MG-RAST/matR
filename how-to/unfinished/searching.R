### Name: dir.MGRAST
### Title: List directory of projects
### Aliases: dir.MGRAST

### ** Examples

## Not run: 
##D ####  names of all public projects
##D dir.MGRAST()$name
##D 
##D ####  ids of all public projects
##D rownames (dir.MGRAST())
##D 
##D ####  investigators who have contributed public projects
##D unique (dir.MGRAST()$pi)
##D 
##D ####  first 25 projects submitted to MG-RAST
##D dir.MGRAST (len=25, order="id")
##D 
##D ####  detailed information about them
##D names (dir.MGRAST(len=25, order="id", verbosity="verbose"))
##D 
##D ####  quick look at public projects
##D strtrim (dir.MGRAST (verbosity="verbose")$description, 70)
## End(Not run)

####  relevant documentation for the underlying API call
doc.MGRAST (3, head=c('project','query','parameters','options'))



### Name: search.MGRAST
### Title: Find metagenomes matching specified criteria
### Aliases: search.MGRAST

### ** Examples

####  relevant documentation for the underlying API call
doc.MGRAST (3, head=c('metagenome','query','parameters','options'))



### Name: metadata.character
### Title: Get metadata of projects and metagenomes
### Aliases: metadata metadata.character

### ** Examples

## Not run: 
##D ####  three levels of detail for project metadata
##D xx <- "mgp21 mgp24 mgp30"
##D metadata (xx)
##D metadata (xx, detail=TRUE)
##D names (metadata (xx, detail="verbose"))
##D 
##D ####  similar (but not identical) for metagenome metadata
##D yy <- "mgm4440066.3 mgm4440062.3 mgm4440055.3 mgm4441681.3 mgm4440463.3 mgm4440464.3"
##D metadata (yy)
##D metadata (yy, detail=TRUE)
##D names (metadata (yy, detail="metadata"))
## End(Not run)

####  relevant documentation for underlying API calls
doc.MGRAST (3, head=c('project','instance','parameters','options'))
doc.MGRAST (3, head=c('metagenome','instance','parameters','options'))



