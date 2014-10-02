
#-----------------------------------------------------------------------------------------
#  biomRequest
#
#  important variations:
#    file containing IDs only
#    file of IDs and metadata
#    direct ID specification
#    data.frame with metadata
#    blocking, file output
#    varying API parameters
#
#  API parameters for "matrix" resource; all optional:
#    asynchronous source result_type filter group_level grep length 
#    evalue identity filter_source hide_metadata id filter_level
#-----------------------------------------------------------------------------------------

library(matR)

tt <- tempfile()
ff <- sampleSets() [1]				# only IDs
gg <- sampleSets () [2]				# also metadata

xx <- readSet (ff)					# vector
yy <- readSet (gg)					# data.frame

biomRequest (file=ff)
biomRequest (file=gg)
biomRequest (xx)
biomRequest (yy)

biomRequest (xx, quiet=TRUE)
biomRequest (xx, blocking=3)
biomRequest (xx, outfile=tt)
biomRequest (xx, blocking=3, outfile=tt)
unlink(tt)

biomRequest (xx, request="function")
biomRequest (xx, request="function", group_level="level1")
biomRequest (xx, request="function", group_level="level2")
biomRequest (xx, request="function", group_level="level3")
biomRequest (xx, request="function", group_level="function")
biomRequest (xx, request="function", group_level="level1", evalue=1)
biomRequest (xx, request="function", group_level="level1", evalue=1, length=20)
biomRequest (xx, request="function", group_level="level1", evalue=1, length=20, identity=85)
biomRequest (xx, request="function", group_level="level1", evalue=1, length=20, identity=85, filter_source="NOG")

biomRequest (xx, request="organism")
biomRequest (xx, request="organism", group_level="domain")
biomRequest (xx, request="organism", group_level="phylum")
biomRequest (xx, request="organism", group_level="species")
biomRequest (xx, request="organism", group_level="strain")
biomRequest (xx, request="organism", group_level="domain", evalue=1)
biomRequest (xx, request="organism", group_level="domain", evalue=1, length=20)
biomRequest (xx, request="organism", group_level="domain", evalue=1, length=20, filter_source="Greengenes")

biomRequest (xx, request="organism", hit_type="all")
biomRequest (xx, request="organism", hit_type="single")

biomRequest (xx, request="organism", result_type="abundance")
biomRequest (xx, request="organism", result_type="identity")

ticket <- biomRequest (xx, wait=FALSE)
# ... here you can go for a coffee break, or do some other calculations; then later:
xx <- biom (ticket, wait=TRUE)


#-----------------------------------------------------------------------------------------
#  metadata(detail=NULL)
#
#  ...which means just lookup ids
#-----------------------------------------------------------------------------------------

metadata ("mgp21")
metadata ("mgp21 mgp24")
metadata ("mgp21 mgp24 mgp30")				# example

metadata("mgm4440066.3")
metadata("mgm4440066.3 mgm4440062.3")
metadata("mgm4440066.3 mgm4440062.3 mgm4440055.3")
metadata("mgm4440066.3 mgm4441681.3")
metadata("mgm4440066.3 mgm4440062.3 mgm4441681.3")
metadata("mgm4440066.3 mgm4440062.3 mgm4440055.3 mgm4441681.3")
metadata("mgm4440066.3 mgm4441681.3 mgm4441682.3")
metadata("mgm4440066.3 mgm4440062.3 mgm4441681.3 mgm4441682.3")
metadata("mgm4440066.3 mgm4440062.3 mgm4440055.3 mgm4441681.3 mgm4441682.3")
metadata("mgm4440066.3 mgm4441681.3 mgm4441682.3 mgm4440463.3")
metadata("mgm4440066.3 mgm4440062.3 mgm4441681.3 mgm4441682.3 mgm4440463.3")
metadata("mgm4440066.3 mgm4440062.3 mgm4440055.3 mgm4441681.3 mgm4441682.3 mgm4440463.3")
metadata("mgm4440066.3 mgm4441681.3 mgm4440463.3 mgm4440464.3")
metadata("mgm4440066.3 mgm4440062.3 mgm4441681.3 mgm4440463.3 mgm4440464.3")
metadata("mgm4440066.3 mgm4440062.3 mgm4440055.3 mgm4441681.3 mgm4440463.3 mgm4440464.3")   # example

#-----------------------------------------------------------------------------------------
#  metadata(detail=TRUE)
#  ...which just means verbosity="minimal"
#-----------------------------------------------------------------------------------------

metadata ("mgp21", detail=TRUE)
metadata ("mgp21 mgp24", detail=TRUE)
metadata ("mgp21 mgp24 mgp30", detail=TRUE)			# example
metadata ("mgm4440463.3", detail=TRUE)
metadata ("mgm4440463.3 mgm4440464.3", detail=TRUE)
metadata ("mgm4440463.3 mgm4440464.3 mgm4441679.3", detail=TRUE)
metadata ("mgm4440066.3 mgm4440062.3 mgm4440055.3 mgm4441681.3 mgm4440463.3 mgm4440464.3", detail=TRUE) # example

#-----------------------------------------------------------------------------------------
#  metadata(detail=...
#    ...for projects:		c("minimal","verbose","full")
#    ...for metagenomes:	c("minimal","metadata","stats","full")
#
#  ...which are relayed directly as "verbosity" to call.MGRAST()
#-----------------------------------------------------------------------------------------

metadata ("mgp21", detail="verbose")
metadata ("mgp21 mgp24", detail="verbose")
metadata ("mgp21 mgp24 mgp30", detail="verbose")	# example; show names() of
metadata ("mgp21", detail="full")
metadata ("mgp21 mgp24", detail="full")
metadata ("mgp21 mgp24 mgp30", detail="full")

metadata ("mgm4440463.3", detail="metadata")
metadata ("mgm4440463.3 mgm4440464.3", detail="metadata")
metadata ("mgm4440463.3 mgm4440464.3 mgm4441679.3", detail="metadata")
metadata ("mgm4440066.3 mgm4440062.3 mgm4440055.3 mgm4441681.3 mgm4440463.3 mgm4440464.3", detail="metadata")    # example; show names() of
metadata ("mgm4440463.3", detail="stats")										# bad idea, don't show
metadata ("mgm4440463.3 mgm4440464.3", detail="stats")							# bad idea, etc
metadata ("mgm4440463.3 mgm4440464.3 mgm4441679.3", detail="stats")				# bad idea, etc
metadata ("mgm4440463.3", detail="full")										# bad idea..
metadata ("mgm4440463.3 mgm4440464.3", detail="full")							# bad idea..
metadata ("mgm4440463.3 mgm4440464.3 mgm4441679.3", detail="full")				# bad idea..

#-----------------------------------------------------------------------------------------
#  metadata, input from file
#-----------------------------------------------------------------------------------------

tt <- tempfile()
writeLines ("mgm4440463.3", file=tt) 
metadata (map=TRUE, file=tt)
writeLines (c("mgm4440463.3", "mgm4440464.3"), file=tt) 
metadata (map=TRUE, file=tt)
writeLines (c("mgm4440463.3", "mgm4440464.3", "mgm4441679.3"), file=tt) 
metadata (map=TRUE, file=tt)
writeLines ("mgm4440463.3", file=tt) 
metadata (map=FALSE, file=tt)
writeLines (c("mgm4440463.3", "mgm4440464.3"), file=tt) 
metadata (map=FALSE, file=tt)
writeLines (c("mgm4440463.3", "mgm4440464.3", "mgm4441679.3"), file=tt) 
metadata (map=FALSE, file=tt)
unlink(tt)

tt <- tempfile()
writeLines ("mgp21", file=tt); metadata (map=TRUE, file=tt)
writeLines (c ("mgp21", "mgp24"), file=tt) ; metadata (map=TRUE, file=tt)
writeLines (c ("mgp21", "mgp24", "mgp30"), file=tt) ; metadata (map=TRUE, file=tt)
writeLines ("mgp21", file=tt); metadata (map=FALSE, file=tt)
writeLines (c ("mgp21", "mgp24"), file=tt); metadata (map=FALSE, file=tt)
writeLines (c ("mgp21", "mgp24", "mgp30"), file=tt) ; metadata (map=FALSE, file=tt)
unlink(tt)


#-----------------------------------------------------------------------------------------
#  dir.MGRAST(from=1, to=50, length.out, ..., quiet=FALSE)
#
#  "..." can be:
#    verbosity = c("minimal", "verbose", "full")
#    order = c("id", "name")
#    limit = ["integer"]
#    offset = ["integer"]
#  cf:
#    doc.MGRAST(3, head=c('project','query','parameters','options'))}
#-----------------------------------------------------------------------------------------

dir.MGRAST()
dir.MGRAST (1, 50)
dir.MGRAST (1, 100)
dir.MGRAST (1, 200)
dir.MGRAST (1, 500)
dir.MGRAST (1, 1000)
dir.MGRAST (100, 150)
dir.MGRAST (100, 200)
dir.MGRAST (100, 300)
dir.MGRAST (100, 600)
dir.MGRAST (100, 1100)
dir.MGRAST (500, 550)
dir.MGRAST (500, 600)
dir.MGRAST (500, 700)
dir.MGRAST (500, 1000)
dir.MGRAST (100, len=50)
dir.MGRAST (100, len=100)
dir.MGRAST (100, len=200)
dir.MGRAST (100, len=500)
dir.MGRAST (100, len=1000)

dir.MGRAST (1, 50, order="id")
dir.MGRAST (1, 100, order="id")
dir.MGRAST (1, 200, order="id")
dir.MGRAST (1, 500, order="id")
dir.MGRAST (1, 1000, order="id")
dir.MGRAST (100, 150, order="id")
dir.MGRAST (100, 200, order="id")
dir.MGRAST (100, 300, order="id")
dir.MGRAST (100, 600, order="id")
dir.MGRAST (100, 1100, order="id")
dir.MGRAST (500, 550, order="id")
dir.MGRAST (500, 600, order="id")
dir.MGRAST (500, 700, order="id")
dir.MGRAST (500, 1000, order="id")
dir.MGRAST (100, len=50, order="id")
dir.MGRAST (100, len=100, order="id")
dir.MGRAST (100, len=200, order="id")
dir.MGRAST (100, len=500, order="id")
dir.MGRAST (100, len=1000, order="id")

dir.MGRAST (1, 50, verbosity="verbose")
dir.MGRAST (1, 100, verbosity="verbose")
dir.MGRAST (1, 200, verbosity="verbose")
dir.MGRAST (1, 500, verbosity="verbose")
dir.MGRAST (1, 1000, verbosity="verbose")
dir.MGRAST (100, 150, verbosity="verbose")			# lot of columns named NA here.  debug that sometime
dir.MGRAST (100, 200, verbosity="verbose")
dir.MGRAST (100, 300, verbosity="verbose")
dir.MGRAST (100, 600, verbosity="verbose")
dir.MGRAST (100, 1100, verbosity="verbose")
dir.MGRAST (500, 550, verbosity="verbose")
dir.MGRAST (500, 600, verbosity="verbose")
dir.MGRAST (500, 700, verbosity="verbose")
dir.MGRAST (500, 1000, verbosity="verbose")
dir.MGRAST (100, len=50, verbosity="verbose")
dir.MGRAST (100, len=100, verbosity="verbose")
dir.MGRAST (100, len=200, verbosity="verbose")
dir.MGRAST (100, len=500, verbosity="verbose")
dir.MGRAST (100, len=1000, verbosity="verbose")

dir.MGRAST (1, 50, verbosity="full")
dir.MGRAST (1, 100, verbosity="full")
dir.MGRAST (1, 200, verbosity="full")
# dir.MGRAST (1, 500, verbosity="full")			# hangs
# dir.MGRAST (1, 1000, verbosity="full")			# didn't try
dir.MGRAST (100, 150, verbosity="full")
dir.MGRAST (100, 200, verbosity="full")
dir.MGRAST (100, 300, verbosity="full")
# dir.MGRAST (100, 600, verbosity="full")			# didn't try
# dir.MGRAST (100, 1100, verbosity="full")		# didn't try
dir.MGRAST (500, 550, verbosity="full")
dir.MGRAST (500, 600, verbosity="full")
dir.MGRAST (500, 700, verbosity="full")
# dir.MGRAST (500, 1000, verbosity="full")		# didn't try
dir.MGRAST (100, len=50, verbosity="full")
dir.MGRAST (100, len=100, verbosity="full")
dir.MGRAST (100, len=200, verbosity="full")
# dir.MGRAST (100, len=500, verbosity="full")		# didn't try
# dir.MGRAST (100, len=1000, verbosity="full")	# didn't try

dir.MGRAST (offset=0, limit=50)
dir.MGRAST (offset=0, limit=100)
dir.MGRAST (offset=0, limit=200)
dir.MGRAST (offset=0, limit=500)
dir.MGRAST (offset=0, limit=1000)
dir.MGRAST (offset=99, limit=50)
dir.MGRAST (offset=99, limit=100)
dir.MGRAST (offset=99, limit=200)
dir.MGRAST (offset=99, limit=500)
dir.MGRAST (offset=99, limit=1000)
dir.MGRAST (offset=499, limit=50)
dir.MGRAST (offset=499, limit=100)
dir.MGRAST (offset=499, limit=200)
dir.MGRAST (offset=499, limit=500)


#-----------------------------------------------------------------------------------------
#  search.MGRAST(...)
#
#  "..." can be:
#    verbosity = c("minimal","mixs","metadata","stats","full")
#    status = c("both","public","private")
#    match = c("all","any")
#    offset = [integer]
#    limit = [integer]
#    order = [string]
#    direction = c("asc","desc")
#    ----
#    function		"search parameter: query string for function"
#    metadata		"search parameter: query string for any metadata field"
#    md5			"search parameter: md5 checksum of feature sequence"
#    organism		"search parameter: query string for organism"
#  cf:
#    doc.MGRAST(3, head=c('metagenome','query','parameters','options'))}
#-----------------------------------------------------------------------------------------



