#  ways to get data:
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
#

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

#  metadata(...)
