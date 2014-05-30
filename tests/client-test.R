#######################################################################
## get data:
##    with direct ID specification
##    using a data.frame with metadata
##    using file containing IDs only
##    using file of IDs and metadata
##    varying MG-RAST parameters
## parameters to "matrix" resource:  (all optional)
##    asynchronous source result_type filter group_level grep length evalue identity filter_source hide_metadata id filter_level
############################################
cc <- c("4441679.3 4441680.3 4441682.3 4441695.3 4441696.3 4440463.3 4440464.3")
bb <- biom(biomRequest(cc))

ss <- c("mgm4441679.3", "mgm4441680.3", "mgm4441682.3", "mgm4441695.3", "mgm4441696.3", "mgm4440463.3", "mgm4440464.3")
bb <- biom(biomRequest(ss))

dd <- data.frame(samples=ss, group=c("alpha","alpha","bravo","charlie","delta","delta","charlie"))
bb <- biom(biomRequest(dd))

tt <- tempfile()
writeLines(ss, tt)
bb <- biom(biomRequest(file=tt))
unlink(tt)

tt <- tempfile()
write.table(dd, file=tt, quote=F, row.names=F)
bb <- biom(biomRequest(file=tt))
unlink(tt)

biom(biomRequest(file=tt, result_type="identity"))
biom(biomRequest(file=tt, group_level="level2"))
biom(biomRequest(file=tt, identity=85))
biom(biomRequest(file=tt, evalue=4))
biom(biomRequest(file=tt, filter_source="SSU"))
biom(biomRequest(file=tt, request="organism"))
biom(biomRequest(file=tt, request="organism", filter_level="genus"))
