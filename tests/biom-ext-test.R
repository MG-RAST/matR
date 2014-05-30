
#######################################################################
## biom manipulations:
##    rownames/colnames --- behaves as matrix
##    rows/columns --- specialized, for metadata
##    subselection
#######################################################################
rownames(bb)
colnames(bb)
rownames(bb) <- 1:nrow(bb)
colnames(bb) <- 1:ncol(bb)

rows(bb)
rows(bb,"taxonomy2")
rows(bb) <- data.frame(number=1:nrow(bb))
tt <- tempfile()
writeLines(1:nrow(bb), tt)
rows(bb) <- read.table(tt, col.names="number2")

columns(bb)
columns(bb,"env")
columns(bb) <- data.frame(number=1:ncol(bb))
writeLines(c("alpha","alpha","bravo","charlie","delta","delta","charlie"), tt)
columns(bb) <- read.table(tt, col.names="group")
unlink(tt)

bb[1:20,]
bb[,c(1,2,4)]
bb[1:20, c(1,2,4)]