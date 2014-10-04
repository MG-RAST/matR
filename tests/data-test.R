library(matR)

#-----------------------------------------------------------------------------------------
#  check package sample data and related functions
#-----------------------------------------------------------------------------------------
ff <- demoSets()
cat ("Sample files:", ff, sep="\n")
li <- lapply (ff, readSet)
gg <- buildDemoSets()
unlink (gg)
