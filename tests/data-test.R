#  make sure package sample data and related functions are in order
#

library(matR)

ff <- sampleSets()
cat ("Sample files:", ff, sep="\n")
li <- lapply (ff, readSet)

gg <- buildSets()
unlink (gg)
