
dontrun <- function () {
#######################################################
### test metadata retrieval, printing, summarizing
#######################################################

### test retrieval of all resource IDs

PIDs <- mProjects ()
SIDs <- mSamples ()
MIDs <- mMetagenomes ()
print (PIDs)
print (SIDs)
print (MIDs)

### retrieval of metadata for 20 random projects

some <- sample (length (PIDs), 20)
pp <- list ()
for (j in some) pp [[PIDs [j]]] <- mProjectMeta (PIDs [j])
for (e in pp) print (e)
for (e in pp) summary (e)

### retrieval of metadata for 20 random samples

some <- sample (length (SIDs), 20)
ss <- list ()
for (j in some) ss [[SIDs [j]]] <- mSampleMeta (SIDs [j])
for (e in ss) print (e)
for (e in ss) summary (e)

### retrieval of metadata for 20 random metagenomes

some <- sample (length (MIDs), 20)
mm <- list ()
for (j in some) mm [[MIDs [j]]] <- mMetagenomeMeta (MIDs [j])
for (e in mm) print (e)
for (e in mm) summary (e)

}