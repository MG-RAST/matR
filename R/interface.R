############################################
### FRIENDLY FUNCTIONS FOR CASUAL USERS
###
### mGet is the engine.  Keep superficial
### the processing performed by these user-
### convenience functions.  The reason is:
### to quarantine the unpredictability of 
### changes in the API.
### 
### on the other hand, mGet should remain
### unaware of the matR class hierarchy.
### (it is an interface, so it should talk
### only in common languages.)
############################################

mProjects <- function () mListAll ("project")
mSamples <- function () mListAll ("sample")
mMetagenomes <- function () mListAll ("metagenome")

mProjectMeta <- function (projectIDs) mGet ("project", projectIDs)
mSampleMeta <- function (sampleIDs) mGet ("sample", sampleIDs)
mMetagenomeMeta <- function (metagenomeIDs) mGet ("metagenome", metagenomeIDs)


orgMatrix <- function (mgIDs, level = "species", source = "m5nr", noMeta = FALSE)
mGet ("abundance", mgIDs, param = paste ("format/plain/source/", source, "/group_level/", level, sep = ""))

orgMatrixEvalue <- function (mgIDs, level = "species", source = "m5nr", noMeta = TRUE)
mGet ("abundance", mgIDs, param = paste ("format/plain/result_column/evalue/source/", source, "/group_level/", level, sep = ""))

orgMatrixLength <- function (mgIDs, level = "species", source = "m5nr", noMeta = TRUE)
mGet ("abundance", mgIDs, param = paste ("format/plain/result_column/length/source/", source, "/group_level/", level, sep = ""))

orgMatrixPercentID <- function (mgIDs, level = "species", source = "m5nr", noMeta = TRUE) {
#	IDs <- chomp (mgIDs)
#	new ("mMatrix", IDs = IDs, mGet ("abundance", mgIDs, param = paste ("format/plain/result_column/identity/source/", source, "/group_level/", level, sep = "")))
#	if (! noMeta) for (id in IDs) y @ meta $ id <- mGet ("metagenome", id)
#	y
	mGet ("abundance", mgIDs, param = paste ("format/plain/result_column/identity/source/", source, "/group_level/", level, sep = ""))
	}


funcMatrix <- function (mgIDs, level = "level3", source = "Subsystem", noMeta = FALSE)
mGet ("abundance", mgIDs, param = paste ("format/plain/type/functional/source/", source, "/group_level/", level, sep = ""))

funcMatrixEvalue <- function (mgIDs, level = "level3", source = "Subsystem" , noMeta = TRUE)
mGet ("abundance", mgIDs, param = paste ("format/plain/type/functional/result_column/evalue/source/", source, "/group_level/", level, sep = ""))

funcMatrixLength <- function (mgIDs, level = "level3", source = "Subsystem" , noMeta = TRUE)
mGet ("abundance", mgIDs, param = paste ("format/plain/type/functional/result_column/length/source/", source, "/group_level/", level, sep = ""))

funcMatrixPercentID <- function (mgIDs, level = "level3", source = "Subsystem" , noMeta = TRUE)
mGet ("abundance", mgIDs, param = paste ("format/plain/type/functional/result_column/identity/source/", source, "/group_level/", level, sep = ""))

