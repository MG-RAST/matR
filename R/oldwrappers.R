
############################################
### FRIENDLY FUNCTIONS FOR CASUAL USERS
### ---first version of such, maintained, but deprecated in favor of selection / view mechanism---
###
### mGet is the engine.  these are wrappers.  keep superficial the processing performed here.
### mGet should be not unaware, and not fully aware, but half-aware of class structures.
### the critical thing is that it quarantines changes in the API.
### it is an interface, so it speaks common languages.
############################################

mProjects <- function () mListAll ("project")
mSamples <- function () mListAll ("sample")
mMetagenomes <- function () mListAll ("metagenome")

############################################

# --there should be some handling here to make a list into a properly classed
# --items, when there are multiple IDs
mProjectMeta <- function (projectIDs) new ("rlist", mGet ("project", projectIDs))
mSampleMeta <- function (sampleIDs) new ("rlist", mGet ("sample", sampleIDs))
mMetagenomeMeta <- function (metagenomeIDs) new ("rlist", mGet ("metagenome", metagenomeIDs))

############################################

### CHECK SOURCE AND LEVEL DEFAULTS

orgMatrix <- function (mgIDs, level = "species", source = "m5nr", noMeta = FALSE)
new ("mmatrix", data = 
	mGet ("abundance", mgIDs, param = paste ("format/plain/source/", source, "/group_level/", 
		level, sep = "", enClass = FALSE)),
	metadata = new ("rlist"))

orgMatrixEvalue <- function (mgIDs, level = "species", source = "m5nr", noMeta = TRUE)
new ("mmatrix", data = 
	mGet ("abundance", mgIDs, param = paste ("format/plain/result_column/evalue/source/", source, "/group_level/", 
		level, sep = ""), enClass = FALSE),
	metadata = new ("rlist"))

orgMatrixLength <- function (mgIDs, level = "species", source = "m5nr", noMeta = TRUE)
new ("mmatrix", data = 
	mGet ("abundance", mgIDs, param = paste ("format/plain/result_column/length/source/", source, "/group_level/", 
		level, sep = ""), enClass = FALSE),
	metadata = new ("rlist"))

orgMatrixPercentID <- function (mgIDs, level = "species", source = "m5nr", noMeta = TRUE)
new ("mmatrix", data = 
	mGet ("abundance", mgIDs, param = paste ("format/plain/result_column/identity/source/", source, "/group_level/", 
		level, sep = ""), enClass = FALSE),
	metadata = new ("rlist"))

############################################

funcMatrix <- function (mgIDs, level = "level3", source = "Subsystems", noMeta = FALSE)
new ("mmatrix", data = 
	mGet ("abundance", mgIDs, param = paste ("format/plain/type/function/source/", source, "/group_level/", 
		level, sep = ""), enClass = FALSE),
	metadata = new ("rlist"))

funcMatrixEvalue <- function (mgIDs, level = "level3", source = "Subsystems" , noMeta = TRUE)
new ("mmatrix", data = 
	mGet ("abundance", mgIDs, param = paste ("format/plain/type/function/result_column/evalue/source/", source, "/group_level/", 
		level, sep = ""), enClass = FALSE),
	metadata = new ("rlist"))

funcMatrixLength <- function (mgIDs, level = "level3", source = "Subsystem" , noMeta = TRUE)
new ("mmatrix", data = 
	mGet ("abundance", mgIDs, param = paste ("format/plain/type/function/result_column/length/source/", source, "/group_level/", 
		level, sep = ""), enClass = FALSE),
	metadata = new ("rlist"))

funcMatrixPercentID <- function (mgIDs, level = "level3", source = "Subsystems" , noMeta = TRUE)
new ("mmatrix", data = 
	mGet ("abundance", mgIDs, param = paste ("format/plain/type/function/result_column/identity/source/", source, "/group_level/", 
		level, sep = ""), enClass = FALSE),
	metadata = new ("rlist"))
