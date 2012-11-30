
############################################
### FRIENDLY FUNCTIONS FOR CASUAL USERS
### first attempt at user-friendly wrappers, deprecated in favor of selection / view mechanism
###
### how aware should mGet() be of the user-facing class structure?  an open question.
### it has an enClass parameter which for now is always FALSE.
### the critical thing is that it quarantines changes in the API.
### it is an interface, so it speaks common languages.
############################################

mProjects <- function () mListAllIds ("project")
mSamples <- function () mListAllIds ("sample")
mMetagenomes <- function () mListAllIds ("metagenome")

############################################

# CHECK: do these work?  do they work for multiple IDs?  is the result then properly classed?

mProjectMeta <- function (ids) new ("rlist", listify (mGet ("project", scrubIds (ids))))
mSampleMeta <- function (ids) new ("rlist", listify (mGet ("sample", scrubIds (ids))))
mMetagenomeMeta <- function (ids) new ("rlist", listify (mGet ("metagenome", scrubIds (ids))))

############################################

orgMatrix <- function (ids, level = "species", source = "m5rna", noMeta = FALSE)
new ("mmatrix", data = 
	mGet ("abundance", scrubIds (ids), param = paste ("format/plain/source/", source, "/group_level/", 
		level, sep = "", enClass = FALSE)))

orgMatrixEvalue <- function (ids, level = "species", source = "m5rna", noMeta = TRUE)
new ("mmatrix", data = 
	mGet ("abundance", scrubIds (ids), param = paste ("format/plain/result_column/evalue/source/", source, "/group_level/", 
		level, sep = ""), enClass = FALSE))

orgMatrixLength <- function (ids, level = "species", source = "m5rna", noMeta = TRUE)
new ("mmatrix", data = 
	mGet ("abundance", scrubIds (ids), param = paste ("format/plain/result_column/length/source/", source, "/group_level/", 
		level, sep = ""), enClass = FALSE))

orgMatrixPercentID <- function (ids, level = "species", source = "m5rna", noMeta = TRUE)
new ("mmatrix", data = 
	mGet ("abundance", scrubIds (ids), param = paste ("format/plain/result_column/identity/source/", source, "/group_level/", 
		level, sep = ""), enClass = FALSE))

############################################

funcMatrix <- function (ids, level = "level3", source = "Subsystems", noMeta = FALSE)
new ("mmatrix", data = 
	mGet ("abundance", scrubIds (ids), param = paste ("format/plain/type/function/source/", source, "/group_level/", 
		level, sep = ""), enClass = FALSE))

funcMatrixEvalue <- function (ids, level = "level3", source = "Subsystems" , noMeta = TRUE)
new ("mmatrix", data = 
	mGet ("abundance", scrubIds (ids), param = paste ("format/plain/type/function/result_column/evalue/source/", source, "/group_level/", 
		level, sep = ""), enClass = FALSE))

funcMatrixLength <- function (ids, level = "level3", source = "Subsystems" , noMeta = TRUE)
new ("mmatrix", data = 
	mGet ("abundance", scrubIds (ids), param = paste ("format/plain/type/function/result_column/length/source/", source, "/group_level/", 
		level, sep = ""), enClass = FALSE))

funcMatrixPercentID <- function (ids, level = "level3", source = "Subsystems" , noMeta = TRUE)
new ("mmatrix", data = 
	mGet ("abundance", scrubIds (ids), param = paste ("format/plain/type/function/result_column/identity/source/", source, "/group_level/", 
		level, sep = ""), enClass = FALSE))
