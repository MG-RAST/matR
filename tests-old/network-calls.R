####################################################################################
### test suite for functions and invocations interacting with the API server
####################################################################################

library (matR)

####################################################################################
### collection() invoked with sample IDs
####################################################################################

# different ways to specify sample IDs
collection (guts)
collection ("4441679.3 4441680.3 4441682.3 4441695.3 4441696.3 4440463.3 4440464.3")
collection (file = "test-IDs.txt")

# specifying different data views explicitly
#   entry--- what returned numbers represent, or what processing has been applied to them
#   annot--- whether data pertains to functional or taxonomic annotations
#   level--- what level of the relevant annotation hierarchy is considered
#   source--- what annotation database is applied
#   hit--- what method of counting hits is used (taxonomic annotations only)
collection (guts, 
						raw = c(entry = "count"), 
						nrm = c(entry = "normed.counts"))
collection (guts, 
						L1 = c(level = "level1"), 
						L2 = c(level = "level2"), 
						L3 = c(level = "level3"), 
						L4 = c(level = "function"))
collection (guts, 
						nog = c(source = "NOG"), 
						cog = c(source = "COG"), 
						ko = c(source = "KO"))
collection (guts, 
						lca = c(annot = "organism", hit = "lca"), 
						repr = c(annot = "organism", hit = "single"), 
						all = c(annot = "organism", hit = "all"))

# complete information about data views:
view.parameters
view.descriptions
view.defaults

# create LISTS of data views (allows easy reuse)
all.levels <- list (
	L1 = c(level = "level1"), 
	L2 = c(level = "level2"), 
	L3 = c(level = "level3"), 
	L4 = c(level = "function"))
all.ontologies <- list (
	nog = c(source = "NOG"), 
	cog = c(source = "COG"), 
	ko = c(source = "KO"),
	sub = c(source = "Subsystems"))
all.count.methods <- list (
	lca = c(annot = "organism", hit = "lca"), 
	repr = c(annot = "organism", hit = "single"), 
	all = c(annot = "organism", hit = "all"))
collection (guts, all.levels)
collection (guts, all.ontologies)
collection (guts, all.count.methods)

# mixed invocations
collection (file = "test-IDs.txt",
						all.levels)
collection ("4441679.3 4441680.3 4441682.3 4441695.3 4441696.3 4440463.3 4440464.3",
						all.ontologies)

# mgm is always prefixed if missing, but may be included
collection ("mgm4441679.3 mgm4441680.3 mgm4441682.3 mgm4441695.3 mgm4441696.3 mgm4440463.3 mgm4440464.3",
						all.count.methods))

####################################################################################
### collection() invoked with project IDs, or project and sample IDs mixed
####################################################################################

# cc1 <- collection ("mgp757")
# cc2 <- collection ("mgp6 mgp20 mgp26 mgp50")
# cc3 <- collection ("mgp757 mgm4441679.3")
# cc4 <- collection ("mgp757 mgm4441679.3 mgm4441680.3")
# cc5 <- collection ("mgp6 mgp20 mgm4441679.3 mgm4441680.3")
# 
# projects (cc1)
# samples (cc1)
# 
# projects (cc2)
# samples (cc2)
# 
# projects (cc3)
# samples (cc3)
# 
# projects (cc4)
# samples (cc4)
# 
# projects (cc5)
# samples (cc5)

####################################################################################
### various invocations of mGet()
####################################################################################

# at this writing, the resources made available through the API are:
# 	"abundanceprofile", "analysisset", "annotation", "inbox", "library", "m5nr", "matrix" "metadata", 
# 	"metagenome", "metagenome_statistics", "notebook", "pcoa", "project", "sample", "sequence", "sequenceset", 
# 	"similarity", "status", "user"
# mGet is the generic matR interface to these; examples (not comprehesive) below

tmp <- tempfile()

######################################################
### !!! THESE DO NOT CRASH BUT THEY DO NOT WORK EITHER
######################################################

# basic syntax and options available for any API resource
mGet ("abundanceprofile", "mgm4441679.3")
mGet ("abundanceprofile", "mgm4441679.3", parse = FALSE)
mGet ("abundanceprofile", "mgm4441679.3", file = tmp)
mGet ("abundanceprofile", "mgm4441679.3", parse = FALSE, file = tmp)

# a smattering of examples...
# with optional parameters specific to the requested resource
# THESE CALLS WORK ...
mGet ("matrix", "4441679.3 4441680.3 4441682.3", name = "function")
mGet ("matrix", "4441679.3 4441680.3 4441682.3", name = "function", asynchronous = 1)
mGet ("project", "mgp24", verbosity = "minimal")
mGet ("project", "mgp24", verbosity = "full")
mGet ("project", NULL, verbosity = "full")
mGet ("project", NULL, verbosity = "full", limit = 20)
mGet ("metagenome", "mgm4441679.3", verbosity = "minimal")
mGet ("metagenome", "mgm4441679.3", verbosity = "full")
mGet ("metagenome", NULL, verbosity = "full")
mGet ("metagenome", NULL, verbosity = "full", limit = 20)

# THESE MAY OR MAY NOT ...
mGet ("abundanceprofile", "mgm4441679.3", source = "NOG")
mGet ("abundanceprofile", "mgm4441679.3", identity = 80)
mGet ("abundanceprofile", "mgm4441679.3", length = 20)
mGet ("abundanceprofile", "mgm4441679.3", evalue = 4)
mGet ("abundanceprofile", "mgm4441679.3", type = "organism")

####################################################################################
### various invocations of callRaw()
####################################################################################

callRaw (call="project?verbosity=minimal&limit=0")
callRaw (call="metagenome?verbosity=minimal&limit=0")

callRaw (call="project?verbosity=minimal&limit=0", parse = FALSE)
callRaw (call="metagenome?verbosity=minimal&limit=0", parse = FALSE)

callRaw (call="project?verbosity=minimal&limit=0", parse = FALSE, file = tmp)
callRaw (call="metagenome?verbosity=minimal&limit=0", parse = FALSE, file = tmp)

unlink (tmp)
