
# The "collection()" function requires a list of MG-RAST IDs such as:

ids <- c (cow.rumen = "4441679.3", fish.gut = "4441695.3", mouse.gut = "4440463.3")

# A collection of these samples plus several others comes with
# the matR package.  Just typing its name gives a summary:

Guts

# "views" show different aspects of a metagenome collection, and
# are accessed with "$".  For example, this shows raw abundance counts:

Guts$count

# This shows the same data, but normalized to lie between 0 and 1:

Guts$normed

# Various functions apply to a collection.  For instance, to show the
# samples it contains:
samples (Guts)

# To assign new names:
names (Guts) <- c ("c1", "c2", "c3", "f1", "f2", "m1", "m2")

# To assign a grouping (used by some analyses):
groups (Guts) <- c (1,1,1,2,2,3,3)

# And to show metadata of the collection:
mm <- metadata (Guts)
mm

# As you can see, metadata is a list of named fields, 
# and the names show a hierarchical organization.
#
# To select metadata fields, an arbitrary number of index vectors may be specified.
# We can use one index (of length one) to get all matching elements:
mm["0464"]
mm["body_product"]

# Or two indices, each length one:
mm["latitude", "longitude"]

# Or one index, of length two:
mm [c ("0464", "env_package.data")]

# Or three indices, all of length two:
mm [c ("0464", "PI"), c ("0464","seq_"), c ("0464","biome")]

# These are just examples to make it clear how to select what you're interested in.
# A list of character vectors is always returned, one per index supplied by you.
# Each entry in the returned list is a vector of every metadata field that 
# matches all elements of the corresponding index vector.
#
# A character vector is returned in two circumstances:
#	when only one index is specified, OR when no index matches more than one metadata field
#
# If that's too technical, just rely on the examples for illustrations.
#
# One other convenient feature creates a data.frame with one row per metagenome.
# If a requested metadata field is missing from a metagenome, NA is substituted.
# For this feature, use bygroup=TRUE, as follows:

mm ["latitude", "longitude", bygroup = TRUE]
mm ["host_common_name", "disease", ".age", bygroup = TRUE]

# When you make a collection, you can specify the views that you want.
# Views allow you to look at various data related to a given selection
# of metagenomes.  To show the views a collection contains, enter:
views (Guts)

# You can also add new views to an existing collection.  These functions
# are explained in detail in other demos.
