
# HOW TO WORK WITH METADATA

# Here are some ids from metagenomes in MG-RAST:
guts

# Metadata can be examined from a collection, or directly.  So the following code
# shows two ways of getting the same thing.  First way:
cc <- collection (guts) ; mm <- metadata (cc)

# Second way:
mm <- metadata (guts)

# Metadata is a list of named fields, and the names show a hierarchical organization:
mm

# To select metadata fields, an arbitrary number of index vectors may be specified.
# We can use one index (of length one) to get all matching elements,
# as in this example:
mm["0464"]

# And same thing in this example:
mm["body_product"]

# Or we can use two indices, each length one:
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
# when only one index is specified, OR,
# when no index matches more than one metadata field
#
# If that's too technical, just rely on the previous examples as models.
#
# One other convenient feature creates a data.frame with one row per metagenome.
# If a requested metadata field is missing from a metagenome, NA is substituted.
# For this feature, use bygroup=TRUE, as follows:

mm ["latitude", "longitude", bygroup = TRUE]

# In that example, no values were missing, but in this one, some are:
mm ["host_common_name", "disease", ".age", bygroup = TRUE]

