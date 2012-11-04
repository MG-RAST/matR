
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
selection (Guts)

# To assign new names:
names (Guts) <- c ("c1", "c2", "c3", "f1", "f2", "m1", "m2")

# To assign a grouping (used by some analyses):
groups (Guts) <- c (1,1,1,2,2,3,3)

# And to show metadata of the collection:
mm <- metadata (Guts) ; mm

# Metadata is a complex structure, a list (of lists (of lists (of lists.....
# With "names" we examine components at successive stages,
# and eventually reach elementary metadata items:
names (mm)
names (mm$"mgm4440463.3")
names (mm$"mgm4440463.3"$metadata)
names (mm$"mgm4440463.3"$metadata$library)
names (mm$"mgm4440463.3"$metadata$library$data)
names (mm$"mgm4440463.3"$metadata$library$data$seq_meth)
mm$"mgm4440463.3"$metadata$library$data$seq_meth

# An "index vector" selects specific parts or items of metadata:
j <- c ("mgm4440463.3", "metadata", "env_package")
mm [[j]]
k <- c ("mgm4440463.3", "metadata", "env_package", "data", "body_product")
mm [[k]]

# And a special syntax selects the same item from all samples:
kk <- c ("metadata", "env_package", "data", "host_common_name")
unlist (mm [ ,kk])

# When you make a collection, you can specify the views that you want.
# To show the views a collection contains, use:
views (Guts)

# You can also add new views to an existing collection.  These functions
# are explained in detail in other demos.
