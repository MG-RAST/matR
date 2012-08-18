
# ABOUT COLLECTIONS...

# How to build a metagenome collection:
ids <- c ("cow" = "4441679.3", fish = "4441695.3", mouse = "4440463.3")
cc <- collection (ids)
# Now you have an object cc to look at from every angle

# Or this simple syntax works too:
cc <- collection ("4441679.3 4441695.3 4440463.3")

# The predefined id list "guts" can help try out commands:
guts
cc <- collection (guts)

# Show the metagenomes of a collection in two slightly different ways:
selection (cc)
names (cc)

# Show the matrices in a collection:
views (cc)

# or just their names:
vnames (cc)

# Show both, with:
cc

# and examine a specific matrix with
cc$count

# Build a collection with custom views with specified names:
cc <- collection (guts, 
	FL1 = view (annotation = "function", level = "level1"),
	FL2 = view (annotation = "function", level = "level2"))

# Add a new view to an existing collection
cc$FL3 <- view (annotation = "function", level = "level3")

# Show or set names of selected metagenomes
names (cc)
names (cc) <- c ("cow1", "cow2", "cow3", "fish1", "fish2", "mouse1", "mouse2")

# Show or set metagenome groups for PCoA analysis (see below):
groups (cc)
groups (cc) <- c (1,1,1,2,2,3,3)


# ABOUT METADATA...

# Study metadata of a selection of ids, without building a collection:
ids
mm <- metadata (ids)
mm

# Or look at the metadata _of_ a collection:
cc <- collection (ids)
metadata (cc)

# Metadata is a list (of lists (of lists (of lists.....
names (mm)
names (mm$"4440463.3")
names (mm$"4440463.3"$metadata)
names (mm$"4440463.3"$metadata$library)
names (mm$"4440463.3"$metadata$library$data)
names (mm$"4440463.3"$metadata$library$data$seq_meth)
mm$"4440463.3"$metadata$library$data$seq_meth

# Select items or entire parts of metadata with a single index _vector_
j <- c ("4440463.3", "metadata", "project", "data", "project_description")
mm [[j]]
k <- c ("4440463.3", "metadata", "env_package")
mm [[k]]

# Subselect multiple metadata items with an index _list_
J <- list (
	host = c ("4440463.3", "metadata", "env_package", "data", "host_common_name"),
	body_prod = c ("4440463.3", "metadata", "env_package", "data", "body_product"),
	age = c ("4440463.3", "metadata", "env_package", "data", "age"))
mm[J]

# This slight modification can be useful:
> unlist (mm[J])

# Subselect the same item from all samples:
i <- c ("metadata", "env_package", "data", "host_common_name")
mm [ ,i]

# Or again a useful modification:
unlist (mm [ ,i])


# ABOUT ANALYSIS...

dianne mcgintee
