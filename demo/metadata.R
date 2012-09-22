# HOW TO WORK WITH METADATA (press return repeatedly to step through)

# Metadata can be examined from a collection, or directly.  So the following code
# shows two ways of getting the same thing.  First way:
ids <- c (cow = "4441679.3", fish = "4441695.3", mouse = "4440463.3")
cc <- collection (ids)
mm <- metadata (cc)

# Second way:
mm <- metadata (ids)

# Metadata is a complex structure:
mm

# Metadata is a list (of lists (of lists (of lists.....
# With "names" we examine components at successive stages,
# and eventually reach elementary metadata items:
names (mm)
names (mm$"4440463.3")
names (mm$"4440463.3"$metadata)
names (mm$"4440463.3"$metadata$library)
names (mm$"4440463.3"$metadata$library$data)
names (mm$"4440463.3"$metadata$library$data$seq_meth)
mm$"4440463.3"$metadata$library$data$seq_meth

# An "index vector" selects specific parts or items of metadata:
j <- c ("4440463.3", "metadata", "env_package")
mm [[j]]
k <- c ("4440463.3", "metadata", "env_package", "data", "body_product")
mm [[k]]

# An "index list" selects multiple items simultaneously:
jj <- list (host = c ("4440463.3", "metadata", "env_package", "data", "host_common_name"), body_prod = c ("4440463.3", "metadata", "env_package", "data", "body_product"), age = c ("4440463.3", "metadata", "env_package", "data", "age"))
mm[jj]

# Finally, a special syntax can be used to select the same item from all samples:
kk <- c ("metadata", "env_package", "data", "host_common_name")
mm [ ,kk]

# By the way, applying "unlist" to a subselection can put the data into
# a simpler structure:
unlist (mm [ ,kk])

