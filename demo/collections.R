
# HOW TO WORK WITH METAGENOME COLLECTIONS

# You need to have ids for the metagenomes you're interested in.
# Find them through the MG-RAST website.  If there are many, consider using
# read.table() or some similar function to read them from a text file.

# To create a collection in your session, do something like this
# (and expect the command to take some time to complete):

ids <- c (cow = "4441679.3", fish = "4441695.3", mouse = "4440463.3")
cc <- collection (ids)

# The names "cow", "fish", and "mouse" are appropriate to these metagenomes.
# Such descriptive names are helpful, but not required.
# The object "guts" predefines a set of ids for testing purposes:

guts
cc <- collection (guts)

# Now we show some of the functions that are useful for collections.

# Show the names and ids of constituent metagenomes:
samples (cc)

# Show just the names of metagenomes:
names (cc)

# Show the matrix views that the collection contains:
views (cc)

# Or just the names of the views:
viewnames (cc)

# Just typing the collection name gives a summary of what it contains:
cc

# It also contains metadata:
metadata (cc)

# Matrix views are accessed with "$".   For example,
# this shows raw abundance counts of functional annotations
# at level 3 of the Subsystems hierarchy:
cc$count

# And this shows the same data, but normalized to lie between 0 and 1:
cc$normed

# Generally, the purpose of different matrix views is to show aspects 
# of the same selection of metagenomes.

# When you make a collection, you can specify the views that you want.
# For instance, here we make a new collection with views for different
# levels of the subsystems hierarchy:
ff <- collection (guts, FL1 = c (level = "level1"), FL2 = c (level = "level2"), FL3 = c (level = "level3"))

# You can assign new names to the metagenomes of a collection:
names (cc) <- c ("cow1", "cow2", "cow3", "fish1", "fish2", "mouse1", "mouse2")

# And also assign a grouping, to be used for certain analyses:
groups (cc) <- c (1,1,1,2,2,3,3)

# Finally, you can add a view to an existing collection in this way:
ff$COG <- c (level = "level2", source = "COG")
views (ff)

# In that example, the parameter "source" restricts the view to include
# only annotations from the named database, "COG".

