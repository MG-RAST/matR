# HOW TO WORK WITH METAGENOME COLLECTIONS (press return repeatedly to step through)

# You need to have ids for the metagenomes you're interested in.
# Find them through the MG-RAST website.  If there are many, consider using
# read.table() or some similar function to read them from a text file.

# To create a collection in your session, do something like this
# (and expect the command to take some time to complete):

ids <- c (cow = "4441679.3", fish = "4441695.3", mouse = "4440463.3")
cc <- collection (ids)

# The names "cow", "fish", and "mouse" are appropriate to these metagenomes.
# Such descriptive names are helpful, but not required, and this simple syntax works too:

cc <- collection ("4441679.3 4441695.3 4440463.3")

# The object "guts" predefines a set of ids for testing purposes:

guts
cc <- collection (guts)

# Now we show some of the functions that are useful for collections.

# Show metadata related to the collection:
metadata (cc)

# Show the names of constituent metagenomes:
names (cc)

# Show the names and ids of constituent metagenomes:
selection (cc)

# Show the matrix views that the collection contains:
views (cc)

# Or just the names of the views:
viewnames (cc)

# Just typing the collection name gives a complete summary of what it contains:
cc

# Matrix views are accessed with "$" plus the name of the view.  For example,
# this shows raw abundance counts of functional annotations at level 3
# of the subsystems hierarchy:
cc$count

# And this one shows the same data, but normalized to lie between 0 and 1:
cc$normed

# But what are matrix views?  Generally, the purpose of matrix views is 
# to show different aspects of a selection of metagenomes.

# When you make a collection, you can specify the views that you want.
# For instance, here we make a new collection with one view for each
# level of the subsystems hierarchy:
ff <- collection (guts, FL1 = view (level = "level1"), FL2 = view (level = "level2"), FL3 = view (level = "level3"), FL4 = view (level = "function"))

# You can assign new names to the metagenomes of a collection:
names (cc) <- c ("cow1", "cow2", "cow3", "fish1", "fish2", "mouse1", "mouse2")

# And also assign a grouping, which will be used automatically for certain analyses:
groups (cc) <- c (1,1,1,2,2,3,3)

# Finally, you can add a view to an existing collection in this way:
ff$FL2bis <- view (level = "level2", source = "COG")

# In that example, the parameter "source" restricts the view to include
# only annotations from the named database, "COG".
