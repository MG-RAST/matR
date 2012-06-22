
############################################
### this stuff is unused.
### it belongs to an older idea about how to build the local objects.
### the notions about how to display and summarize the various types of
### metadata are still good and should be reimplemented.
############################################

### metadata of different types
# setClass ("mProjectMeta", representation (ID = "character"), contains = "list")
# setClass ("mSampleMeta", representation (ID = "character"), contains = "list")
# setClass ("mMetagenomeMeta", representation (ID = "character"), contains = "list")

############################################
### simple stuff to print important 
### metadata cleanly, and to gracefully 
### evolve in the long-term with changes
### to the metadata hierarchy
###
### ... the right way is not to print but to return a printable object, so that needs to be fixed ...
### ... and do not assume people are using fixed width fonts ...
############################################

print.mProjectMeta <- function (x, ...) {
# like many things we are dealing with, metadata formats are in flux.
# so here, a bit of a hack .. just to print long lists of samples, 
# metagenomes, and libraries more nicely.
	for (e in names (x)) if (class (x [[e]]) == "character" && length (x [[e]]) > 1 && is.null (names (x [[e]])))
		x [[e]] <- paste (x [[e]], collapse = " ")
	items <- unlist (x)
	twoColPrint (items, "left", "right")
	cat (paste ("  <project: ", x@ID, ">\n", sep = ""))
	}

summary.mProjectMeta <- function (object, ...) {
	items <- c (
		object$metadata [c ("project_name", "PI_firstname", "PI_lastname", "PI_email", "PI_organization", 
							"PI_organization_address", "PI_organization_country", "PI_organization_url", "project_description")],
# same hack here (next two lines)
		samples = paste (object$samples, collapse = " "),
		analyzed = paste (object$analyzed, collapse = " "))
	twoColPrint (items, "left", "right")
	cat (paste ("  <metadata summary of project: ", object@ID, ">\n", sep = ""))
	}

print.mSampleMeta <- function (x, ...) {
# same hack here
	for (e in names (x)) if (class (x [[e]]) == "character" && length (x [[e]]) > 1 && is.null (names (x [[e]])))
		x [[e]] <- paste (x [[e]], collapse = " ")
	items <- unlist (x)
	twoColPrint (items, "left", "right")
	cat (paste ("  <sample: ", x@ID, ">\n", sep = ""))
	}

summary.mSampleMeta <- function (object, ...) {
# same hack here
	for (e in names (object)) if (class (object [[e]]) == "character" && length (object [[e]]) > 1 && is.null (names (object [[e]])))
		object [[e]] <- paste (object [[e]], collapse = " ")
	items <- c (
		project = object$project,
		metagenomes = object$metagenomes,
		object$metadata [c ("seq_method", "seq_center")])
	twoColPrint (items, "left", "right")
	cat (paste ("  <metadata summary of sample: ", object@ID, ">\n", sep = ""))
	}

# simply unlisting metagenome metadata gives a nicely printing representation
print.mMetagenomeMeta <- function (x, ...) {
	items <- unlist (x)
	twoColPrint (items, "left", "right")
	cat (paste ("  <metagenome: ", x@ID, ">\n", sep = ""))
	}

# the summary of a metagenome needs somewhat more information than 
# metadata summaries of other resources
summary.mMetagenomeMeta <- function (object, ...) {
	items <- c (
		object$metadata$project$data [c ("project_name", "PI_lastname", "PI_organization", "PI_organization_country", "project_description")],
		project_id = object$metadata$project$id,
		sample_name = object$metadata$sample$name, 
		object$metadata$sample$data [c ("location", "country", "isolation_desc")],
		sample_id = object$metadata$sample$id)
	twoColPrint (items, "left", "right")
	cat (paste ("  <metadata summary of metagenome: ", object@ID, ">\n", sep = ""))
	}

### the mmatrix class exists in the new implementation, so needs to blanked out here
# ... it would be nice to have ellipsis indicating missing columns ...
#print.mmatrix <- function (x, ...) {
#		m <- min (nrow (x@data), 15)
#		n <- min (ncol (x@data), 5)
#		rownames (x@data) <- abbrev (rownames (x@data), getOption ("width") / 3, "middle")
#		print (x@data [1:m, 1:n])
#		cat ("  <truncated from", nrow (x@data), "rows and", ncol (x@data), "columns>\n")
#	}

# ... we also want per column: mean and number of nonzero entries ...
#summary.mmatrix <-	function (object, ...) {
#		cat ("  <abundance matrix with", dim (object@data) [1], "rows and", dim (object@data) [2], "columns>\n")
#		cat (paste ("  <metagenomes: ", paste (colnames (object@data), collapse = " "), ">\n", sep = ""))
#		print (data.frame (mean = format (colMeans (object), digits = 1), nonzero = "...", row.names = colnames (object)))
#	}
