resolve <- function (first, second) {
#---------------------------------------------------------------
#  merge two named lists, eliminating duplicates and giving priority to the first
#---------------------------------------------------------------
	append (first, second) [ !duplicated (c (names (first), names(second))) ]
	}


parMapper <- function (object, name.map, value.map) {
#---------------------------------------------------------------------
#	produce par values given mappings.  input is:
#		xx			biom object (with metadata)
#		map			mapping of names of par to names of metadata
#		values		list _possibly_ including a specific mapping for each mapped par
#
#  the workhorse enabling user calls such as:
#
#  princomp(xx,
#    map = c (col="host_common_name", pch="samp_store_temp"),
#    col = c (Mouse="blue", cow="red", "striped bass"="brown"),
#    pch = c ("-80"="+", "NA"="x"))
#-----------------------------------------------------------------------------------------

#---------------------------------------------------------------------
	if (is.null (name.map)) return (list())

	pars <- names (name.map)
	metadata.values <- lapply (name.map, getMetColumns, object)

#  make NA any metadata that is "NA".

	metadata.values <- lapply (metadata.values,
		function (xx) {
			levels (xx) [levels (xx) == "NA"] <- NA
			xx
			})

#  create mappings where they are not specified at all.
#  in the mapping, the missing level must be identified by name "NA",
#  not by a missing name.

	automatic <- setdiff (pars, names (value.map))
	value.map [automatic] <- mapply(
		function (par, met) {
			met <- addNA (met, ifany=TRUE)
			yy <- parAuto (par, nlevels (met))
			names (yy) <- levels (met)
			names (yy) [is.na (names (yy))] <- "NA"
			yy
			},
		as.list (automatic),
		metadata.values [automatic],
		SIMPLIFY=FALSE)

	value.map <- value.map [pars]

#  make NA any unspecified levels of partially-specified metadata

	metadata.values <- mapply(
		function (xx, map) {
			levels (xx) [! (levels (xx) %in% names (map))] <- NA
			xx
			},
		metadata.values,
		value.map,
		SIMPLIFY=FALSE)

#  complete partial mappings by adding a value for NA, if necessary

	value.map <- mapply(
		function (map, par) {
			if (! ("NA" %in% names (map))) {
				c(map, "NA" = parAuto (par))
			} else map },
		value.map,
		names (value.map),
		SIMPLIFY=FALSE)

#  apply all mappings to create par specifications.
#  the missing level must be renamed to "NA",
#  in order to get the value intended for it.

	yy <- mapply(
		function (values, map) {
			values <- addNA(values,ifany=TRUE)
			yy <- levels (values)
			yy [is.na (yy)] <- "NA"
			levels (values) <- map [yy]
			if (is.numeric (map)) {						# ensure resulting par has the right class
				as.numeric (as.character (values))
			} else as.character (values)
			},
		metadata.values,
		value.map,
		SIMPLIFY=FALSE)
	}

#---------------------------------------------------------------------
#  facilitate:
#    (1) metadata mapping
#    (2) literal metadata substitutions
#---------------------------------------------------------------------

getMetColumns <- function (name, xx) {
	yy <- columns (xx, name)
	if (ncol (yy) != 1)
		stop ("\"", name, "\" does not identify a unique column metadata field")
	yy [[1]]
	}

getMetRows <- function (name, xx) {
	yy <- rows (xx, name)
	if (ncol (yy) != 1)
		stop ("\"", name, "\" does not identify a unique row metadata field")
	yy [[1]]
	}

subMetColumns <- function (name, xx) {
	if (length (name) != 1 ||
		!is.character (name) ||
		substr (name, 1, 2) != "$$") return (name)
	yy <- columns (xx, substring (name, 3))
	if (ncol (yy) != 1)
		stop ("\"", name, "\" does not identify a unique column metadata field")
	as.character (yy [[1]])
	}

subMetRows <- function (name, xx) {
	if (length (name) != 1 ||
		!is.character (name) ||
		substr (name, 1, 2) != "$$") return (name)
	yy <- rows (xx, substring (name, 3))
	if (ncol (yy) != 1)
		stop ("\"", name, "\" does not identify a unique row metadata field")
	as.character (yy [[1]])
	}
