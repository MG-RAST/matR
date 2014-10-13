#---------------------------------------------------------------------
#  Data transformations:
#    matrix transformations:  set of primitives
#    biom transformations:  function transform() as common interface
#---------------------------------------------------------------------

transform.biom <- function (`_data`, ...) {
	xx <- `_data`
	li <- list (...)
	if (is.null (names (li))) {
		f.list <- li
		a.list <- replicate (length (li), NULL)
	} else {
		f.list <- a.list <- list()
		length (f.list) <- length (a.list) <- length (li)

		f.list [names (li) == ""] <- li [names (li) == ""]
		a.list [names (li) == ""] <- replicate (sum (names (li) == ""), NULL)

		f.list [names (li) != ""] <- mget (names (li) [names (li) != ""], inherits=TRUE)
		a.list [names (li) != ""] <- li [names (li) != ""]
		}

	ll <- append(										#	this is:  list(x, list(list of functions, list of argument lists))
			list (as.matrix (xx, expand=TRUE)),
			mapply (list, f.list, a.list, SIMPLIFY=FALSE))
	pass.to <- function (x, funcWithArgs) {
		do.call (funcWithArgs [[1]], append (list (x), funcWithArgs [[2]]))				# some issue; args being passed in a list capsule should not be
		}
	data1 <- Reduce (pass.to, ll)

	y <- xx [rownames(xx) %in% rownames (data1), colnames (xx) %in% colnames (data1)]
	y$data <- data1
	y$sparse <- NULL
	y$generated_by <- tagline()
	y$date <- strftime(Sys.time())
	y$id <- paste0 ("derived with ", deparse (match.call(), width=500))
	y
	}

t_NA2Zero <- function (x, ...) {
	x [is.na (x)] <- 0;
	x
	}
t_Threshold <- function (x, entry.min=2, row.min=2, col.min=2) {
	x [x < entry.min] <- 0
	x <- x [rowSums (x) >= row.min, ]
	x <- x [, colSums (x) >= col.min]
	x
	}
t_Log <- function (x, ...) {
	log2 (1 + x)
	}
t_ColCenter <- function (x, ...) {
	x - colMeans(x) [col(x)]
	}
t_ColScale <- function (x, ...) {
	sigma <- apply (x, 2, sd)
	sigma [sigma == 0] <- 1
	x / sigma [col(x)]
	}
t_DENorm <- function (x, DEparam, ...) { x }
