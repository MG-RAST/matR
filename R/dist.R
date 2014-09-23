
#---------------------------------------------------------------------
#  Distance, with expanded functionality
#
#---------------------------------------------------------------------

distx <- function (x, ...) UseMethod ("distx")

distx.biom <- function (
	x, y=NULL, 
	groups=NULL, 
	method=c("euclidean", "bray-curtis", "jaccard", "mahalanobis", "sorensen", "difference", "maximum", "manhattan", "canberra", "binary", "minkowski"),
	..., 
	bycol=TRUE) {

	distx (as.matrix (x, expand=TRUE), y, subMetColumns (groups, x), match.arg (method), ..., bycol)
	}

distx.matrix <- function(
	x, y=NULL,
	groups=NULL, 
	method=c("euclidean", "bray-curtis", "jaccard", "mahalanobis", "sorensen", "difference", "maximum", "manhattan", "canberra", "binary", "minkowski"),
	..., 
	bycol=TRUE) {

	method <- match.arg (method)
	if (bycol) x <- t(x)
	dist.fun <- if (method %in% c ("bray-curtis", "jaccard", "mahalanobis", "sorensen", "difference")) {
		ecodist::distance
	} else stats::dist

	if (!is.null (y)) {
		dist2y <- apply (x, 1, 
				function (r, y, m) dist.fun (rbind (r, y), m),
				y, method)
		return(
			if (is.null (groups)) {
				dist2y
			} else tapply (dist2y, groups, mean))
		}

	if (is.null (groups)) return (dist.fun (x, method))

	groups <- as.factor (groups)
	D <- dist.fun (x, method)
	from <- unlist (sapply (2:nrow(x), seq, to=nrow(x)))
	to <- unlist (mapply (rep, 1 : (nrow(x)-1), (nrow(x)-1) : 1))
	zz <- tapply (D, list (groups [from], groups [to]), mean)

#---------------------------------------------------------------------
#  make symmetric by replacing NA's across the diagonal
#
#---------------------------------------------------------------------
	dd <- diag (zz)
	zz [is.na (zz)] <- 0
	zz <- zz + t(zz)
	diag (zz) <- dd
	zz
	}
