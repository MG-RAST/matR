
normalize <- function (M) {
	M [is.na (M)] <- 0
# log scale
	M <- log2 (M + 1)
# then scale by mean and standard deviation per sample
	mu <- colMeans (M)
	sigm <- unlist (sapply (as.data.frame (M), sd))
	M <- t ((t (M) - mu) / sigm)
# then scale to [0,1] across all samples
	shift <- min (M)
	scale <- max (M) - shift
	if (scale != 0) M <- (M - shift) / scale
	M
	}
