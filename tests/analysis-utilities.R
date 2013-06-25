####################################################################################
### test suite for analysis routines that operate on plain matrices
####################################################################################

library (matR)

# an approximately realistic random matrix for testing
nr <- sample (100:200, 1) ; nc <- sample (10:15, 1)
m <- matrix (sample (100000, nr*nc, repl = TRUE), nr = nr, nc = nc)
m [sample (length (m), runif (1, min = 1, max = length (m)))] <- NA
m [sample (length (m), runif (1, min = 1, max = length (m)))] <- 0

# and matrices to test specific cases
n <- p <- q <- r <- s <- m
n [sample (nrow(n), 1), ] <- 0
p [ ,sample (ncol(p), 1)] <- 0
q <- q [sample (nrow(q), 1), , drop = FALSE]
r <- r [ ,sample (ncol (r), 1), drop = FALSE]
s [is.na(s)] <- 0
rownames (m) <- paste("R", 1:nrow(m), sep = "")
colnames (m) <- paste("C", 1:ncol(m), sep = "")
rownames (n) <- paste("R", 1:nrow(n), sep = "")
colnames (n) <- paste("C", 1:ncol(n), sep = "")
rownames (p) <- paste("R", 1:nrow(p), sep = "")
colnames (p) <- paste("C", 1:ncol(p), sep = "")
rownames (q) <- paste("R", 1:nrow(q), sep = "")
colnames (q) <- paste("C", 1:ncol(q), sep = "")
rownames (r) <- paste("R", 1:nrow(r), sep = "")
colnames (r) <- paste("C", 1:ncol(r), sep = "")
rownames (s) <- paste("R", 1:nrow(s), sep = "")
colnames (s) <- paste("C", 1:ncol(s), sep = "")

####################################################################################
### test: dist()
####################################################################################
# standard distance computation on a matrix
dist(s)
dist(s, bycol = TRUE)
dist(s, method = "bray-curtis")
dist(s, method = "bray-curtis", bycol = TRUE)

row.groups <- c (rep ("a", nrow (s) / 3),
								 rep ("b", nrow (s) / 3),
								 rep ("c", nrow (s) - 2 * trunc (nrow (s) / 3)))
col.groups <- c (rep ("a", ncol (s) / 3),
								 rep ("b", ncol (s) / 3),
								 rep ("c", ncol (s) - 2 * trunc (ncol (s) / 3)))
row.groups.with.single <- row.groups
col.groups.with.single <- col.groups
row.groups.with.single [length (row.groups.with.single)] <- "d"
col.groups.with.single [length (col.groups.with.single)] <- "d"

# with groups specified, compute mean pairwise intra- and inter-group distances
# dist(s, groups = row.groups)                                      !!!!
# dist(s, groups = col.groups, bycol = TRUE)                        !!!!
# dist(s, groups = row.groups.with.single)                          !!!!
# dist(s, groups = col.groups.with.single, bycol = TRUE)            !!!!

y = sample (ncol (s))
z = sample (nrow (s))

# with vector specified, compute distance from it to each row or column
dist(s, y)
dist(s, z, bycol = TRUE)

# with vector and groups specified, compute distance from vector to each group
dist(s, y, groups = row.groups)
dist(s, z, groups = col.groups, bycol = TRUE)
dist(s, y, groups = row.groups.with.single)
dist(s, z, groups = col.groups.with.single, bycol = TRUE)

####################################################################################
### test: remove.singletons()
####################################################################################
remove.singletons (m)
remove.singletons (n)
remove.singletons (p)
remove.singletons (q)
remove.singletons (r)

####################################################################################
### test: normalize()
####################################################################################
normalize (m)
normalize (n)
normalize (p)
# normalize (q)           !!!!
normalize (r)

####################################################################################
### test: randomize()
####################################################################################
randomize (m, n = 5)
randomize (m, n = 10, method = "sample")
# randomize (m, n = 10, method = "rowwise", FUN = mean)
randomize (m, n = 10, method = "dataset", FUN = colSums, na.rm = TRUE)
# randomize (m, n = 10, method = "complete", FUN = function (m) apply (m, MARGIN = 2, hist, plot = FALSE))      !!!!

####################################################################################
### test: sigtest()
####################################################################################

sigtest (m, groups = col.groups, test = "Kruskal-Wallis")
# unpaired tests
sigtest (m, test = "t-test-unpaired")
sigtest (m, test = "Mann-Whitney-unpaired-Wilcoxon")

# paired tests
sigtest (Waters, test = "t-test-paired")
sigtest (Waters, test = "Wilcoxon-paired")

# ANOVA
groups (m) <- c (1,1,1,2,2,3,3)
sigtest (m,  test = "ANOVA-one-way")

# qvalue and FDR testing
# ...