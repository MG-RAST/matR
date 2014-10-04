#---------------------------------------------------------------------
#  Significance testing of rows (annotations)
#---------------------------------------------------------------------

rowstats <- function (x, ...) UseMethod("rowstats")

rowstats.biom <- function(
	x, groups, 
	test=c("Kruskal-Wallis", "t-test-paired", "Wilcoxon-paired", "t-test-unpaired", "Mann-Whitney-unpaired-Wilcoxon", "ANOVA-one-way"), 
	fdr.level = NULL, 
	qvalue=FALSE, ...) {

	rowstats (as.matrix (x), subMetColumns (groups, x), test, fdr.level, qvalue)
	}

rowstats.matrix <- function(
	x, groups, 
	test=c("Kruskal-Wallis", "t-test-paired", "Wilcoxon-paired", "t-test-unpaired", "Mann-Whitney-unpaired-Wilcoxon", "ANOVA-one-way"), 
	fdr.level = NULL, 
	qvalue=FALSE, ...) {

	test <- match.arg (test)
	groups <- as.factor (groups)
	fun <- switch (
		test,
		"t-test-unpaired" = function (x1, x2) t.test (x1, x2),
		"t-test-paired" = function (x1, x2) t.test (x1, x2, paired = TRUE),
		"Mann-Whitney-unpaired-Wilcoxon" = function (x1, x2) wilcox.test (x1, x2, exact = TRUE),
		"Wilcoxon-paired" = function (x1, x2) wilcox.test (x1, x2, exact = TRUE, paired = TRUE),
		"Kruskal-Wallis" = function (r) unlist (kruskal.test (r, groups) [c ("statistic", "p.value")], use.names = FALSE),
		"ANOVA-one-way" = function (r) {
			a <- anova (aov (r ~ groups)) [c ("F value", "Pr(>F)")]
			c (a ["F value"] [1,1], a ["Pr(>F)"] [1,1])
		})

	res <- list()
	res$samples <- colnames (x)
	res$groups <- groups
	res$mean <- t (apply (x, 1, function (row) tapply (row, groups, mean)))
	res$sd <- t (apply (x, 1, function (row) tapply (row, groups, sd)))

	stat <- as.data.frame (t (
		if (test %in% c ("Kruskal-Wallis", "ANOVA-one-way")) apply (x, 1, fun)
		else {
			g1 <- lapply (apply (x [ ,groups == levels (groups) [1]], 1, list), unlist)
			g2 <- lapply (apply (x [ ,groups == levels (groups) [2]], 1, list), unlist)
			mapply (function (x1, x2) unlist (fun (x1, x2) [c ("statistic", "p.value")], use.names = FALSE), g1, g2)
		}))
	names (stat) <- c ("statistic", "p.value")
	if (test != "ANOVA-one-way" && qvalue) {
		stat [c ("q.value", "significant")] <- qvalue::qvalue (stat$p.value, fdr.level = fdr.level) [c ("qvalues", "significant")]
	}
	append (res, stat)
	}
