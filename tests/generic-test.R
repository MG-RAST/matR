library(matR)

#-----------------------------------------------------------------------------------------
#  general-purpose testing pipeline for:
#    projects or metagenome sets
#    biom objects
#-----------------------------------------------------------------------------------------

jj <- ""														# set jj to mgp id or mgm ids
args <- NULL

xx <- do.call (biomRequest, matR:::resolve (args, list (		# or just set xx to a biom object
	x = jj,
	request = "function")))

rows (xx)
names (rows (xx))
columns (xx)
names (columns (xx))
rows(xx, "junk") <- 1:nrow(xx)
columns(xx, "junk") <- 1:ncol(xx)
xx [1,]
xx [,1]
xx [1:2,]
xx [,1:2]
rownames (xx) <- 1:nrow(xx)										# does not work
colnames (xx) <- 1:ncol(xx)										# does not work

distx (xx)
distx (xx, groups = 1:ncol(xx) %% 3)

transform (xx, t_NA2Zero)
transform (xx, t_NA2Zero, t_Threshold)
transform (xx, t_NA2Zero, t_Threshold = list(...))
transform (xx, t_NA2Zero, t_Threshold = list(...))
transform (xx, t_NA2Zero, t_Threshold = list(...))
transform (xx, t_NA2Zero, t_Threshold = list(...))
transform (xx, t_NA2Zero, t_Threshold = list(...), t_Log)
transform (xx, t_NA2Zero, t_Threshold = list(...), t_Log, t_ColCenter)
yy <- transform (xx, t_NA2Zero, t_Threshold = list(...), t_Log, t_ColCenter,  t_ColScale)

rowstats (yy, test="t-test-paired", groups = 1:ncol(xx) %% 3)
rowstats (yy, test="Wilcoxon-paired", groups = 1:ncol(xx) %% 3)
rowstats (yy, test="t-test-unpaired", groups = 1:ncol(xx) %% 3)
rowstats (yy, test="Mann-Whitney-unpaired-Wilcoxon", groups = 1:ncol(xx) %% 3)
rowstats (yy, test="Kruskal-Wallis", groups = 1:ncol(xx) %% 3)
rowstats (yy, test="ANOVA-one-way", groups = 1:ncol(xx) %% 3)

boxplot (xx, main="title")
boxplot (xx, yy, x.main="x title", y.main="y title")

princomp (xx, dim=1)
princomp (xx, dim=1:2)
princomp (xx, dim=1:3)

princomp (yy, dim=1)
princomp (yy, dim=1:2)
princomp (yy, dim=1:3)

j <- rowstats (yy, test="Kruskal-Wallis", groups = 1:ncol(xx) %% 3) $ p.value < 0.5
image (xx)
image (xx [j,])

image (yy)
image (yy [j,])
