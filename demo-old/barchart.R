
cc <- collection (guts, L1 = c (entry="counts", annot="function", level = "level1"), L2 = c (entry="counts", annot="function", level = "level2"))
views (cc)

cc$L1N <- c (entry = "normed.counts", annot="function", level = "level1")
cc$L2N <- c (entry = "normed.counts", annot="function", level = "level2")

d <- as.data.frame (t (cc$L1N))
r <- reshape (d, vary = list (names (d)), dir = "long", ids = rownames (d), times = names(d))
names (r) <- c ("function", "count", "sample")
rownames (r) <- NULL
r <- r[c (3,1,2)]
r

library (ggplot2)
ggplot (r) + geom_bar (aes (x = sample, y = count, fill = `function`), position = "fill", stat = "identity") + scale_x_discrete (name = "") + scale_y_continuous ("log-normalized proportional abundance") + labs (title = "Function Representation in Gut Metagenomes") + theme (axis.text.x = element_text (angle = 90, hjust = 1))
