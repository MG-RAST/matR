
cc <- collection (guts, L1 = c (entry="count", level = "level1"), L2 = c (entry="count", level = "level2"))
views (cc)

cc$L1N <- c (entry = "normed", level = "level1")
cc$L2N <- c (entry = "normed", level = "level2")

d <- as.data.frame (t (cc$L1N))
r <- reshape (d, vary = list (names (d)), dir = "long", ids = rownames (d), times = names(d))
names (r) <- c ("function", "count", "sample")
rownames (r) <- NULL
r <- r[c (3,1,2)]
r

library (ggplot2)
ggplot (r) + geom_bar (aes (x = sample, y = count, fill = `function`), position = "fill", stat = "identity") + scale_x_discrete (name = "") + scale_y_continuous ("log-normalized proportional abundance") + opts (title = "Function Representation in Gut Metagenomes", axis.text.x = theme_text (angle = 90, hjust = 1))
