
view.params
standard.views
all.views
guts

### COLLECTION CONSTRUCTION

cc <- collection (guts)
dd <- collection (guts, func1 = c (entry="normed", lev="level1"), func2 = c (entry="normed", lev="level2"), 
									func3 = c (entry="normed", lev="level3"), func4 = c (entry="normed", lev="function"))

### COLLECTION MANIPULATION

cc
names (cc)
samples (cc)
views (cc)
viewnames (cc)
groups (cc)
metadata (cc)

views (cc)
viewnames (cc)
cc$org.count <- c (annot="organism", entry="count")
cc$org.normed <- c (annot="organism", entry="normed")
views (cc)
viewnames (cc) [1:2] <- c ("func.count", "func.normed")
viewnames (cc)

cc$func.count
cc$func.normed
cc$org.count
cc$org.normed

groups (cc)
groups (cc) <- c (1,1,1,2,2,3,3)
groups (cc)
samples (cc)

cc.sub <- cc [1:3]
names (cc.sub)
samples (cc.sub)
views (cc.sub)
viewnames (cc.sub)
groups (cc.sub)
metadata (cc.sub)

### METADATA

mm <- metadata (cc)
summary (mm)
mm["0464"]
mm["body_product"]
mm["latitude", "longitude"]
mm["latitude", "longitude", bygroup = TRUE]
mm[c("0464", "env_package.data")]
mm[c("0464", "PI"), c("0464","seq_"), c("0464","biome")]
mm["disease"]
mm[".age"]
mm["host_common_name"]
mm["host_common_name", "disease", ".age", bygroup = TRUE]

### ANALYSIS

viewnames (cc) [1:2] <- c ("count", "normed")

heatmap (cc)
heatmap (cc, main = "My Title", col = c ("blue", "green"), cexCol = 0.8)
nrow (cc$normed)
heatmap (cc, rows = c (rep (TRUE, 100), rep (FALSE, 915)))
heatmap (cc, rows = c (rep (TRUE, 50), rep (FALSE, 965)))

pco (cc)
groups (cc) <- c (1,1,1,2,2,3,3)
pco (cc, view = "org.normed")
pco (cc, comp = c (1,2))
pco (cc, main = "My Title", pch = 19:25, color=c(rep("blue",5),rep("red",2)))

render(cc)
render(cc, "count", las=3, cex.axis=.7, ylab = "my axis title")
