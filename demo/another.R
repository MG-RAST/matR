Waters
samples (Waters)

## We look at a principal coordinates analysis with samples colored by group:

groups (Waters) <- c (rep (1, 15), rep (2, 9))
col <- groups (Waters)
levels (col) <- c ("blue", "red")
col <- as.character (col)
pco (Waters, comp = c (1,2), main = "fresh vs. spring water samples", col = col)

## We can also look at principal coordinates in three dimensions, or a heatmap:

pco (Waters, comp = c (1,2,3), main = "fresh vs. spring water samples", color = col)
heatmap (Waters)

## A statistical test such as Kruskal-Wallis can help identify the most significant rows (annotations) and sharpen the picture:

results <- sigtest (Waters, test = "Kruskal-Wallis")

## We can browse the test results:

head (results$sd)
head (results$mean)
head (results$stat)

## Using p-values, we check how many annotations are retained, at various significance levels:

sum (results$stat$p.value < 0.05)
sum (results$stat$p.value < 0.005)
sum (results$stat$p.value < 0.0005)

## It is easy to subselect, leaving abundance data for the eighteen functional annotations of highest significance in differentiating the two groups:

subm <- Waters$normed [results$stat$p.value < 0.0005, ]
dimnames (subm)
subm

## A heatmap restricted to these functions shows clear distinctions between fresh and spring water, but also a curious aberration in the ninth spring water sample:

heatmap (Waters, rows = (results$stat$p.value < 0.0005))

## We can scrutinize available metadata for an explanation, and after noticing the various projects contained in the collection, it seems natural to compare sample environments:

metadata (Waters) ["project.name"]
metadata (Waters) ["4443745.3"]
metadata (Waters) ["env_package"]
metadata (Waters) ["sample.data"]
metadata (Waters) ["temperature"]

## Nothing turns up, so we turn to library information:

metadata (Waters) ["library"]

## There, we discover that the ninth spring water sample, unlike every other in the collection and even in its own project, underwent Amplicon sequencing:

metadata (Waters) ["library.type"]

## At this point we may wish to continue analysis with subcollections, using WGS metagenomes, only:

Waters2 <- Waters [1:23]

## Or we may, to isolate a comparison of different sequencing technologies, select metagenomes only from the project that includes the Amplicon metagenome:

Waters3 <- Waters [19:24]

## For instance, we can redo the eighteen-row heatmap from above, with the Amplicon metagenome omitted:

groups (Waters2) <- c (rep (1, 15), rep (2, 8))
results2 <- sigtest (Waters2, test = "Kruskal-Wallis")
heatmap (Waters2, rows = (results2$stat$p.value < 0.0005))

## Significance testing could also be redone with annotations from a higher (or lower) level of the Subsystems hierarchy:

Waters$lev2 <- c (entry = "normed", level = "level2")
results.lev2 <- sigtest (Waters, view = "lev2", test = "Kruskal-Wallis")
heatmap (Waters, view = "lev2", rows = (results.lev2$stat$p.value < 0.005))
rownames (Waters$lev2) [results.lev2$stat$p.value < 0.005]
