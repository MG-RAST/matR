
# BUILD THE EXAMPLE DATASETS INCLUDED WITH matR

Whalebone <- collection (unlist (mGet ("project", "mgp9", enClass = FALSE) $ analyzed))

Mat <- collection (unlist (mGet ("project", "mgp12", enClass = FALSE) $ analyzed))

Marine <- collection (unlist (mGet ("project", "mgp18", enClass = FALSE) $ analyzed))

Coral <- collection (unlist (mGet ("project", "mgp16", enClass = FALSE) $ analyzed))	     # no longer available

# Hospital <- collection (unlist (mGet ("project", "mgp757", enClass = FALSE) $ analyzed))     # no longer available

guts <- c (cow.rumen.1 = "4441679.3", cow.rumen.2 = "4441680.3", cow.rumen.3 = "4441682.3", fish.gut.1 = "4441695.3", fish.gut.2 = "4441696.3", mouse.lean = "4440463.3", mouse.obese = "4440464.3")
Guts <- collection (guts)

waters <- c ("4440424.3", "4440423.3", "4440439.3", "4440422.3", "4440412.3", "4440414.3", "4440440.3", "4440413.3", "4440411.3", "4443681.3", "4443682.3", "4443684.3", "4443679.3", "4443683.3", "4443680.3", "4441096.3", "4442583.3", "4441095.3", "4443750.3", "4443762.3", "4443749.3", "4443746.3", "4443747.3", "4443745.3")
names (waters) <- c (paste ("fresh", 1:15, sep = ""), paste ("spring", 1:9, sep = ""))
Waters <- collection (waters)

# invisible (sapply (list (Whalebone, Mat, Marine, Coral, Guts, Waters), function (x) try (x$raw)))
# save (Whalebone, Mat, Marine, Coral, guts, Guts, waters, Waters, file = "~/Desktop/ANL/matR/data/collection-examples.rda", compress = "xz")
