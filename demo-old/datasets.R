
# BUILD THE EXAMPLE DATASETS INCLUDED WITH matR
# ...mgp757 was a good set but no longer available

views <- list(L1=view.defaults$raw, L2=view.defaults$raw, L3=view.defaults$raw)
views$L1[["level"]] <- "level1"
views$L2[["level"]] <- "level2"
views$L3[["level"]] <- "level3"

Coral <- collection (coral, views)
Guts <- collection (guts, views)
Marine <- collection (marine, views)
Mat <- collection (mat, views)
Waters <- collection (waters, views)
Whalebone <- collection (whalebone, views)
while (sapply (list (Whalebone, Mat, Marine, Coral, Guts, Waters), function (x) try (x$raw))) ...
save (Coral, Guts, Marine, Mat, Waters, Whalebone, file="matR-example-collections.rda", compress="xz")
