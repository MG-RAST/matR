### Name: rowstats
### Title: Apply selected significance test across rows
### Aliases: rowstats rowstats.matrix rowstats.biom

### ** Examples

####  Kruskal test applied, for the case of more than two metagenome groups
columns (xx1, "host_common_name")
str (rowstats (xx1, groups="$$host_common_name", test="Kruskal"))

####  force a desired grouping of metagenomes
gg <- columns (xx2, "material") [[1]]
gg
levels (gg) <- levels (gg) [c(1,2,2)]
str (rowstats (xx2, groups=gg, test="t-test-unpaired"))



