sampleSets()
lapply (sampleSets(), readSet)
lapply (lapply (sampleSets(), readSet), scrubSet)
lapply (lapply (sampleSets(), readSet), scrapeSet)


scrubSet
scrapeSet
readSet
expandSet
metadata( ..project.. )


expandSet('mgp9')
expandSet('mgp24')
expandSet('mgp24 mgp9')
expandSet('mgp24 mgp9 mgm4440463.3')
expandSet('mgp24 mgm4440463.3 mgp9')
expandSet('mgm4440463.3 mgp24 mgp9')
expandSet('mgm4440463.3')

df <- readSet(sampleSets()[6])
df
expandSet (df)

df ['mgm4440463.3',] <- 'foo'
df
expandSet (df)

xx <- readSet(sampleSets()[7])
xx
expandSet (xx)

li <- list (
	"mgm4440066.3",
	"mgm4440066.3 mgm4440062.3",
	"mgm4440066.3 mgm4440062.3 mgm4440055.3",

	"mgm4440066.3",
	"mgm4440066.3\tmgm4440062.3",
	"mgm4440066.3\tmgm4440062.3 mgm4440055.3",

	"mgm4440066.3",
	"mgm4440066.3\t mgm4440062.3",
	"mgm4440066.3\t mgm4440062.3\t mgm4440055.3",

	"mgm4440066.3",
	"mgm4440066.3\nmgm4440062.3",
	"mgm4440066.3\nmgm4440062.3\nmgm4440055.3",

	"4440066.3",
	"4440066.3 4440062.3",
	"4440066.3 4440062.3 4440055.3",

	c("mgm4440066.3"),
	c("mgm4440066.3", "mgm4440062.3"),
	c("mgm4440066.3", "mgm4440062.3", "mgm4440055.3"),

	c(4440066.3),
	c(4440066.3, 4440062.3),
	c(4440066.3, 4440062.3, 4440055.3),

	c("mgm4440066.3 mgm4440062.3", "mgm4440055.3"),
	c("4440066.3 mgm4440062.3", "mgm4440055.3"),
	c("mgm4440066.3 4440062.3", "mgm4440055.3"),
	c("mgm4440066.3 mgm4440062.3", "4440055.3"),
	c("mgm4440066.3 4440062.3", "4440055.3"),
	c("4440066.3 mgm4440062.3", "4440055.3"),
	c("4440066.3 4440062.3", "mgm4440055.3"),

	"mgp21",
	"mgp21 mgp24",
	"mgp21 mgp24 mgp30",

	c("mgp21"),
	c("mgp21", "mgp24"),
	c("mgp21", "mgp24", "mgp30"))

lapply (li, scrubSet)
