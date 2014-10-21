###  what are people sharing in MG-RAST ?

dir.MGRAST (75, len=25)

search.MGRAST (metadata="antarctica")

###  we could have searched by functional or taxonomic annotation, as well.

search.MGRAST (metadata="antarctica", detail=TRUE) [ , c("name","project1")]

###  here is how to retrieve and inspect some data for one of the projects.

xx <- biomRequest('mgp10307', request='organism', source='Greengenes')

colnames(xx)

rownames(xx)

###  and now we can study it in detail.

t_xx <- transform (xx, t_Log)

columns (t_xx, "biome")

princomp (t_xx, map = c(col="biome"), label.cex=0.5)
