
##  what are people sharing in MG-RAST ?

dir.MGRAST (75, len=25)

search.MGRAST (metadata="antarctica")

##  we could have searched by functional or taxonomic annotation, as well

search.MGRAST (metadata="antarctica", detail=TRUE) [ , c("name","project1")]

xx <- biomRequest('mgp10307', request='organism', source='Greengenes')

colnames(xx)

rownames(xx)

t_xx <- transform (xx, t_Log)

columns (t_xx, "biome")

princomp (t_xx, map = c(col="biome"), label.cex=0.5)
