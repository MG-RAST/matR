
#######################################################################
## put pco through the paces
#######################################################################
pp <- pco(xx)
pco(main="with title", rerender=pp)
pco(xx, method="euclidean")
pco(xx, dim=c(2,3))
pco(xx, dim=c(2,3,4))
pco(xx, rows = 1:10)
pco(xx, columns = c(1,2,4))

# pco(xx, <missing col --> automatic>)
pco(xx, col="blue")
pco(xx, col=c("blue","blue","blue","red","red","red","red"))
pco(xx, col="$body_product")											# check and replace with sth appropriate
pco(xx, col=columns(xx, "body_product"))								# check and replace with sth appropriate

# pco(xx, <missing labels --> none)
pco(xx, labels=letters[1:7])
pco(xx, labels="$group")
pco(xx, labels=columns(xx, "group"))

# pco(xx, <missing pch --> automatic)
pco(xx, pch=c('a','b','c','d','e','f','g')
pco(xx, pch="$group")
pco(xx, pch=columns(xx, "group"))

# the easy expression we've wanted all along:
PP <- pco(xx, labels="$metagenome_id", col="$body_product", pch="$group")
pco(main="easy!", rerender=PP)


#######################################################################
## put heatmap through the paces
#######################################################################
hh <- heatmap(xx)
heatmap(main="with title", rerender=hh)
heatmap(xx, rows=1:20)
heatmap(xx, columns=c(1,2,4)) 

heatmap(xx, labCol=letters[1:7])
heatmap(xx, labCol="$group")
heatmap(xx, labCol=columns(xx, "group"))

heatmap(xx, labRow=1:nrow(xx))
heatmap(xx, labRow="$phylum")
heatmap(xx, labRow=rows(xx, "phylum"))
