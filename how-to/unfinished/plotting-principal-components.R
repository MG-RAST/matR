### Name: princomp.biom
### Title: Compute and plot principal coordinates of BIOM data
### Aliases: princomp princomp.biom

### ** Examples

####  quick two or three dimensional plots with choice of dissimilarity measure
princomp (xx1)
princomp (xx1, dim=2:3, method="bray-curtis")

####  graphical tweaks incorporating metadata
columns (xx1, "host_common_name|samp_store_temp")
princomp (xx1, dim=1:2, map=c(col="host_common_name", pch="samp_store_temp"),
  col=c(Mouse="brown", cow="red", "striped bass"="blue"),
  pch=c("-80"="+","NA"="*"), cex=2, label.pos=c(4,4,2,2,2,2,4), label.font=3)

####  transformed data, labeling from metadata, and modified perspective
columns (xx2, "material")
princomp (transform (xx2, t_Log), map=c(col="material"), labels="$$project.id", 
  angle=50, mar=c(1,1,0,0))



