
#---------------------------------------------------------------------
#  biom -> analysis and/or visualization
#---------------------------------------------------------------------

boxplot.biom <- (x, ...) {
	yy <- boxplot(as.matrix(x), ...)
	yy 
	}


pco <- function (x, ..., method="Bray-Curtis", components=c(1,2,3), rows=TRUE, cols=TRUE, rerender=NULL) {
	if(class(rerender)=="pco") {
		}
	if(class(rerender)=="dist") {
		}
	}


heatmap[.biom?] <- function (x, ..., rows=TRUE, cols=TRUE, rerender=NULL) {
	if(class(rerender)=="heatmap") {
		}
	require(gplots)
	yy <- heatmap.2(as.matrix(x) [rows,cols], ...)
	...modify yy
	yy
	}
