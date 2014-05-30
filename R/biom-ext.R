add ncol, nrow


#---------------------------------------------------------------------
#  additional biom methods
#
#  rownames,colnames --- based on "id" field
#---------------------------------------------------------------------

rownames.biom		<-	function(xx) rownames(xx$data)
colnames.biom		<-	function(xx) colnames(xx$data)
`rownames<-.biom`		<-	function(x, value) { rownames(xx$data) <- xx ; ...}
`colnames<-.biom`		<-	function(x, value) { ... }

#---------------------------------------------------------------------
#  rows,columns --- based on "metadata" fields
#---------------------------------------------------------------------

rows <- function (bb, pattern="*") {
	ll <- lapply (bb$rows, function (rr) unlist(rr$metadata))
	ii <- lapply (ll, function (vv, p) grepl (p, names(vv)), pattern)
	xx <- mapply (`[`, ll, ii, SIMPLIFY=FALSE)
	ddl <- lapply (xx, data.frame)
	mrr <- function (a, b)  { dd <- merge(a, b, all=TRUE, by=0) ; rownames(dd) <- dd[,1] ; dd[,-1] }
	rrr <- t (Reduce (mrr, ddl))
	rownames(rrr) <- sapply(bb$rows, `[[`, "id")
	rrr
}

columns <- function (bb, pattern="*") {
	ll <- lapply (bb$columns, function (rr) unlist(rr$metadata))
	ii <- lapply (ll, function (vv, p) grepl (p, names(vv)), pattern)
	xx <- mapply (`[`, ll, ii, SIMPLIFY=FALSE)
	ddl <- lapply (xx, data.frame)
	mrr <- function (a, b)  { dd <- merge(a, b, all=TRUE, by=0) ; rownames(dd) <- dd[,1] ; dd[,-1] }
	rrr <- Reduce (mrr, ddl)
	colnames(rrr) <- sapply(bb$columns, `[[`, "id")
	rrr
}

`rows<-` function (x, i) { 
	ins <- function (rr, vv) { rr$metadata[[nn]] <- vv }
	x$rows <- mapply(ins, x$rows, i) 
}

`columns<-` function (x, i) { }

#---------------------------------------------------------------------
#  indexing
#---------------------------------------------------------------------

`[.biom` <- function ........
