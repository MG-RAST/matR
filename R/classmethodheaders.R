
###
### This package defines methods for the following functions,
### and additionally for "show" which is already generic.
### 
### We also define certain S3 methods
###

setGeneric ("print")
setGeneric ("summary")
# setGeneric ("plot")
setGeneric ("samples", function (x, ...) standardGeneric ("samples"))
setGeneric ("names")
setGeneric ("names<-")
setGeneric ("groups", function (x, ...) standardGeneric ("groups"))
setGeneric ("groups<-", function (x, value) standardGeneric ("groups<-"))

setGeneric ("metadata", function (x, ...) standardGeneric ("metadata"))

setGeneric ("views", function (x, ...) standardGeneric ("views"))
setGeneric ("viewnames", function (x, ...) standardGeneric ("viewnames"))
setGeneric ("viewnames<-", function (x, value) standardGeneric ("viewnames<-"))

# did I do it right?  I think so, here, but check the handling
# of calls to rownames, dist, and heatmap, that are not meant for us
setGeneric ("rownames", function (x, ...) standardGeneric ("rownames"))

setGeneric ("selection", function (x, ...) standardGeneric ("selection"))
setGeneric ("collection", function (x, ...) standardGeneric ("collection"))

### Defining generics for common functions has to be done right,
### so we don't produce weird effects in users' environments

setGeneric ("dist", function (x, ...) standardGeneric ("dist"))
setGeneric ("pco", function (x, ...) standardGeneric ("pco"))
setGeneric ("heatmap", function (x, ...) standardGeneric ("heatmap"))
setGeneric ("sigtest", function (x, ...) standardGeneric ("sigtest"))

setGeneric ("render", function (x, ...) standardGeneric ("render"))

