
### This package defines methods for the following functions,
### and additionally for "show" which is already generic.
### 
### We also define certain S3 methods (maybe?)

setGeneric ("print")
setGeneric ("summary")
setGeneric ("plot")
setGeneric ("names")
setGeneric ("names<-")
setGeneric ("samples", function (x, ...) standardGeneric ("samples"))
setGeneric ("groups", function (x, ...) standardGeneric ("groups"))
setGeneric ("groups<-", function (x, value) standardGeneric ("groups<-"))
setGeneric ("metadata", function (x, ...) standardGeneric ("metadata"))

setGeneric ("views", function (x, ...) standardGeneric ("views"))
setGeneric ("viewnames", function (x, ...) standardGeneric ("viewnames"))
setGeneric ("viewnames<-", function (x, ...) standardGeneric ("viewnames<-"))

setGeneric ("mmatrix", function (x, ...) standardGeneric ("mmatrix"))
setGeneric ("selection", function (x, ...) standardGeneric ("selection"))
setGeneric ("collection", function (x, ...) standardGeneric ("collection"))
