\name{coerce-methods}
\docType{methods}
\alias{coerce-methods}
\alias{coerce,biom,collection-method}
\alias{coerce,biom,list-method}
\alias{coerce,biom,matrix-method}
\alias{coerce,character,biom-method}
\alias{coerce,character,collection-method}
\alias{coerce,collection,biom-method}
\alias{coerce,collection,matrix-method}
\alias{coerce,matrix,biom-method}
\alias{coerce,matrix,collection-method}

\title{Conversion Between Collection, Matrix, and BIOM}
\description{
Three mutually convertible data types.  Each has its uses: \code{collection} is a structured object expected by high-level analysis functions, incorporating metadata and multiple views of annotations; \code{matrix} is a basic type expected by many builtin R functions; biological observation matrix (BIOM) is a JSON text format used by other systems and implemented here as a structured character vector.
}
\section{Methods}{
\describe{
\item{\code{signature(from = "biom", to = "collection")}}{...}
\item{\code{signature(from = "biom", to = "list")}}{...}
\item{\code{signature(from = "biom", to = "matrix")}}{...}
\item{\code{signature(from = "character", to = "biom")}}{...}
\item{\code{signature(from = "character", to = "collection")}}{...}
\item{\code{signature(from = "collection", to = "biom")}}{...}
\item{\code{signature(from = "collection", to = "matrix")}}{...}
\item{\code{signature(from = "matrix", to = "biom")}}{...}
\item{\code{signature(from = "matrix", to = "collection")}}{...}
}
}
\keyword{methods}
