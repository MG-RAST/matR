\name{analysis-class}
\alias{analysis-class}
\alias{Analyses}
\alias{analyses}
\alias{Analysis}
\alias{analysis}
\docType{class}

\title{Class \code{"analysis"}}
\description{
\code{matR} defines various classes, generic functions, and methods to facilitate standard and custom metagenomics analysis workflows.  It is convenient to think of several stages:  data preparation, computation, visualization, and export.  These often occur in sequence, and modifications of some stages might recur, according to the investigation.

Data preparation is typically accomplished with the \code{collection} function, which enables retrieval of specified metagenomes from \code{MG-RAST}.  Such \code{collection} objects can be saved, restored, and shared.  After some computations, part of a \code{collection} might be subselected for reanalysis.

Three varieties of analysis computation are currently implemented: principal components analysis, heatmap-dendrogram analysis, and per-row significance testing.  The associated functions (\code{pco}, \code{heatmap}, and \code{sigtest}) are generic, with methods defined for a variety of appropriate argument types.  Each function name, additionally, is the name of an object class returned by the function of the same name.  Further details are available via links below.

Two generic functions accomplish the goals of visualization and export, respectively: \code{render} and \code{asFile}.  In this context, "export" means writing computed numerical data to a text or binary file.  Images, too, can be saved in various graphical formats, but that is accomplished by an argument to \code{render}.  By default, in interactive use, \code{render} displays visualizations in windows.  Further details are available via links below.

\code{asFile} can be applied to most objects defined or used by the package.  \code{render} applies to objects of class: \code{collection}, \code{pco}, \code{heatmap}.  Further details are available via links below.

\code{analysis} is the virtual class encapsulating the specific types of analysis class, presently: \code{pco}, \code{heatmap}, \code{sigtest}.
}
\author{Daniel T. Braithwaite}
\seealso{\code{\link{collection}}, \code{\link{pco}},\code{\link{heatmap}}, \code{\link{sigtest}}, \code{\link{asFile}},\code{\link{render}}}
\examples{
# give a solid example of an analysis workflow here
}
