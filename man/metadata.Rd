\name{metadata}
\alias{metadata}
\alias{Metadata}
\title{Access Metadata of Objects}
\description{}
\usage{
metadata (x, extent = "asis", ...)
}
\arguments{
  \item{x}{a \code{collection}, \code{selection}, or character string identifier of an \code{MG-RAST} project, sample, library, or metagenome (e.g., \code{"mgp101"})}
  \item{extent}{control over completeness of metadata retrieval}
  \item{...}{arguments passed to methods}
}
\details{
Metadata is data about data, that serves to contextualize.  Metadata in \code{matR} is available for projects, samples, libraries, metagenomes, selections, and collections.  Note that some metadata objects are naturally related: for instance, metadata of a sample is related to metadata of a project in which it resides.  This suggests a hierarchical stucture, but note that the hierarchy order is not fixed:  sample metadata is "part" of its project's metadata, but from a different perspective, the project metadata is "part" of the sample metadata, too.

A metadata object, regardless of what kind of thing it describes, is like a filesystem, containing files and folders.  File names correspond to metadata field names, file contents correspond to metadata values, and folders correspond to \emph{related metadata objects}.  So as in a filesystem, each "folder" may in turn contain more "files" and/or "folders", and so on.

Without further specification, the metadata of (for instance) a project is not yet definite, because there is ambiguity in referring simply to "the metadata of the project".  Some metadata is \emph{proper} to the project, such as \code{PI_organization}, but metadata of any related objects (such as samples in the project) is also metadata of the project, in a sense.  Consequently, when requesting metadata of an object, you are enabled to control its \code{extent}, that is, how much related metadata to include.  The default \code{extent} will work well in most cases.

To continue the filesystem analogy:  accessing a particular metadata field requires knowing its "filesystem path".  An index vector is the natural mechanism for this in \code{R}, that is, a character vector that successively names each "folder" in the hierarchy of nested "folders" that contains the "file" you want.  See below for typical examples of this.

Metadata objects are printed with fields indented according to their level within the hierachical structure, and that makes it easy to see the "path" of any particular field.

The metadata of a \code{collection} is exactly the metadata of the \code{selection} it is based on.  By default, the metadata of a \code{selection} is the aggregate of metadata objects for each metagenome included in the \code{selection}.  This can be modified with \code{extent} and according to how the \code{selection} is constructed.
}
\value{an object of class \code{rlist}}
\references{
\url{http://blog.metagenomics.anl.gov/metadata-in-mg-rast/},\cr
\url{http://gensc.org/gc_wiki/index.php/MIxS}
}
%\author{Daniel T. Braithwaite}
\seealso{\code{\link{Collections}}, \code{\link{Resources and Identifiers}}, \code{\link{rlist-class}}, \code{\link{metadata-methods}}}
\examples{
\dontrun{
metadata ("mgp92") [[c ("metadata", "study_abstract")]]

metadata ("mgs25665") [[c ("env_package", "metadata", "host_common_name")]]

metadata ("4440284.3") [[c ("metadata", "library", "data", "seq_meth")]]

mm <- collection ("4440283.3;4440286.3;4440285.3")
metadata (mm) [[c ("4440286.3", "metadata", "project", "data", "PI_organization")]]
metadata (mm) [[c ("4440286.3", "metadata", "sample", "data", "latitude")]]

md <- metadata ("4440283.3;4440286.3;4440285.3")
md$`4440286.3`$metadata$sample$data$latitude

ln <- c ("4440286.3", "metadata", "project", "data", "PI_lastname"
org <- c ("4440286.3", "metadata", "project", "data", "PI_organization"
lat <- c ("4440286.3", "metadata", "sample", "data", "latitude")
md [list (ln, org, lat)]

md[,c("metadata","sample","data","latitude")]


# live code actually requires slightly different calls for the moment
# metadata ("mgp92", resource="project") [[c("metadata","study_abstract")]]
# metadata ("25665", resource="sample") [[c("env_package","metadata", "host_common_name")]]
# metadata ("mgm4440284.3", resource="metagenome") [[c ("metadata", "library", "data", "seq_meth")]]

}
}
