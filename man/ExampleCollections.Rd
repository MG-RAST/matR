\name{ExampleCollections}
\alias{xx1}
\alias{xx2}
\alias{xx3}
\alias{xx4}
\alias{xx5}
\alias{xx6}
\alias{xx7}
\alias{yy1}
\alias{yy2}
\alias{yy3}
\alias{yy4}

\docType{data}

\title{
Example collections of metagenomic samples
}

\description{
Collections of metagenomes prebuilt as objects of class "biom", 
for demonstration purposes and getting started.
}

\usage{
xx1
xx2
xx3
xx4
xx5
xx6
xx7
yy1
yy2
yy3
yy4
}

\details{
"xx1" to "xx7" are collections of metagenomes prebuilt with "biomRequest()", 
included as examples to help users get started.
They are objects of class "biom" built by "buildDemoSets()", 
which is undocumented but exported.

"yy1" to "yy4" correspond to the example lists "li1" to "li4" from package BIOM.utils.
That is, "yy#" is equal to "biom(li#)".  These contain (all but the last) vacuous data.
}
