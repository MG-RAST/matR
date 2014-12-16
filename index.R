#' ---
#' title: "Metagenomics Analysis Tools: the matR package"
#' author: ""
#' date: ""
#' ---

#' The matR [package](http://cran.r-project.org/web/packages/matR/index.html)
#' and manual
#' ([HTML]();
#' [pdf](http://cran.r-project.org/web/packages/matR/matR.pdf))
#' are available on
#' [CRAN](http://cran.r-project.org);
#' see **"Installation"** at the bottom of this page to get started.
#' Just below, some extended tutorials are available in HTML and pdf versions.
#' Also, with matR loaded you can execute these in your R session,
#' for example with:

#+ eval=FALSE
step.through(file="http://mg-rast.github.io/matR/analysis-in-a-nutshell.R")

#' In the same way, **"Other scripts"** (below) can be viewed here or executed locally.
#' The **"Extensions"** section lists add-ons to the CRAN version of the package.
#' There's no reason to step through them line by line, but the extra functionality
#' they provide can be introduced to your R session, for example with:

#+ eval=FALSE
source("file="http://mg-rast.github.io/matR/local-workbench.R")

#' Please credit this software if it assists your work.
#' R proposes a standard form for package citation shown by:
		 
#+ eval=FALSE
citation('matR')

#' Check out our [Google group (matR-forum)](https://groups.google.com/forum/#!forum/matR-forum)
#' for discussion and help, 
#' and contact us at [mg-rast@mcs.anl.gov]().

#'
#' Tutorials
#' ---------
#' + [HTML](analysis-in-a-nutshell.html) | [pdf](analysis-in-a-nutshell.pdf) | [source](analysis-in-a-nutshell.R) : analysis-in-a-nutshell
#' + [HTML](retrieving-annotations.html) | [pdf](retrieving-annotations.pdf) | [source](retrieving-annotations.R) : retrieving-annotations
#' + [HTML](annotation-distribution.html) | [pdf](annotation-distribution.pdf) | [source](annotation-distribution.R) : annotation-distribution
#' + [HTML](handling-biom.html) | [pdf](handling-biom.pdf) | [source](handling-biom.R) : handling-biom
#' + [HTML](help.html) | [pdf](help.pdf) | [source](help.R) : help
#' 
#' 
#' Extensions
#' ----------
#' + [source](local-workbench.R) : local-workbench
#' 
#' Other scripts
#' -------------
#' + [HTML]() | [pdf]() | [source]() : soils-2014-tutorial-1
#' + [HTML]() | [pdf]() | [source]() : soils-2014-tutorial-2
#' + [HTML]() | [pdf]() | [source]() : soils-2014-tutorial-3
#' 
#' Installation
#' ------------
#' Make sure to have a [current version of the R language](http://www.r-project.org).
#' Then open an R session and install with:

#+ eval=FALSE
install.packages('matR', dependencies=TRUE)
library(matR)
dependencies()

#' Go through the several prompts following `dependencies()`.  Once installed,
#' the package must be loaded during every session with:  `library(matR)`.
#' 
#' Old version
#' -----------
#' Users of the early release version can reinstall it with:

#+ eval=FALSE
remove.packages('matR')        # if necessary
install.packages('devtools')
library(devtools)
install_github(repo='MG-RAST/matR', ref="early-release")
library(matR)
dependencies()
