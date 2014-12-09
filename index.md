`Metagenomics Analysis Tools: the "matR" package for R`
=======================================================
See **"Getting started"** at the bottom of this page for installation instructions.
The [package](http://cran.r-project.org/web/packages/matR/index.html)
and manual
([HTML]();
[pdf](http://cran.r-project.org/web/packages/matR/matR.pdf))
are available on
[CRAN](http://cran.r-project.org).

HTML and pdf versions of **"Tutorials"** are available just below.
Or if you have matR installed, execute these in your R session,
for example with:

	> library(matR)
	> step.through(file="http://mg-rast.github.io/matR/using-metadata.R")

Execute the **"Other scripts"** locally in the same way.
Current add-ons to the package's CRAN version
are listed under **"Extensions"** and can be loaded with:

	> library(matR)
	> source("file="http://mg-rast.github.io/matR/local-workbench.R")

Send questions and comments to [mg-rast@mcs.anl.gov]().

***

`Tutorials`
---------
+ [HTML](boxplot-examples.html) | [pdf](boxplot-examples.pdf) | [source](boxplot-examples.R) : boxplot-examples
+ [HTML]() | [pdf]() | [source]() : how-to-search
+ [HTML]() | [pdf]() | [source]() : using-metadata
+ [HTML]() | [pdf]() | [source]() : how-to-download-annotations
+ [HTML]() | [pdf]() | [source]() : handling-biom

`Extensions`
------------
+ [source]() : local-workbench
+ [source]() : improved-color-palettes

`Other scripts`
---------------
+ [HTML]() | [pdf]() | [source]() : soils-2014-tutorial-1
+ [HTML]() | [pdf]() | [source]() : soils-2014-tutorial-2
+ [HTML]() | [pdf]() | [source]() : soils-2014-tutorial-3

`Getting Started`
-----------------
Make sure to have a [current version of R](http://www.r-project.org).
Then open an R session and install the package with:

	> install.packages('matR', dependencies=TRUE)
	> library(matR)
	> dependencies()

It must be loaded during every R session with:  `library(matR)`

`Old version`
-------------
Users of the early release version can reinstall it with:

	> remove.packages('matR')        # if necessary
	> install.packages('devtools')
	> library(devtools)
	> install_github(repo='MG-RAST/matR', ref="early-release")
	> library(matR)
	> dependencies()
