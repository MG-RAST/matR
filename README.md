Package matR (Metagenomics Analysis Tools for R) is an analysis client for the 
MG-RAST metagenome annotation engine, part of the US Department of Energy (DOE)
Systems Biology Knowledge Base (KBase).  Customized analysis and visualization
tools securely access remote data and metadata within the popular open source R 
language and environment for statistical computing.  See: http://metagenomics.anl.gov
and: http://kbase.us.

CRAN (Comprehensive R Archive Network) hosts 
the [package](http://cran.r-project.org/web/packages/matR/index.html)
including 
its [manual](http://cran.r-project.org/web/packages/matR/matR.pdf).

LOADING
-------
During an R session, load matR with:

	> library(matR)

INSTALLATION
------------
Make sure to have a current version of R, available from http://www.r-project.org.
Then during an R session, install with:

	> install.packages('matR', dep=TRUE)
	> library(matR)
	> dependencies()

DEVELOPMENT VERSION
-------------
Users of the early release version can reinstall it with:

	> remove.packages('matR')        # if necessary
	> install.packages('devtools')
	> library(devtools)
	> install_github(repo='MG-RAST/matR', ref="early-release")
	> library(matR)
	> dependencies()
