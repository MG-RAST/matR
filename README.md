Package matR (metagenomics analysis tools for R) is an analysis client for the 
MG-RAST metagenome annotation engine, part of the US Department of Energy (DOE)
Systems Biology Knowledge Base (KBase).  Customized statistical and visualization
tools securely access remote data and metadata.
See: http://metagenomics.anl.gov
and: http://kbase.us

LOADING
During an R session, load matR with:
> library(matR)

INSTALLATION (SEE INSTALLATION NOTE BELOW!)

\## Make sure to have a current version of the R language (http://www.r-project.org).

\## Then install during an R session with:

\##

\##     install.packages(matR)

\##     library(matR)

\##     dependencies()

INSTALLTION NOTE (October 2014)
We are in the process of updating matR code - during this time, it is possible that 
some matR functionality be temporarily unavaialable in the most current version.

To install the last stable release of matR, you can use the following commands:

\# uninstall current matR

remove.packages("matR")

\# install devtools, then use it to install matR from the July 10 2014 commit

   library(devtools)
   install_github(repo="MG-RAST/matR", dependencies=FALSE, ref="3d068d0c4c644083f588379ca111a575a409c797")
   library(matR)
   dependencies()
