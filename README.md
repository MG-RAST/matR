Package matR (Metagenomics Analysis Tools for R) is an analysis client for the 
MG-RAST metagenome annotation engine, part of the US Department of Energy (DOE)
Systems Biology Knowledge Base (KBase).  Customized analysis and visualization
tools securely access remote data and metadata within the popular open source R 
language and environment for statistical computing.  
See: http://mg-rast.org

CRAN (Comprehensive R Archive Network) hosts 
the [package](https://CRAN.R-project.org/package=matR).

LOADING
-------
During an R session, load matR with:

	> library(matR)

INSTALLATION
------------
	> remove.packages('matR')        # if necessary
	> install.packages('devtools')
	> library(devtools)
	> install_github(repo='MG-RAST/matR') 
	> library(matR)
	> dependencies()


QUICK START
-------------

    > # auth.MGRAST('MgRastWebKeyGoesHereKEJ88')      # get this from the upload page if you need to access private data
    > list_mgp80869 <- metadata("mgp80869")$mgp80869  #  Cross CZO soil depth profiles  project
    > biom_phylum <- biomRequest(list_mgp80869, request="organism", hit_type="all", source="RDP", group_level="phylum", evalue=15,  wait=TRUE)
    > phylum_matrix <- as.matrix(biom_phylum)
    > dimnames(phylum_matrix)

