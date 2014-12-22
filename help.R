#' ---
#' title: "General information and troubleshooting"
#' author: ""
#' date: ""
#' ---

#' Once installed, the package must be loaded separately 
#' in every session with: `library(matR)`.
#'
#' Help within R
#' -------------
#' Note that on the matR homepage, a pdf version of the manual is linked.
#' With the package loaded in your R session, runtime help is available.
#' For a list of topics, try the first command below.
#' As usual in R, using `?` gives help for individual functions, as shown.

#+ eval=FALSE
help(package='matR')
?biomRequest

#'
#' Demos
#' -----
#' Note that the executable tutorials posted on the matR homepage serve nicely as demos.
#' Demos are also distributed with the package.  
#' The first command below lists them,
#' and the second runs a demo called `"API"`.

#+ eval=FALSE
demo(package='matR')
step.through('API')

#' Installation troubleshooting
#' ------------
#' Simple instructions are given on the matR homepage.
#' In case of trouble,
#' here is detailed information and a more thorough procedure.
#' matR makes liberal use of other R software packages.
#' These "dependencies" should also be installed.
#' matR and its dependencies are mostly available from 
#' [CRAN (the Comprehensive R Archive Network)](http://cran.r-project.org), 
#' but some are only available from the [Bioconductor repository](http://bioconductor.org).
#' With the following commands, tell your R session where to look for those repositories.
#' When prompted, select locations near you.

#+ eval=FALSE
chooseCRANmirror(graphics=FALSE)
chooseBioCmirror(graphics=FALSE)

#' Next, execute this command and select _all the repositories listed_:

#+ eval=FALSE
setRepositories(graphics=FALSE)

#' Then, the single command below should install matR and all its dependencies.

#+ eval=FALSE
install.packages("matR", dependencies=TRUE)

#' About package dependencies
#' --------------------------
#' Difficulties may arise with packages that matR uses ("dependencies").
#' Third-party open-source software offers a wealth of functionality that can be leveraged to advantage,
#' but often incurring costs in stability and portability.
#' Addressing such issues is beyond the scope of this tutorial, but
#' the maintainer of a problematic package should be able to help.
#' Note that a missing dependency affects only the particular functionality it supports.
#' So matR can always be used without all its dependencies installed.

#'
#' Discussion forum
#' -----------------
#' Check out our [Google group (matR-forum)](https://groups.google.com/forum/#!forum/matR-forum)
#' for discussion and help.
