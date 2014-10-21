
R version 3.1.1 (2014-07-10) -- "Sock it to Me"
Copyright (C) 2014 The R Foundation for Statistical Computing
Platform: x86_64-apple-darwin10.8.0 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[Previously saved workspace restored]

Loading required package: utils
MGRASTer (0.9 02e288)
BIOM.utils (0.9 dbcb27)
matR: metagenomics analysis tools for R (0.9)
> library(matR)
> N <- 1:4
> List <- mget (paste0 ("xx", N), inherits=TRUE)
> 
> #-----------------------------------------------------------------------------------------
> #  OK FOR CRAN
> #-----------------------------------------------------------------------------------------
> 
> for (xx in List) {
+ #-----------------------------------------------------------------------------------------
+ #  distx()		...biom method
+ #-----------------------------------------------------------------------------------------
+ 	uu <- as.matrix (xx, TRUE) [,1]
+ 	vv <- as.matrix (xx, TRUE) [1,]
+ 	distx (xx)														# distance between columns
+ 	distx (xx, bycol=FALSE)											# distance between rows
+ 	distx (xx, method="bray-curtis")								# alt measure
+ 	distx (xx, method="bray-curtis", bycol=FALSE)
+ 	distx (xx, groups=1:ncol(xx) %% 4)								# mean pairwise distance between groups
+ 	distx (xx, groups=1:nrow(xx) %% 4, bycol=FALSE)					# row groups
+ 	distx (xx, p=uu)												# from each col to a given vector
+ 	distx (xx, p=vv, bycol=FALSE)									# from each row
+ 	distx (xx, p=uu, groups=1:ncol(xx) %% 4)						# from each group to given vector
+ 	distx (xx, p=vv, groups=1:nrow(xx) %% 4, bycol=FALSE)			# row groups
+ }
> 
> for (xx in List) {
+ #-----------------------------------------------------------------------------------------
+ #  rowstats()	...biom method
+ #-----------------------------------------------------------------------------------------
+ 	str (rowstats (xx, groups=seq(along=colnames(xx)) %% 2, test="Kr"))
+ 	str (rowstats (xx, groups=seq(along=colnames(xx)) %% 3, test="Kr"))
+ 	str (rowstats (xx, groups=seq(along=colnames(xx)) %% 2, test="t-test-un"))
+ 	str (rowstats (xx, groups=seq(along=colnames(xx)) %% 2, test="Mann"))		# gives warning re. ties
+ 	str (rowstats (xx, groups=seq(along=colnames(xx)) %% 2, test="AN"))
+ 	str (rowstats (xx, groups=seq(along=colnames(xx)) %% 3, test="AN"))
+ 	if (ncol(xx) %% 2 != 1) {
+ 		str (rowstats (xx, groups=seq(along=colnames(xx)) %% 2, test="t-test-p"))
+ 		str (rowstats (xx, groups=seq(along=colnames(xx)) %% 2, test="Wilc"))
+ 		}
+ }
List of 5
 $ groups   : Factor w/ 2 levels "0","1": 2 1 2 1 2 1 2
  ..- attr(*, "names")= chr [1:7] "mgm4440463.3" "mgm4440464.3" "mgm4441679.3" "mgm4441680.3" ...
 $ statistic: Named num [1:161] 0.5 0.5 0.0324 0.5091 0.5 ...
  ..- attr(*, "names")= chr [1:161] "ABC transporters" "ATP synthases" "Acid stress" "Adhesion" ...
 $ p.value  : Named num [1:161] 0.48 0.48 0.857 0.476 0.48 ...
  ..- attr(*, "names")= chr [1:161] "ABC transporters" "ATP synthases" "Acid stress" "Adhesion" ...
 $ mean     :List of 2
  ..$ 0: num [1:161] 132.7 60 10.3 10 134.3 ...
  ..$ 1: num [1:161] 178.2 109.5 11.2 13.8 244 ...
 $ sd       :List of 2
  ..$ 0: num [1:161] 103.23 73.75 10.07 6.24 99.89 ...
  ..$ 1: num [1:161] 118.23 107.02 5.85 7.89 188.09 ...
List of 5
 $ groups   : Factor w/ 3 levels "0","1","2": 2 3 1 2 3 1 2
  ..- attr(*, "names")= chr [1:7] "mgm4440463.3" "mgm4440464.3" "mgm4441679.3" "mgm4441680.3" ...
 $ statistic: Named num [1:161] 0.179 0.714 1.389 2.509 0.714 ...
  ..- attr(*, "names")= chr [1:161] "ABC transporters" "ATP synthases" "Acid stress" "Adhesion" ...
 $ p.value  : Named num [1:161] 0.915 0.7 0.499 0.285 0.7 ...
  ..- attr(*, "names")= chr [1:161] "ABC transporters" "ATP synthases" "Acid stress" "Adhesion" ...
 $ mean     :List of 3
  ..$ 0: num [1:161] 202 108 14.5 19.5 285.5 ...
  ..$ 1: num [1:161] 156.67 59.67 12.33 7.33 139.67 ...
  ..$ 2: num [1:161] 118 112 5 12 194 ...
 $ sd       :List of 3
  ..$ 0: num [1:161] 8.49 121.62 7.78 3.54 235.47 ...
  ..$ 1: num [1:161] 138.414 73.928 7.506 0.577 99.551 ...
  ..$ 2: num [1:161] 146.37 139.3 5.66 9.9 214.25 ...
List of 5
 $ groups   : Factor w/ 2 levels "0","1": 2 1 2 1 2 1 2
  ..- attr(*, "names")= chr [1:7] "mgm4440463.3" "mgm4440464.3" "mgm4441679.3" "mgm4441680.3" ...
 $ statistic: Named num [1:161] -0.543 -0.724 -0.141 -0.702 -0.994 ...
  ..- attr(*, "names")= chr [1:161] "ABC transporters" "ATP synthases" "Acid stress" "Adhesion" ...
 $ p.value  : Named num [1:161] 0.611 0.502 0.897 0.515 0.369 ...
  ..- attr(*, "names")= chr [1:161] "ABC transporters" "ATP synthases" "Acid stress" "Adhesion" ...
 $ mean     :List of 2
  ..$ 0: num [1:161] 132.7 60 10.3 10 134.3 ...
  ..$ 1: num [1:161] 178.2 109.5 11.2 13.8 244 ...
 $ sd       :List of 2
  ..$ 0: num [1:161] 103.23 73.75 10.07 6.24 99.89 ...
  ..$ 1: num [1:161] 118.23 107.02 5.85 7.89 188.09 ...
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
List of 5
 $ groups   : Factor w/ 2 levels "0","1": 2 1 2 1 2 1 2
  ..- attr(*, "names")= chr [1:7] "mgm4440463.3" "mgm4440464.3" "mgm4441679.3" "mgm4441680.3" ...
 $ statistic: Named num [1:161] 4 4 6.5 4 4 3 6 4 3 7 ...
  ..- attr(*, "names")= chr [1:161] "ABC transporters" "ATP synthases" "Acid stress" "Adhesion" ...
 $ p.value  : Named num [1:161] 0.629 0.629 1 0.593 0.629 ...
  ..- attr(*, "names")= chr [1:161] "ABC transporters" "ATP synthases" "Acid stress" "Adhesion" ...
 $ mean     :List of 2
  ..$ 0: num [1:161] 132.7 60 10.3 10 134.3 ...
  ..$ 1: num [1:161] 178.2 109.5 11.2 13.8 244 ...
 $ sd       :List of 2
  ..$ 0: num [1:161] 103.23 73.75 10.07 6.24 99.89 ...
  ..$ 1: num [1:161] 118.23 107.02 5.85 7.89 188.09 ...
List of 5
 $ groups   : Factor w/ 2 levels "0","1": 2 1 2 1 2 1 2
  ..- attr(*, "names")= chr [1:7] "mgm4440463.3" "mgm4440464.3" "mgm4441679.3" "mgm4441680.3" ...
 $ statistic: Named num [1:161] 0.2816 0.4642 0.0236 0.4553 0.8176 ...
  ..- attr(*, "names")= chr [1:161] "ABC transporters" "ATP synthases" "Acid stress" "Adhesion" ...
 $ p.value  : Named num [1:161] 0.618 0.526 0.884 0.53 0.407 ...
  ..- attr(*, "names")= chr [1:161] "ABC transporters" "ATP synthases" "Acid stress" "Adhesion" ...
 $ mean     :List of 2
  ..$ 0: num [1:161] 132.7 60 10.3 10 134.3 ...
  ..$ 1: num [1:161] 178.2 109.5 11.2 13.8 244 ...
 $ sd       :List of 2
  ..$ 0: num [1:161] 103.23 73.75 10.07 6.24 99.89 ...
  ..$ 1: num [1:161] 118.23 107.02 5.85 7.89 188.09 ...
List of 5
 $ groups   : Factor w/ 3 levels "0","1","2": 2 3 1 2 3 1 2
  ..- attr(*, "names")= chr [1:7] "mgm4440463.3" "mgm4440464.3" "mgm4441679.3" "mgm4441680.3" ...
 $ statistic: Named num [1:161] 0.58262 0.00124 2.08326 1.20912 0.29911 ...
  ..- attr(*, "names")= chr [1:161] "ABC transporters" "ATP synthases" "Acid stress" "Adhesion" ...
 $ p.value  : Named num [1:161] 0.48 0.973 0.209 0.322 0.608 ...
  ..- attr(*, "names")= chr [1:161] "ABC transporters" "ATP synthases" "Acid stress" "Adhesion" ...
 $ mean     :List of 3
  ..$ 0: num [1:161] 202 108 14.5 19.5 285.5 ...
  ..$ 1: num [1:161] 156.67 59.67 12.33 7.33 139.67 ...
  ..$ 2: num [1:161] 118 112 5 12 194 ...
 $ sd       :List of 3
  ..$ 0: num [1:161] 8.49 121.62 7.78 3.54 235.47 ...
  ..$ 1: num [1:161] 138.414 73.928 7.506 0.577 99.551 ...
  ..$ 2: num [1:161] 146.37 139.3 5.66 9.9 214.25 ...
List of 5
 $ groups   : Factor w/ 2 levels "0","1": 2 1 2 1 2 1 2 1 2 1 ...
  ..- attr(*, "names")= chr [1:23] "mgm4440411.3" "mgm4440412.3" "mgm4440413.3" "mgm4440414.3" ...
 $ statistic: Named num [1:62] 1.2297 0.3068 0.9167 0.0351 0.0771 ...
  ..- attr(*, "names")= chr [1:62] "Acidobacteria" "Actinobacteria" "Annelida" "Apicomplexa" ...
 $ p.value  : Named num [1:62] 0.267 0.58 0.338 0.851 0.781 ...
  ..- attr(*, "names")= chr [1:62] "Acidobacteria" "Actinobacteria" "Annelida" "Apicomplexa" ...
 $ mean     :List of 2
  ..$ 0: num [1:62] 187 18849 0 324 472 ...
  ..$ 1: num [1:62] 1.30e+02 1.00e+03 8.33e-02 3.63e+01 3.49e+02 ...
 $ sd       :List of 2
  ..$ 0: num [1:62] 307 41480 0 697 882 ...
  ..$ 1: num [1:62] 161.26 2026.316 0.289 87.811 840.933 ...
List of 5
 $ groups   : Factor w/ 3 levels "0","1","2": 2 3 1 2 3 1 2 3 1 2 ...
  ..- attr(*, "names")= chr [1:23] "mgm4440411.3" "mgm4440412.3" "mgm4440413.3" "mgm4440414.3" ...
 $ statistic: Named num [1:62] 5.18 4.62 1.88 1.18 2.28 ...
  ..- attr(*, "names")= chr [1:62] "Acidobacteria" "Actinobacteria" "Annelida" "Apicomplexa" ...
 $ p.value  : Named num [1:62] 0.0752 0.0992 0.3916 0.5556 0.3193 ...
  ..- attr(*, "names")= chr [1:62] "Acidobacteria" "Actinobacteria" "Annelida" "Apicomplexa" ...
 $ mean     :List of 3
  ..$ 0: num [1:62] 285 13886 0 247 544 ...
  ..$ 1: num [1:62] 122 14160 0 237 223 ...
  ..$ 2: num [1:62] 81.125 1109.875 0.125 47.125 473.625 ...
 $ sd       :List of 3
  ..$ 0: num [1:62] 255 34716 0 607 1061 ...
  ..$ 1: num [1:62] 271 39520 0 647 391 ...
  ..$ 2: num [1:62] 156.842 2514.653 0.354 107.855 1026.21 ...
List of 5
 $ groups   : Factor w/ 2 levels "0","1": 2 1 2 1 2 1 2 1 2 1 ...
  ..- attr(*, "names")= chr [1:23] "mgm4440411.3" "mgm4440412.3" "mgm4440413.3" "mgm4440414.3" ...
 $ statistic: Named num [1:62] 0.543 1.426 -1 1.36 0.34 ...
  ..- attr(*, "names")= chr [1:62] "Acidobacteria" "Actinobacteria" "Annelida" "Apicomplexa" ...
 $ p.value  : Named num [1:62] 0.595 0.184 0.339 0.203 0.737 ...
  ..- attr(*, "names")= chr [1:62] "Acidobacteria" "Actinobacteria" "Annelida" "Apicomplexa" ...
 $ mean     :List of 2
  ..$ 0: num [1:62] 187 18849 0 324 472 ...
  ..$ 1: num [1:62] 1.30e+02 1.00e+03 8.33e-02 3.63e+01 3.49e+02 ...
 $ sd       :List of 2
  ..$ 0: num [1:62] 307 41480 0 697 882 ...
  ..$ 1: num [1:62] 161.26 2026.316 0.289 87.811 840.933 ...
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
List of 5
 $ groups   : Factor w/ 2 levels "0","1": 2 1 2 1 2 1 2 1 2 1 ...
  ..- attr(*, "names")= chr [1:23] "mgm4440411.3" "mgm4440412.3" "mgm4440413.3" "mgm4440414.3" ...
 $ statistic: Named num [1:62] 48 57 60.5 69 61.5 64 64.5 43 65 46 ...
  ..- attr(*, "names")= chr [1:62] "Acidobacteria" "Actinobacteria" "Annelida" "Apicomplexa" ...
 $ p.value  : Named num [1:62] 0.281 0.608 0.384 0.876 0.805 ...
  ..- attr(*, "names")= chr [1:62] "Acidobacteria" "Actinobacteria" "Annelida" "Apicomplexa" ...
 $ mean     :List of 2
  ..$ 0: num [1:62] 187 18849 0 324 472 ...
  ..$ 1: num [1:62] 1.30e+02 1.00e+03 8.33e-02 3.63e+01 3.49e+02 ...
 $ sd       :List of 2
  ..$ 0: num [1:62] 307 41480 0 697 882 ...
  ..$ 1: num [1:62] 161.26 2026.316 0.289 87.811 840.933 ...
List of 5
 $ groups   : Factor w/ 2 levels "0","1": 2 1 2 1 2 1 2 1 2 1 ...
  ..- attr(*, "names")= chr [1:23] "mgm4440411.3" "mgm4440412.3" "mgm4440413.3" "mgm4440414.3" ...
 $ statistic: Named num [1:62] 0.31 2.226 0.913 2.019 0.116 ...
  ..- attr(*, "names")= chr [1:62] "Acidobacteria" "Actinobacteria" "Annelida" "Apicomplexa" ...
 $ p.value  : Named num [1:62] 0.584 0.151 0.35 0.17 0.737 ...
  ..- attr(*, "names")= chr [1:62] "Acidobacteria" "Actinobacteria" "Annelida" "Apicomplexa" ...
 $ mean     :List of 2
  ..$ 0: num [1:62] 187 18849 0 324 472 ...
  ..$ 1: num [1:62] 1.30e+02 1.00e+03 8.33e-02 3.63e+01 3.49e+02 ...
 $ sd       :List of 2
  ..$ 0: num [1:62] 307 41480 0 697 882 ...
  ..$ 1: num [1:62] 161.26 2026.316 0.289 87.811 840.933 ...
List of 5
 $ groups   : Factor w/ 3 levels "0","1","2": 2 3 1 2 3 1 2 3 1 2 ...
  ..- attr(*, "names")= chr [1:23] "mgm4440411.3" "mgm4440412.3" "mgm4440413.3" "mgm4440414.3" ...
 $ statistic: Named num [1:62] 2.9049 0.7292 1.4348 0.6217 0.0164 ...
  ..- attr(*, "names")= chr [1:62] "Acidobacteria" "Actinobacteria" "Annelida" "Apicomplexa" ...
 $ p.value  : Named num [1:62] 0.103 0.403 0.244 0.439 0.899 ...
  ..- attr(*, "names")= chr [1:62] "Acidobacteria" "Actinobacteria" "Annelida" "Apicomplexa" ...
 $ mean     :List of 3
  ..$ 0: num [1:62] 285 13886 0 247 544 ...
  ..$ 1: num [1:62] 122 14160 0 237 223 ...
  ..$ 2: num [1:62] 81.125 1109.875 0.125 47.125 473.625 ...
 $ sd       :List of 3
  ..$ 0: num [1:62] 255 34716 0 607 1061 ...
  ..$ 1: num [1:62] 271 39520 0 647 391 ...
  ..$ 2: num [1:62] 156.842 2514.653 0.354 107.855 1026.21 ...
List of 5
 $ groups   : Factor w/ 2 levels "0","1": 2 1 2 1 2 1 2 1 2 1 ...
  ..- attr(*, "names")= chr [1:32] "mgm4477803.3" "mgm4477804.3" "mgm4477805.3" "mgm4477807.3" ...
 $ statistic: Named num [1:28] 0.32 0.513 0.142 0.513 0.364 ...
  ..- attr(*, "names")= chr [1:28] "Amino Acids and Derivatives" "Carbohydrates" "Cell Division and Cell Cycle" "Cell Wall and Capsule" ...
 $ p.value  : Named num [1:28] 0.572 0.474 0.706 0.474 0.546 ...
  ..- attr(*, "names")= chr [1:28] "Amino Acids and Derivatives" "Carbohydrates" "Cell Division and Cell Cycle" "Cell Wall and Capsule" ...
 $ mean     :List of 2
  ..$ 0: num [1:28] 135113 165454 22660 45526 225689 ...
  ..$ 1: num [1:28] 98705 120280 16522 34696 167773 ...
 $ sd       :List of 2
  ..$ 0: num [1:28] 148565 181621 26300 48946 250248 ...
  ..$ 1: num [1:28] 101669 123878 17503 35566 172331 ...
List of 5
 $ groups   : Factor w/ 3 levels "0","1","2": 2 3 1 2 3 1 2 3 1 2 ...
  ..- attr(*, "names")= chr [1:32] "mgm4477803.3" "mgm4477804.3" "mgm4477805.3" "mgm4477807.3" ...
 $ statistic: Named num [1:28] 1.134 0.886 1.106 1.3 1.106 ...
  ..- attr(*, "names")= chr [1:28] "Amino Acids and Derivatives" "Carbohydrates" "Cell Division and Cell Cycle" "Cell Wall and Capsule" ...
 $ p.value  : Named num [1:28] 0.567 0.642 0.575 0.522 0.575 ...
  ..- attr(*, "names")= chr [1:28] "Amino Acids and Derivatives" "Carbohydrates" "Cell Division and Cell Cycle" "Cell Wall and Capsule" ...
 $ mean     :List of 3
  ..$ 0: num [1:28] 125094 155513 21295 42397 211970 ...
  ..$ 1: num [1:28] 134246 160823 22977 47472 228743 ...
  ..$ 2: num [1:28] 92132 113415 14655 30673 150864 ...
 $ sd       :List of 3
  ..$ 0: num [1:28] 138364 172989 23203 46258 233111 ...
  ..$ 1: num [1:28] 133973 160203 24338 45464 227311 ...
  ..$ 2: num [1:28] 115850 141648 20202 37805 192135 ...
List of 5
 $ groups   : Factor w/ 2 levels "0","1": 2 1 2 1 2 1 2 1 2 1 ...
  ..- attr(*, "names")= chr [1:32] "mgm4477803.3" "mgm4477804.3" "mgm4477805.3" "mgm4477807.3" ...
 $ statistic: Named num [1:28] 0.809 0.822 0.777 0.716 0.762 ...
  ..- attr(*, "names")= chr [1:28] "Amino Acids and Derivatives" "Carbohydrates" "Cell Division and Cell Cycle" "Cell Wall and Capsule" ...
 $ p.value  : Named num [1:28] 0.426 0.418 0.444 0.48 0.453 ...
  ..- attr(*, "names")= chr [1:28] "Amino Acids and Derivatives" "Carbohydrates" "Cell Division and Cell Cycle" "Cell Wall and Capsule" ...
 $ mean     :List of 2
  ..$ 0: num [1:28] 135113 165454 22660 45526 225689 ...
  ..$ 1: num [1:28] 98705 120280 16522 34696 167773 ...
 $ sd       :List of 2
  ..$ 0: num [1:28] 148565 181621 26300 48946 250248 ...
  ..$ 1: num [1:28] 101669 123878 17503 35566 172331 ...
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
List of 5
 $ groups   : Factor w/ 2 levels "0","1": 2 1 2 1 2 1 2 1 2 1 ...
  ..- attr(*, "names")= chr [1:32] "mgm4477803.3" "mgm4477804.3" "mgm4477805.3" "mgm4477807.3" ...
 $ statistic: Named num [1:28] 143 147 138 147 144 141 138 137 151 134 ...
  ..- attr(*, "names")= chr [1:28] "Amino Acids and Derivatives" "Carbohydrates" "Cell Division and Cell Cycle" "Cell Wall and Capsule" ...
 $ p.value  : Named num [1:28] 0.59 0.491 0.724 0.491 0.564 ...
  ..- attr(*, "names")= chr [1:28] "Amino Acids and Derivatives" "Carbohydrates" "Cell Division and Cell Cycle" "Cell Wall and Capsule" ...
 $ mean     :List of 2
  ..$ 0: num [1:28] 135113 165454 22660 45526 225689 ...
  ..$ 1: num [1:28] 98705 120280 16522 34696 167773 ...
 $ sd       :List of 2
  ..$ 0: num [1:28] 148565 181621 26300 48946 250248 ...
  ..$ 1: num [1:28] 101669 123878 17503 35566 172331 ...
List of 5
 $ groups   : Factor w/ 2 levels "0","1": 2 1 2 1 2 1 2 1 2 1 ...
  ..- attr(*, "names")= chr [1:32] "mgm4477803.3" "mgm4477804.3" "mgm4477805.3" "mgm4477807.3" ...
 $ statistic: Named num [1:28] 0.654 0.676 0.604 0.513 0.581 ...
  ..- attr(*, "names")= chr [1:28] "Amino Acids and Derivatives" "Carbohydrates" "Cell Division and Cell Cycle" "Cell Wall and Capsule" ...
 $ p.value  : Named num [1:28] 0.425 0.418 0.443 0.48 0.452 ...
  ..- attr(*, "names")= chr [1:28] "Amino Acids and Derivatives" "Carbohydrates" "Cell Division and Cell Cycle" "Cell Wall and Capsule" ...
 $ mean     :List of 2
  ..$ 0: num [1:28] 135113 165454 22660 45526 225689 ...
  ..$ 1: num [1:28] 98705 120280 16522 34696 167773 ...
 $ sd       :List of 2
  ..$ 0: num [1:28] 148565 181621 26300 48946 250248 ...
  ..$ 1: num [1:28] 101669 123878 17503 35566 172331 ...
List of 5
 $ groups   : Factor w/ 3 levels "0","1","2": 2 3 1 2 3 1 2 3 1 2 ...
  ..- attr(*, "names")= chr [1:32] "mgm4477803.3" "mgm4477804.3" "mgm4477805.3" "mgm4477807.3" ...
 $ statistic: Named num [1:28] 0.366 0.397 0.484 0.417 0.444 ...
  ..- attr(*, "names")= chr [1:28] "Amino Acids and Derivatives" "Carbohydrates" "Cell Division and Cell Cycle" "Cell Wall and Capsule" ...
 $ p.value  : Named num [1:28] 0.55 0.534 0.492 0.523 0.51 ...
  ..- attr(*, "names")= chr [1:28] "Amino Acids and Derivatives" "Carbohydrates" "Cell Division and Cell Cycle" "Cell Wall and Capsule" ...
 $ mean     :List of 3
  ..$ 0: num [1:28] 125094 155513 21295 42397 211970 ...
  ..$ 1: num [1:28] 134246 160823 22977 47472 228743 ...
  ..$ 2: num [1:28] 92132 113415 14655 30673 150864 ...
 $ sd       :List of 3
  ..$ 0: num [1:28] 138364 172989 23203 46258 233111 ...
  ..$ 1: num [1:28] 133973 160203 24338 45464 227311 ...
  ..$ 2: num [1:28] 115850 141648 20202 37805 192135 ...
List of 5
 $ groups   : Factor w/ 2 levels "0","1": 2 1 2 1 2 1 2 1 2 1 ...
  ..- attr(*, "names")= chr [1:32] "mgm4477803.3" "mgm4477804.3" "mgm4477805.3" "mgm4477807.3" ...
 $ statistic: Named num [1:28] 2.17 2.25 1.84 1.77 1.9 ...
  ..- attr(*, "names")= chr [1:28] "Amino Acids and Derivatives" "Carbohydrates" "Cell Division and Cell Cycle" "Cell Wall and Capsule" ...
 $ p.value  : Named num [1:28] 0.0465 0.0396 0.086 0.0978 0.0764 ...
  ..- attr(*, "names")= chr [1:28] "Amino Acids and Derivatives" "Carbohydrates" "Cell Division and Cell Cycle" "Cell Wall and Capsule" ...
 $ mean     :List of 2
  ..$ 0: num [1:28] 135113 165454 22660 45526 225689 ...
  ..$ 1: num [1:28] 98705 120280 16522 34696 167773 ...
 $ sd       :List of 2
  ..$ 0: num [1:28] 148565 181621 26300 48946 250248 ...
  ..$ 1: num [1:28] 101669 123878 17503 35566 172331 ...
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, paired = TRUE,  :
  cannot compute exact p-value with zeroes
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, paired = TRUE,  :
  cannot compute exact p-value with zeroes
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, paired = TRUE,  :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, paired = TRUE,  :
  cannot compute exact p-value with zeroes
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, paired = TRUE,  :
  cannot compute exact p-value with zeroes
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, paired = TRUE,  :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, paired = TRUE,  :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, paired = TRUE,  :
  cannot compute exact p-value with zeroes
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, paired = TRUE,  :
  cannot compute exact p-value with zeroes
List of 5
 $ groups   : Factor w/ 2 levels "0","1": 2 1 2 1 2 1 2 1 2 1 ...
  ..- attr(*, "names")= chr [1:32] "mgm4477803.3" "mgm4477804.3" "mgm4477805.3" "mgm4477807.3" ...
 $ statistic: Named num [1:28] 101 104 97 106 96 100 89 76 105 79 ...
  ..- attr(*, "names")= chr [1:28] "Amino Acids and Derivatives" "Carbohydrates" "Cell Division and Cell Cycle" "Cell Wall and Capsule" ...
 $ p.value  : Named num [1:28] 0.0934 0.0654 0.1439 0.0507 0.1591 ...
  ..- attr(*, "names")= chr [1:28] "Amino Acids and Derivatives" "Carbohydrates" "Cell Division and Cell Cycle" "Cell Wall and Capsule" ...
 $ mean     :List of 2
  ..$ 0: num [1:28] 135113 165454 22660 45526 225689 ...
  ..$ 1: num [1:28] 98705 120280 16522 34696 167773 ...
 $ sd       :List of 2
  ..$ 0: num [1:28] 148565 181621 26300 48946 250248 ...
  ..$ 1: num [1:28] 101669 123878 17503 35566 172331 ...
List of 5
 $ groups   : Factor w/ 2 levels "0","1": 2 1 2 1 2 1 2 1 2 1 ...
  ..- attr(*, "names")= chr [1:16] "mgm4575333.3" "mgm4575334.3" "mgm4575335.3" "mgm4575336.3" ...
 $ statistic: Named num [1:3] 1.867 0.397 1.591
  ..- attr(*, "names")= chr [1:3] "Archaea" "Bacteria" "Eukaryota"
 $ p.value  : Named num [1:3] 0.172 0.529 0.207
  ..- attr(*, "names")= chr [1:3] "Archaea" "Bacteria" "Eukaryota"
 $ mean     :List of 2
  ..$ 0: num [1:3] 1760 149028 986
  ..$ 1: num [1:3] 2439.1 127268.2 96.4
 $ sd       :List of 2
  ..$ 0: num [1:3] 3447 65606 2221
  ..$ 1: num [1:3] 5492 54871 111
List of 5
 $ groups   : Factor w/ 3 levels "0","1","2": 2 3 1 2 3 1 2 3 1 2 ...
  ..- attr(*, "names")= chr [1:16] "mgm4575333.3" "mgm4575334.3" "mgm4575335.3" "mgm4575336.3" ...
 $ statistic: Named num [1:3] 0.00405 0.12353 2.73343
  ..- attr(*, "names")= chr [1:3] "Archaea" "Bacteria" "Eukaryota"
 $ p.value  : Named num [1:3] 0.998 0.94 0.255
  ..- attr(*, "names")= chr [1:3] "Archaea" "Bacteria" "Eukaryota"
 $ mean     :List of 3
  ..$ 0: num [1:3] 1460 142215 1459
  ..$ 1: num [1:3] 2746 138660 152
  ..$ 2: num [1:3] 1963 133467 90
 $ sd       :List of 3
  ..$ 0: num [1:3] 2163 81958 2797
  ..$ 1: num [1:3] 6444 51846 231
  ..$ 2: num [1:3] 4036 56117 140
List of 5
 $ groups   : Factor w/ 2 levels "0","1": 2 1 2 1 2 1 2 1 2 1 ...
  ..- attr(*, "names")= chr [1:16] "mgm4575333.3" "mgm4575334.3" "mgm4575335.3" "mgm4575336.3" ...
 $ statistic: Named num [1:3] -0.296 0.72 1.131
  ..- attr(*, "names")= chr [1:3] "Archaea" "Bacteria" "Eukaryota"
 $ p.value  : Named num [1:3] 0.772 0.484 0.295
  ..- attr(*, "names")= chr [1:3] "Archaea" "Bacteria" "Eukaryota"
 $ mean     :List of 2
  ..$ 0: num [1:3] 1760 149028 986
  ..$ 1: num [1:3] 2439.1 127268.2 96.4
 $ sd       :List of 2
  ..$ 0: num [1:3] 3447 65606 2221
  ..$ 1: num [1:3] 5492 54871 111
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, ...) :
  cannot compute exact p-value with ties
List of 5
 $ groups   : Factor w/ 2 levels "0","1": 2 1 2 1 2 1 2 1 2 1 ...
  ..- attr(*, "names")= chr [1:16] "mgm4575333.3" "mgm4575334.3" "mgm4575335.3" "mgm4575336.3" ...
 $ statistic: Named num [1:3] 19 38 44
  ..- attr(*, "names")= chr [1:3] "Archaea" "Bacteria" "Eukaryota"
 $ p.value  : Named num [1:3] 0.189 0.574 0.227
  ..- attr(*, "names")= chr [1:3] "Archaea" "Bacteria" "Eukaryota"
 $ mean     :List of 2
  ..$ 0: num [1:3] 1760 149028 986
  ..$ 1: num [1:3] 2439.1 127268.2 96.4
 $ sd       :List of 2
  ..$ 0: num [1:3] 3447 65606 2221
  ..$ 1: num [1:3] 5492 54871 111
List of 5
 $ groups   : Factor w/ 2 levels "0","1": 2 1 2 1 2 1 2 1 2 1 ...
  ..- attr(*, "names")= chr [1:16] "mgm4575333.3" "mgm4575334.3" "mgm4575335.3" "mgm4575336.3" ...
 $ statistic: Named num [1:3] 0.0877 0.5178 1.28
  ..- attr(*, "names")= chr [1:3] "Archaea" "Bacteria" "Eukaryota"
 $ p.value  : Named num [1:3] 0.771 0.484 0.277
  ..- attr(*, "names")= chr [1:3] "Archaea" "Bacteria" "Eukaryota"
 $ mean     :List of 2
  ..$ 0: num [1:3] 1760 149028 986
  ..$ 1: num [1:3] 2439.1 127268.2 96.4
 $ sd       :List of 2
  ..$ 0: num [1:3] 3447 65606 2221
  ..$ 1: num [1:3] 5492 54871 111
List of 5
 $ groups   : Factor w/ 3 levels "0","1","2": 2 3 1 2 3 1 2 3 1 2 ...
  ..- attr(*, "names")= chr [1:16] "mgm4575333.3" "mgm4575334.3" "mgm4575335.3" "mgm4575336.3" ...
 $ statistic: Named num [1:3] 0.0299 0.0506 1.9808
  ..- attr(*, "names")= chr [1:3] "Archaea" "Bacteria" "Eukaryota"
 $ p.value  : Named num [1:3] 0.865 0.825 0.181
  ..- attr(*, "names")= chr [1:3] "Archaea" "Bacteria" "Eukaryota"
 $ mean     :List of 3
  ..$ 0: num [1:3] 1460 142215 1459
  ..$ 1: num [1:3] 2746 138660 152
  ..$ 2: num [1:3] 1963 133467 90
 $ sd       :List of 3
  ..$ 0: num [1:3] 2163 81958 2797
  ..$ 1: num [1:3] 6444 51846 231
  ..$ 2: num [1:3] 4036 56117 140
List of 5
 $ groups   : Factor w/ 2 levels "0","1": 2 1 2 1 2 1 2 1 2 1 ...
  ..- attr(*, "names")= chr [1:16] "mgm4575333.3" "mgm4575334.3" "mgm4575335.3" "mgm4575336.3" ...
 $ statistic: Named num [1:3] -0.272 1.204 1.115
  ..- attr(*, "names")= chr [1:3] "Archaea" "Bacteria" "Eukaryota"
 $ p.value  : Named num [1:3] 0.793 0.268 0.302
  ..- attr(*, "names")= chr [1:3] "Archaea" "Bacteria" "Eukaryota"
 $ mean     :List of 2
  ..$ 0: num [1:3] 1760 149028 986
  ..$ 1: num [1:3] 2439.1 127268.2 96.4
 $ sd       :List of 2
  ..$ 0: num [1:3] 3447 65606 2221
  ..$ 1: num [1:3] 5492 54871 111
Warning in wilcox.test.default(r[i == 1], r[i == 2], exact = TRUE, paired = TRUE,  :
  cannot compute exact p-value with ties
List of 5
 $ groups   : Factor w/ 2 levels "0","1": 2 1 2 1 2 1 2 1 2 1 ...
  ..- attr(*, "names")= chr [1:16] "mgm4575333.3" "mgm4575334.3" "mgm4575335.3" "mgm4575336.3" ...
 $ statistic: Named num [1:3] 13 24 29.5
  ..- attr(*, "names")= chr [1:3] "Archaea" "Bacteria" "Eukaryota"
 $ p.value  : Named num [1:3] 0.547 0.461 0.123
  ..- attr(*, "names")= chr [1:3] "Archaea" "Bacteria" "Eukaryota"
 $ mean     :List of 2
  ..$ 0: num [1:3] 1760 149028 986
  ..$ 1: num [1:3] 2439.1 127268.2 96.4
 $ sd       :List of 2
  ..$ 0: num [1:3] 3447 65606 2221
  ..$ 1: num [1:3] 5492 54871 111
> 
> for (xx in List) {
+ #-----------------------------------------------------------------------------------------
+ #  transform()				
+ #-----------------------------------------------------------------------------------------
+ 	transform (xx, t_NA2Zero)
+ 	transform (xx, t_NA2Zero, t_Threshold)
+ 	transform (xx, t_NA2Zero, t_Threshold = list(entry=3))
+ 	transform (xx, t_NA2Zero, t_Threshold = list(row=6))
+ 	transform (xx, t_NA2Zero, t_Threshold = list(row=6,col=9))
+ 	transform (xx, t_NA2Zero, t_Threshold = list(entry=5))
+ 	transform (xx, t_NA2Zero, t_Threshold = list(entry=5), t_Log)
+ 	transform (xx, t_NA2Zero, t_Threshold = list(entry=5), t_Log, t_ColCenter)
+ }
> 
> for (xx in List) {
+ #-----------------------------------------------------------------------------------------
+ #  boxplot()
+ #-----------------------------------------------------------------------------------------
+ 	xx.normed <- transform (xx, t_Log)
+ 	boxplot(xx)
+ 	boxplot(
+ 		xx, 
+ 		xx,
+ 		main="so good they named it twice")
+ 	boxplot(
+ 		xx, 
+ 		xx.normed)
+ 	boxplot( 
+ 		xx, 
+ 		xx.normed,
+ 		columns=2:4)
+ 	boxplot(
+ 		xx, 
+ 		xx.normed,
+ 		x.main="raw",
+ 		y.main="log")
+ 	boxplot(
+ 		xx,
+ 		xx.normed,
+ 		x.main="raw",
+ 		y.main="log",
+ 		cex.main=2,
+ 		x.names="$$project.id",
+ 		x.cex.axis=1.5,
+ 		y.names="$$metagenome.id",
+ 		y.cex.axis=0.75)
+ }
> xx <- xx1 ; xx.normed <- transform (xx, t_Log)
> boxplot(
+ 	xx,
+ 	xx.normed,
+ 	map=c(
+ 		col="host_common_name"))
> boxplot(
+ 	xx, 
+ 	xx.normed,
+ 	x.main="raw",
+ 	y.main="log",
+ 	map=c(
+ 		col="host_common_name"))
> boxplot(
+ 	xx.normed,
+ 	xx.normed,
+ 	x.main="log",
+ 	y.main="log",
+ 	map=c(
+ 		x.col="host_common_name",
+ 		y.col="samp_stor"),
+ 	y.col=c(
+ 		"-80"="salmon",
+ 		"NA"="orange"))
> 
> #-----------------------------------------------------------------------------------------
> #  princomp()				
> #-----------------------------------------------------------------------------------------
> princomp (xx1, method="euclidean")
> princomp (xx1, method="bray-curtis")
> 
> princomp (xx1, dim=1)											# single PC
> princomp (xx1, dim=2)
> princomp (xx1, dim=c(1,2))										# two PCs
> princomp (xx1, dim=c(2,3))
> princomp (xx1, dim=c(1,2,3))									# same three PCs
> princomp (xx1, dim=c(1,3,2))									# from different perspectives
> princomp (xx1, dim=c(2,1,3))
> princomp (xx1, dim=c(2,3,4))									# different three PCs
> 
> princomp (xx1, labels = "")										# labeling variations (color, size, metadata)
> princomp (xx1, labels = LETTERS [1:7])
> princomp (xx1, labels = LETTERS [1:7], label.col = "blue")
> princomp (xx1, labels = "$$host_common_name")
> princomp(
+ 	xx1, 
+ 	labels="$$pubmed_id",
+ 	label.col="blue",
+ 	label.cex=0.5)
> # princomp(
> # 	xx1,
> # 	labels="$$pubmed_id", 
> # 	map=c(
> # 		label.col="host_common_name"))
> # princomp(
> # 	xx1,
> # 	labels="$$pubmed_id", 
> # 	map=c(
> # 		label.col="host_common_name"),
> # 	label.col=c(
> # 		"cow"="blue",
> # 		"striped bass"="brown",
> # 		"Mouse"="brown"))
> princomp (xx3, dim=3, labels="", map=c(col="biome"))
> 
> princomp(														# plotting character variations
+ 	xx1,
+ 	col="blue")
> princomp(
+ 	xx1,
+ 	col=c("blue","blue","blue","red","red","red","red"))
> princomp(
+ 	xx1,
+ 	pch=17)
> princomp(
+ 	xx1,
+ 	pch=15:21)
> princomp(
+ 	xx1,
+ 	cex=2)
> princomp(
+ 	xx1,
+ 	cex=seq(1,2,len=7))
> 
> princomp(
+ 	xx1, 
+ 	map=c(												# automap one par variable to metadata
+ 		pch="samp_store_temp"))
> princomp(
+ 	xx1, 
+ 	map=c(												# automap two
+ 		col="host_common_name",
+ 		pch="samp_store_temp"))
> princomp(
+ 	xx1, 
+ 	map=c(												# automap three
+ 		col="host_common_name",
+ 		pch="samp_store_temp",
+ 		cex="material"))
> princomp (xx1,
+ 	map=c(												# explicitly map one (of two)
+ 		col="host_common_name",
+ 		pch="samp_store_temp"),
+ 	col=c(
+ 		Mouse="brown",
+ 		cow="red",
+ 		"striped bass"="blue"))
> princomp (xx1,
+ 	map=c(												# explicitly map both
+ 		col="host_common_name",
+ 		pch="samp_store_temp"),
+ 	col=c(
+ 		Mouse="brown",
+ 		cow="red",
+ 		"striped bass"="blue"),
+ 	pch=c(
+ 		"-80"="+",
+ 		"NA"="x"),
+ 	cex=2)
> princomp(
+ 	xx1,
+ 	dim=1:2,
+ 	map=c(												# give explicit but incomplete map
+ 		cex="host_common_name"),
+ 	cex=c(
+ 		cow=2.5),
+ 	labels="")
> 
> zz <- princomp (xx1)									# reuse computation
> princomp(
+ 	xx1, 
+ 	main="title added with\nno redundant calculation", 
+ 	rerender=zz)
> yy <- distx(xx1)
> princomp(												# reuse computation of distance, only
+ 	xx1, 
+ 	main="a distance computation\ncan be reused too", 
+ 	rerender=yy)
> princomp(												# restrict columns analyzed
+ 	xx1, 
+ 	columns= 
+ 		("cow" == columns(xx1, "host_common_name")[,1]))
> princomp(
+ 	xx1,												# restrict rows analyzed
+ 	rows= 
+ 		("Carbohydrates" == rows (xx1,"ontology1")[,1]))
> princomp(
+ 	xx1, 												# push 3d plot to margins and change persp
+ 	labels="$$project.id",								# with scatterplot3d pars
+ 	map=c(col="host_common_name", pch="samp_store_temp"),
+ 	col=c(Mouse="blue", cow="red", "striped bass"="brown"),
+ 	pch=c("-80"="+",`NA`="x"),
+ 	cex=2,
+ 	angle=20,
+ 	mar=c(1,1,0,0))
> princomp(												# label refinement...
+ 	xx1, dim=c(1,2),
+ 	map = c (col="host_common_name", pch="samp_store_temp"),
+ 	col = c (Mouse="brown", cow="red", "striped bass"="blue"),
+ 	pch = c ("-80"="+","NA"="*"),
+ 	cex=2,
+ 	label.font=3, 										# ...italic and
+ 	label.pos=c(1,4,2,2,2,2,4))							# repositioned to stay within box
> princomp(
+ 	biom(li4),
+ 	dim=3:1,
+ 	map=c(												# final example with different data
+ 		pch="data.age",
+ 		col="body_site"),
+ 	pch=c(
+ 		"39 ; Year" = 'z',
+ 		"36 ; Year" = 'y',
+ 		"23 ; Year" = 'x'),
+ 	col=c(
+ 		"Teeth surfaces" = "blue"),
+ 	cex=2,
+ 	angle=30,
+ 	box=TRUE,
+ 	box.lty="dashed",
+ 	mar=c(1,1,0,0))
Warning in title(main, sub, ...) :
  "box.lty" is not a graphical parameter
Warning in segments(x, z, x, z2, col = col, cex = cex.symbols, lty = lty.hplot,  :
  "box.lty" is not a graphical parameter
Warning in plot.xy(xy.coords(x, y), type = type, ...) :
  "box.lty" is not a graphical parameter
> 
> #-----------------------------------------------------------------------------------------
> #  image()				
> #-----------------------------------------------------------------------------------------
> xx1.log <- transform (xx1, t_Log)
> xx2.log <- transform (xx2, t_Log)
> image(
+ 	xx1.log,
+ 	margins=c(6,13),
+ 	lwid=c(1,1.75), lhei=c(1,10),
+ 	cexRow=0.3, cexCol=0.8)
> image(
+ 	xx2.log,
+ 	margins=c(6,6),
+ 	lwid=c(1,2.5), lhei=c(1,10),
+ 	cexRow=0.5, cexCol=0.8)
> image(
+ 	xx2.log,
+ 	margins=c(9,6),
+ 	lwid=c(1,2.5), lhei=c(1,10),
+ 	cexRow=0.5, cexCol=0.8,
+ 	labCol="$$material")
> image(
+ 	xx2.log,
+ 	margins=c(4,6),
+ 	lwid=c(1,2.5), lhei=c(1,10),
+ 	cexRow=0.5, cexCol=0.8,
+ 	labCol="$$project.id")
>  
> zz <- image (xx1.log)
> image (xx1.log, 											# is this working?
+ 	main = "title added without recompute",
+ 	margins=c(5,5),
+ 	lhei=c(1,3), lwid=c(1,3),
+ 	labRow=NA,
+ 	rerender=zz)
> 
> image (xx1.log, 											# row subselection
+ 	rows = (rows(xx1,"ontology1")[[1]] == "Clustering-based subsystems"),
+ 	labRow="$$ontology2",
+ 	lwid=c(1,3),
+ 	cexRow=0.5,
+ 	margins=c(5,10))
> 
> image (xx1.log, columns = c(1,2,4))							# column subselection
> 
> image (xx1.log, labCol=letters[1:7])
> image (xx1.log, labCol = "$$data.age")
> image (xx1.log, labCol=columns(xx1, "data.age") [[1]])		# same as previous
> 
> image (xx1.log, rows=1:20, labRow=1:20)
> image (xx1.log, labRow="$$ontology1")
> image (xx1.log, labRow=rows(xx1, "ontology1")[[1]])			# same as previous
> 
> image(														# no dendrograms
+ 	xx2.log,
+ 	dendrogram='none',
+ 	lwid=c(1,5), lhei=c(1,10),
+ 	margins=c(5,7))
> 
> proc.time()
   user  system elapsed 
   8.29    0.18    8.71 
