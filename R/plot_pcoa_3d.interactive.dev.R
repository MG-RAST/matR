## plot_pcoa_3d.interactive.dev <- function(
##                          file_in,
##                       #groups_in,
##                          out_prefix = "Zhou_data_test",
##                          my_colors = c('red'),
##                          legend_colors = c('red'),
##                       #names(legend_colors) <- c('red'),
##                          figure_width = 10,
##                          figure_height = 10,
##                          figure_res = NA,
##                          debug = FALSE,
##                          PC1 = 1,
##                          PC2 = 2,
##                          PC3 = NA,
##                          method_3d = "scatterplot3d"                         
##                          )
##   {

##     ####################### CUSTOM SETTINGS #######################
 
##     ## my_colors <- c(
##     ##   "1CC.C082"="red",
##     ##   "1CC.C085"="red",
##     ##   "1CC.C156"="red",
##     ##   "1CW.C089"="red",
##     ##   "1CW.C096"="red",
##     ##   "1CW.C136"="red",
##     ##   "1UC.C141"="red",
##     ##   "1UC.C149"="red",
##     ##   "1UW.C153"="red",
##     ##   "2CC.C139"="red",
##     ##   "2CC.C154"="blue",
##     ##   "2CW.C160"="blue",
##     ##   "2UC.C074"="blue",
##     ##   "2UC.C091"="blue",
##     ##   "2UC.C093"="blue",
##     ##   "2UW.C100"="blue",
##     ##   "2UW.C126"="blue",
##     ##   "2UW.C147"="blue",
##     ##   "3CC.C081"="green",
##     ##   "3CC.C086"="green",
##     ##   "3CC.C116"="green",
##     ##   "3CW.C090"="green",
##     ##   "3CW.C102"="green",
##     ##   "3CW.C151"="green",
##     ##   "3UC.C066"="green",
##     ##   "3UC.C140"="green",
##     ##   "3UW.C101"="green",
##     ##   "3UW.C143"="green",
##     ##   "4CC.C125"="magenta",
##     ##   "4CC.C150"="magenta",
##     ##   "4CW.C084"="magenta",
##     ##   "4CW.C130"="magenta",
##     ##   "4UC.C099"="magenta",
##     ##   "4UC.C159"="magenta",
##     ##   "4UW.C077"="magenta",
##     ##   "4UW.C128"="magenta",
##     ##   "4UW.C144"="magenta",
##     ##   "5CC.C157"="orange",
##     ##   "5CC.C161"="orange",
##     ##   "5CW.C075"="orange",
##     ##   "5CW.C088"="orange",
##     ##   "5CW.C132"="orange",
##     ##   "5UC.C146"="orange",
##     ##   "5UC.C162"="orange",
##     ##   "5UW.C070"="orange",
##     ##   "5UW.C098"="orange",
##     ##   "6CC.C026"="cyan",
##     ##   "6CC.C134"="cyan",
##     ##   "6CW.C137"="cyan",
##     ##   "6CW.C145"="cyan",
##     ##   "6UC.C079"="cyan",
##     ##   "6UC.C083"="cyan",
##     ##   "6UC.C131"="cyan",
##     ##   "6UW.C095"="cyan",
##     ##   "6UW.C127"="cyan",
##     ##   "6UW.C158"="cyan"
##     ##   )
    
##     ## legend_colors <- c ("red", "blue", "green", "magenta", "orange", "cyan")
    
##     ## names (legend_colors) <- c ("1","2","3","4","5","6")

##     ## my_pch <- c(
##     ##          "1CC.C082"=21,
##     ##          "1CC.C085"=21,
##     ##          "1CC.C156"=21,
##     ##          "1CW.C089"=22,
##     ##          "1CW.C096"=22,
##     ##          "1CW.C136"=22,
##     ##          "1UC.C141"=23,
##     ##          "1UC.C149"=23,
##     ##          "1UW.C153"=24,
##     ##          "2CC.C139"=21,
##     ##          "2CC.C154"=21,
##     ##          "2CW.C160"=22,
##     ##          "2UC.C074"=23,
##     ##          "2UC.C091"=23,
##     ##          "2UC.C093"=23,
##     ##          "2UW.C100"=24,
##     ##          "2UW.C126"=24,
##     ##          "2UW.C147"=24,
##     ##          "3CC.C081"=21,
##     ##          "3CC.C086"=21,
##     ##          "3CC.C116"=21,
##     ##          "3CW.C090"=22,
##     ##          "3CW.C102"=22,
##     ##          "3CW.C151"=22,
##     ##          "3UC.C066"=23,
##     ##          "3UC.C140"=23,
##     ##          "3UW.C101"=24,
##     ##          "3UW.C143"=24,
##     ##          "4CC.C125"=21,
##     ##          "4CC.C150"=21,
##     ##          "4CW.C084"=21,
##     ##          "4CW.C130"=22,
##     ##          "4UC.C099"=23,
##     ##          "4UC.C159"=23,
##     ##          "4UW.C077"=24,
##     ##          "4UW.C128"=24,
##     ##          "4UW.C144"=24,
##     ##          "5CC.C157"=21,
##     ##          "5CC.C161"=21,
##     ##          "5CW.C075"=22,
##     ##          "5CW.C088"=22,
##     ##          "5CW.C132"=22,
##     ##          "5UC.C146"=23,
##     ##          "5UC.C162"=23,
##     ##          "5UW.C070"=24,
##     ##          "5UW.C098"=24,
##     ##          "6CC.C026"=21,
##     ##          "6CC.C134"=21,
##     ##          "6CW.C137"=22,
##     ##          "6CW.C145"=22,
##     ##          "6UC.C079"=23,
##     ##          "6UC.C083"=23,
##     ##          "6UC.C131"=23,
##     ##          "6UW.C095"=24,
##     ##          "6UW.C127"=24,
##     ##          "6UW.C158"=24
##     ##          )
    
##     my_colors <- c(
##       "1CC.C082"="red",
##       "1CC.C085"="red",
##       "1CC.C156"="red",
##       "1CW.C089"="blue",
##       "1CW.C096"="blue",
##       "1CW.C136"="blue",
##       "1UC.C141"="green",
##       "1UC.C149"="green",
##       "1UW.C153"="magenta",
##       "2CC.C139"="red",
##       "2CC.C154"="red",
##       "2CW.C160"="blue",
##       "2UC.C074"="green",
##       "2UC.C091"="green",
##       "2UC.C093"="green",
##       "2UW.C100"="magenta",
##       "2UW.C126"="magenta",
##       "2UW.C147"="magenta",
##       "3CC.C081"="red",
##       "3CC.C086"="red",
##       "3CC.C116"="red",
##       "3CW.C090"="blue",
##       "3CW.C102"="blue",
##       "3CW.C151"="blue",
##       "3UC.C066"="green",
##       "3UC.C140"="green",
##       "3UW.C101"="magenta",
##       "3UW.C143"="magenta",
##       "4CC.C125"="red",
##       "4CC.C150"="red",
##       "4CW.C084"="blue",
##       "4CW.C130"="blue",
##       "4UC.C099"="green",
##       "4UC.C159"="green",
##       "4UW.C077"="magenta",
##       "4UW.C128"="magenta",
##       "4UW.C144"="magenta",
##       "5CC.C157"="red",
##       "5CC.C161"="red",
##       "5CW.C075"="blue",
##       "5CW.C088"="blue",
##       "5CW.C132"="blue",
##       "5UC.C146"="green",
##       "5UC.C162"="green",
##       "5UW.C070"="magenta",
##       "5UW.C098"="magenta",
##       "6CC.C026"="red",
##       "6CC.C134"="red",
##       "6CW.C137"="blue",
##       "6CW.C145"="blue",
##       "6UC.C079"="green",
##       "6UC.C083"="green",
##       "6UC.C131"="green",
##       "6UW.C095"="magenta",
##       "6UW.C127"="magenta",
##       "6UW.C158"="magenta"
##       )

##     my_pch = 21
    
##     legend_colors <- c ("red", "blue", "green", "magenta")
    
##     names (legend_colors) <- c ("CC","CW","UC","UW")
    
##     ## my_colors <- c(
##     ##                "Ca"="green",
##     ##                "Cb"="green",
##     ##                "Cc"="green",
##     ##                "Cd"="green",
##     ##                "Ce"="green",
##     ##                "Ch1a"="purple",
##     ##                "Ch1b"="purple",
##     ##                "Ch1c"="purple",
##     ##                "Ch1d"="purple",
##     ##                "Ch1e"="purple",
##     ##                "Ch2a"="purple",
##     ##                "Ch2b"="purple",
##     ##                "Ch2c"="purple",
##     ##                "Ch2d"="purple",
##     ##                "Ch2e"="purple",
##     ##                "Ch3a"="purple",
##     ##                "Ch3b"="purple",
##     ##                "Ch3c"="purple",
##     ##                "Ch3d"="purple",
##     ##                "Ch3e"="purple",
##     ##                "Ch4a"="purple",
##     ##                "Ch4b"="purple",
##     ##                "Ch4c"="purple",
##     ##                "Ch4d"="purple",
##     ##                "Ch4e"="purple",
##     ##                "Ch5a"="purple",
##     ##                "Ch5b"="purple",
##     ##                "Ch5c"="purple",
##     ##                "Ch5d"="purple",
##     ##                "Ch5e"="purple",
##     ##                "E1a"="blue",
##     ##                "E1b"="blue",
##     ##                "E1c"="blue",
##     ##                "E1d"="blue",
##     ##                "E1e"="blue",
##     ##                "Ra"="orange",
##     ##                "Rb"="orange",
##     ##                "Rc"="orange",
##     ##                "Rd"="orange",
##     ##                "Re"="orange",
##     ##                "Wa"="brown",
##     ##                "Wb"="brown",
##     ##                "Wc"="brown",
##     ##                "Wd"="brown",
##     ##                "We"="brown"
##     ##                )
    
##     ## legend_colors <- c ("blue", "green", "purple", "orange", "brown")

##     ## names (legend_colors) <- c ("E1","C","Ch","R", "W")

##      # OLD VERSION THAT WORKS
##       ## points(x=((sorted_my_data[,PC1])[1:5]), y=((sorted_my_data[,PC2])[1:5]), pch=23, col="green", bg = "green", cex=my_cex) #C
##       ## points([6:10]), y=((sorted_my_data[,PC2])[6:10]), pch=21, col="purple", bg = "purple", cex=my_cex) #ch1
##       ## points(x=((sorted_my_data[,PC1])[11:15]), y=((sorted_my_data[,PC2])[11:15]), pch=22, col="purple", bg = "purple", cex=my_cex) #ch2
##       ## points(x=((sorted_my_data[,PC1])[16:20]), y=((sorted_my_data[,PC2])[16:20]), pch=23, col="purple", bg = "purple", cex=my_cex) #ch3
##       ## points(x=((sorted_my_data[,PC1])[21:25]), y=((sorted_my_data[,PC2])[21:25]), pch=24, col="purple", bg = "purple", cex=my_cex) #ch4
##       ## points(x=((sorted_my_data[,PC1])[26:30]), y=((sorted_my_data[,PC2])[26:30]), pch=25, col="purple", bg = "purple", cex=my_cex) #ch5
##       ## points(x=((sorted_my_data[,PC1])[31:35]), y=((sorted_my_data[,PC2])[31:35]), pch=21, col="blue", bg = "blue", cex=my_cex) #E1
##       ## points(x=((sorted_my_data[,PC1])[36:40]), y=((sorted_my_data[,PC2])[36:40]), pch="X", col="orange", bg = "orange", cex=0.7*my_cex, lwd=3) #R
##       ## points(x=((sorted_my_data[,PC1])[41:45]), y=((sorted_my_data[,PC2])[41:45]), pch="+", col="brown", bg = "brown", cex=my_cex, lwd=3) #W




    
##     ###############################################################

##     if(debug==TRUE){print("made it here 1")}
##     ######################################################### MAIN #########################################################
##     # read file_in into a list
##     con <- file(file_in)
##     num_lines <- length(readLines(con))
##     if(debug==TRUE){print(paste("num_lines:", num_lines))}
##     open(con)
##     results.list <<- list();
##     current.line <- 1
##     while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
##       results.list[[current.line]] <<- line
##       current.line <- current.line + 1
##     } 
##     close(con)    

##     # parse file_in into Eigen values and Eigen vectors
##     num_eigen_values <- length(grep("PC", results.list)) # need to figure out how to do emty line regex in R ...
    
##     eigen_value.list <<- vector("list", num_eigen_values)
##     eigen_vector.list <<- vector("list", num_eigen_values)
##     value.count <- 1
##     vector.count <- 1

##     if(debug==TRUE){print("made it here 2")}
    
##     for (i in 1:num_lines){
##       my_line <<- noquote(results.list[i])
      
##       if (any(grep("^#", my_line))==TRUE){  
##       }else{
##         if (any(grep("PC", my_line))==TRUE){ # pull out the eigen value
##           eigen_value.list[value.count] <<- my_line 
##           value.count <- value.count + 1
##           if(debug==TRUE){print(paste("PC", my_line))}
##         }else if (any(grep("[0-9]", my_line))==TRUE){ # pull out the eigen vectors
##           eigen_vector.list[vector.count] <<- my_line
##           vector.count <- vector.count + 1
##         }else{print("encountered empty variable")}
        
##       }
##     }

##     if(debug==TRUE){print("made it here 3")}

##     # Split list of eigen vectors into a vector
##     vector_list <<- strsplit(as.character(eigen_vector.list), split="\t")
    
##     my_names <<- vector("list", num_eigen_values)
##     for (i in 1:num_eigen_values){
##       my_names[i] <<- vector_list[[i]][1]
##     }
    
##     my_data <<- matrix(ncol = num_eigen_values, nrow = num_eigen_values) 
##     for (i in 1:num_eigen_values){
##       for (j in 1:num_eigen_values){
##         my_data[i,j] <<- vector_list[[i]][j+1]
##       }
##     }

##     if(debug==TRUE){print("made it here 4")}
    
##     rownames(my_data) <<- my_names

##     eigen_value.names <<- vector("list", num_eigen_values)
##     for (i in 1:num_eigen_values){
##       pcName_pcEigen <- unlist(strsplit(as.character(eigen_value.list[i]), split="\t"))
##       pcName <- gsub("\"", "", (pcName_pcEigen[1]))
##       pcEigen <- gsub(" ", "", paste(  (round( as.numeric(pcName_pcEigen[2]), digits = 4))*100, "%"))
##       eigen_value.names[i] <<- paste( pcName, "::", pcEigen, "of observed variation")
##     } 

##     if(debug==TRUE){print("made it here 5")}
    
##     sorted_my_data <- my_data[order(rownames(my_data)), ]

##     colnames(sorted_my_data) <- eigen_value.names

##     write.table(sorted_my_data, file = "PCoA_sorted_data.txt", col.names=NA, row.names = TRUE, sep="\t", quote=FALSE)
##     # use this table to double check point/ color ordering

##     if(debug==TRUE){print("made it here 6")}
    
##     my_sample_names <- as.vector( gsub("\"", "", rownames(sorted_my_data)), mode="character")
##     my_ordered_colors <- ( my_colors[ my_sample_names[1:length(my_sample_names)] ])

##     if (  length(my_pch) > 1 ){
##       my_ordered_pch <-  ( my_pch[ my_sample_names[1:length(my_sample_names)] ])
##     }else{
##       my_ordered_pch <- my_pch
##     }

##     if(debug==TRUE){print(my_sample_names)}
##     if(debug==TRUE){print(my_ordered_colors)}
    

##     if(debug==TRUE){print("made it here 7")}
      
##     ########################### PLOT 2d  ##########################
##     if(identical(PC3, NA)){ # plot in 2d

##       image_out <- gsub(" ", "", (paste(out_prefix, ".2d_plot.pdf")))
##       legend_out <- gsub(" ", "", (paste(out_prefix, ".legend.pdf")))

##       if(debug==TRUE){print("made it here 8")}
      
##       pdf(file=image_out, width = figure_width, height = figure_height)

##       plot(
##            x = sorted_my_data[,PC1],
##            y = sorted_my_data[,PC2],
##            type="n",
##            xlab = eigen_value.names[PC1],
##            ylab = eigen_value.names[PC2],
##            cex = 0.8
##            )
##       my_cex <- 1.2

##       if(debug==TRUE){print("made it here 9")}

##       points(
##              x=(sorted_my_data[,PC1]),
##              y=(sorted_my_data[,PC2]),
##              pch=my_ordered_pch,
##              col=my_ordered_colors,
##              bg=my_ordered_colors,
##              cex=my_cex
##              )
      
##       if(debug==TRUE){print("made it here 10")}
     
##       title( (paste(out_prefix,"\n", "PC", PC1, "vs PC", PC2 )), cex.main = 0.8)
##       dev.off()

##       if(debug==TRUE){print("made it here 11")}
      
##       # produce a speparte figure with the legend
##       pdf (file=legend_out, width = 2, height = 4)
##       plot.new ()
##       legend (0, 1, legend = names(legend_colors), pch = 18, col = legend_colors )
##       dev.off ()
      
##     }
##     ###############################################################


##     ########################### PLOT 3d  ##########################

##     if(!(identical(PC3, NA))){ # create 3d rendering with scatterplot3d (pdf) or rgl (interactive image)

##       image_out <- gsub(" ", "", (paste(out_prefix, ".3d_plot.pdf")))
##       legend_out <- gsub(" ", "", (paste(out_prefix, ".legend.pdf")))
      
##       if(identical(method_3d, "scatterplot3d")){
        
##         require(scatterplot3d)

##         # scaling for the axes -- autoscaling stinks
##         x_range <- as.numeric(range(sorted_my_data[,PC1])[2]) - as.numeric(range(sorted_my_data[,PC1])[1])
##         x_lim_max <- ( as.numeric(range(sorted_my_data[,PC1])[2]) + 0.25*x_range)
##         x_lim_min <- ( as.numeric(range(sorted_my_data[,PC1])[1]) - 0.25*x_range)

##         y_range <- as.numeric(range(sorted_my_data[,PC2])[2]) - as.numeric(range(sorted_my_data[,PC2])[1])
##         y_lim_max <- ( as.numeric(range(sorted_my_data[,PC2])[2]) + 0.25*y_range)
##         y_lim_min <- ( as.numeric(range(sorted_my_data[,PC2])[1]) - 0.25*y_range)

##         z_range <- as.numeric(range(sorted_my_data[,PC3])[2]) - as.numeric(range(sorted_my_data[,PC3])[1])
##         z_lim_max <- ( as.numeric(range(sorted_my_data[,PC3])[2]) + 0.25*z_range)
##         z_lim_min <- ( as.numeric(range(sorted_my_data[,PC3])[1]) - 0.25*z_range)
                             
        
##         pdf(file=image_out, width = figure_width, height = figure_height)
##         my_3d_plot <- scatterplot3d(
##                                     x<-sorted_my_data[,PC1],
##                                     #xlim = c( x_lim_min, x_lim_max ),
##                                     y<-sorted_my_data[,PC2],
##                                     #ylim = c( y_lim_min, y_lim_max ),
##                                     z<-sorted_my_data[,PC3],
##                                     #zlim = c( z_lim_min, z_lim_max ),
##                                     type="n",
##                                     xlab = eigen_value.names[PC1],
##                                     ylab = eigen_value.names[PC2],
##                                     zlab = eigen_value.names[PC3],
##                                     cex.axis = 0.8
##                                     )
##         my_cex <- 1.2
        
##         my_3d_plot$points3d(
##                  x=((sorted_my_data[,PC1])),
##                  y=((sorted_my_data[,PC2])),
##                  z=((sorted_my_data[,PC3])),
##                  pch=my_ordered_pch,
##                  col=my_ordered_colors,
##                  bg=my_ordered_colors,
##                  cex=my_cex
##                  )

##         ## my_3d_plot$points3d(x=((sorted_my_data[,PC1])[1:5]), y=((sorted_my_data[,PC2])[1:5]), z=((sorted_my_data[,PC3])[1:5]), pch=23, col="green", bg = "green", cex=my_cex)#C
##         ## my_3d_plot$points3d(x=((sorted_my_data[,PC1])[6:10]), y=((sorted_my_data[,PC2])[6:10]), z=((sorted_my_data[,PC3])[1:5]), pch=21, col="purple", bg = "purple", cex=my_cex) #ch1
##         ## my_3d_plot$points3d(x=((sorted_my_data[,PC1])[11:15]), y=((sorted_my_data[,PC2])[11:15]), z=((sorted_my_data[,PC3])[1:5]), pch=22, col="purple", bg = "purple", cex=my_cex) #ch2
##         ## my_3d_plot$points3d(x=((sorted_my_data[,PC1])[16:20]), y=((sorted_my_data[,PC2])[16:20]), z=((sorted_my_data[,PC3])[1:5]), pch=23, col="purple", bg = "purple", cex=my_cex) #ch3
##         ## my_3d_plot$points3d(x=((sorted_my_data[,PC1])[21:25]), y=((sorted_my_data[,PC2])[21:25]), z=((sorted_my_data[,PC3])[1:5]), pch=24, col="purple", bg = "purple", cex=my_cex) #ch4
##         ## my_3d_plot$points3d(x=((sorted_my_data[,PC1])[26:30]), y=((sorted_my_data[,PC2])[26:30]), z=((sorted_my_data[,PC3])[1:5]), pch=25, col="purple", bg = "purple", cex=my_cex) #ch5
##         ## my_3d_plot$points3d(x=((sorted_my_data[,PC1])[31:35]), y=((sorted_my_data[,PC2])[31:35]), z=((sorted_my_data[,PC3])[1:5]), pch=21, col="blue", bg = "blue", cex=my_cex) #E1
##         ## my_3d_plot$points3d(x=((sorted_my_data[,PC1])[36:40]), y=((sorted_my_data[,PC2])[36:40]), z=((sorted_my_data[,PC3])[1:5]), pch="X", col="orange", bg = "orange", cex=0.7*my_cex, lwd=3) #R
##         ## my_3d_plot$points3d(x=((sorted_my_data[,PC1])[41:45]), y=((sorted_my_data[,PC2])[41:45]), z=((sorted_my_data[,PC3])[1:5]), pch="+", col="brown", bg = "brown", cex=my_cex, lwd=3) #W

##         title( (paste(out_prefix,"\n", "PC", PC1, "vs PC", PC2 , "vs PC", PC3 )), cex.main = 0.8)
##         dev.off()
        
##         pdf (file=legend_out) #, width = 2, height = 4)
        
##         plot.new ()
##         legend (0, 1, legend = names (legend_colors), pch = 18, col = legend_colors )
##         dev.off ()

##         ## scatterplot3d(x, y=NULL, z=NULL, color=par("col"), pch=NULL,
##         ##               main=NULL, sub=NULL, xlim=NULL, ylim=NULL, zlim=NULL,
##         ##               xlab=NULL, ylab=NULL, zlab=NULL, scale.y=1, angle=40,
##         ##               axis=TRUE, tick.marks=TRUE, label.tick.marks=TRUE,
##         ##               x.ticklabs=NULL, y.ticklabs=NULL, z.ticklabs=NULL,
##         ##               y.margin.add=0, grid=TRUE, box=TRUE, lab=par("lab"),
##         ##               lab.z=mean(lab[1:2]), type="p", highlight.3d=FALSE,
##         ##               mar=c(5,3,4,3)+0.1, col.axis=par("col.axis"),
##         ##               col.grid="grey", col.lab=par("col.lab"),
##         ##               cex.symbols=par("cex"), cex.axis=0.8 * par("cex.axis"),
##         ##               cex.lab=par("cex.lab"), font.axis=par("font.axis"),
##         ##               font.lab=par("font.lab"), lty.axis=par("lty"),
##         ##               lty.grid=par("lty"), lty.hide=NULL, lty.hplot=par("lty"),
##         ##               log="", ...)
##       }


##       if (identical(method_3d, "rgl")){

##         require(rgl)

##         open3d()

##         x<-sorted_my_data[,PC1]
##         y<-sorted_my_data[,PC2]
##         z<-sorted_my_data[,PC3]

##         my_colors <- c(

                       
##                        )

    
        
##         plot3d(
##                x,
##                y,
##                z,
##                #xlab = "test",
##                xlab = paste("(x)", eigen_value.names[PC1]),
##                ylab = paste("(y)", eigen_value.names[PC2]),
##                zlab = paste("(z)", eigen_value.names[PC3]),
##                col=my_colors,
##                radius = 100
##                )

##         rgl.sprites(x,y,z,radius=1000,color=cols)
        
##         #rgl.postscript("persp3dd.pdf","pdf")
        

##         ##  rgl.plot3d<-function(z, x, y, cols="red",axes=T,new=T)
##         ##   {xr<-range(x)
##         ##    x01<-(x-xr[1])/(xr[2]-xr[1])
##         ##    yr<-range(y)
##         ##    y01<-(y-yr[1])/(yr[2]-yr[1])
##         ##    zr<-range(z)
##         ##    z01<-(z-zr[1])/(zr[2]-zr[1])
           
##         ##    if(new) rgl.clear()
##         ##    if(axes)
##         ##      {xlab<-pretty(x)
##         ##       ylab<-pretty(y)
##         ##       zlab<-pretty(z)
##         ##       xat<-(xlab-xr[1])/(xr[2]-xr[1])
##         ##       yat<-(ylab-yr[1])/(yr[2]-yr[1])
##         ##       zat<-(zlab-zr[1])/(zr[2]-zr[1])
##         ##       rgl.lines(c(0,1.1),0,0)
##         ##       rgl.lines(0,c(0,1.1),0)
##         ##       rgl.lines(0,0,c(0,1.1))
##         ##       rgl.texts(xat,-.05,-.05,xlab)
##         ##       rgl.texts(-.05,yat,-.05,ylab)
##         ##       rgl.texts(-.05,-.05,zat,zlab)
##         ##       rgl.texts(c(0.5,-.15,-.15),c(-.15,.5,-.15),c(-.15,-.15,.5),
##         ##                 c(deparse(substitute(x)),deparse(substitute(y)),deparse(substitute(z))))
##         ##     }
           
##         ##    rgl.spheres(x01,y01,z01,.01,color=cols)
##         ##  }

## #sprites
##         ## rgl.plot3d(sorted_my_data[,PC3],sorted_my_data[,PC1],sorted_my_data[,PC2])
        
##         ## iris.pc<-prcomp(iris[,1:4],scale=T)
##         ## rgl.plot3d(iris.pc$x[,1],iris.pc$x[,2],iris.pc$x[,3])
##         ## # different colors
##         ## rgl.plot3d(iris.pc$x[,1],iris.pc$x[,2],iris.pc$x[,3],col=unclass(iris[,5])+1)

        
##         ## ### https://stat.ethz.ch/pipermail/r-help/2008-May/161202.html
##         ## #Install library rgl
##         ## #here is the is the function:
##         ## rgl.plot3d<-function(z, x, y, cols="red",axes=T,new=T)
##         ##   {xr<-range(x)
##         ##    x01<-(x-xr[1])/(xr[2]-xr[1])
##         ##    yr<-range(y)
##         ##    y01<-(y-yr[1])/(yr[2]-yr[1])
##         ##    zr<-range(z)
##         ##    z01<-(z-zr[1])/(zr[2]-zr[1])
           
##         ##    if(new) rgl.clear()
##         ##    if(axes)
##         ##      {xlab<-pretty(x)
##         ##       ylab<-pretty(y)
##         ##       zlab<-pretty(z)
##         ##       xat<-(xlab-xr[1])/(xr[2]-xr[1])
##         ##       yat<-(ylab-yr[1])/(yr[2]-yr[1])
##         ##       zat<-(zlab-zr[1])/(zr[2]-zr[1])
##         ##       rgl.lines(c(0,1.1),0,0)
##         ##       rgl.lines(0,c(0,1.1),0)
##         ##       rgl.lines(0,0,c(0,1.1))
##         ##       rgl.texts(xat,-.05,-.05,xlab)
##         ##       rgl.texts(-.05,yat,-.05,ylab)
##         ##       rgl.texts(-.05,-.05,zat,zlab)
##         ##       rgl.texts(c(0.5,-.15,-.15),c(-.15,.5,-.15),c(-.15,-.15,.5),
##         ##                 c(deparse(substitute(x)),deparse(substitute(y)),deparse(substitute(z))))
##         ##     }
           
##         ##    rgl.spheres(x01,y01,z01,.01,color=cols)
##         ##  }

##         ## #and here is how you call it
##         ## library(rgl)
##         ## data(iris)

##         ## iris.pc<-prcomp(iris[,1:4],scale=T)
##         ## rgl.plot3d(iris.pc$x[,1],iris.pc$x[,2],iris.pc$x[,3])
##         ## # different colors
##         ## rgl.plot3d(iris.pc$x[,1],iris.pc$x[,2],iris.pc$x[,3],col=unclass(iris[,5])+1)

##         ###########################################################################

##         ## ### https://stat.ethz.ch/pipermail/r-help/2008-May/161202.html
##         ## #Install library rgl
##         ## #here is the is the function:
##         ## rgl.plot3d<-function(z, x, y, cols="red",axes=T,new=T)
##         ##   {xr<-range(x)
##         ##    x01<-(x-xr[1])/(xr[2]-xr[1])
##         ##    yr<-range(y)
##         ##    y01<-(y-yr[1])/(yr[2]-yr[1])
##         ##    zr<-range(z)
##         ##    z01<-(z-zr[1])/(zr[2]-zr[1])
           
##         ##    if(new) rgl.clear()
##         ##    if(axes)
##         ##      {xlab<-pretty(x)
##         ##       ylab<-pretty(y)
##         ##       zlab<-pretty(z)
##         ##       xat<-(xlab-xr[1])/(xr[2]-xr[1])
##         ##       yat<-(ylab-yr[1])/(yr[2]-yr[1])
##         ##       zat<-(zlab-zr[1])/(zr[2]-zr[1])
##         ##       rgl.lines(c(0,1.1),0,0)
##         ##       rgl.lines(0,c(0,1.1),0)
##         ##       rgl.lines(0,0,c(0,1.1))
##         ##       rgl.texts(xat,-.05,-.05,xlab)
##         ##       rgl.texts(-.05,yat,-.05,ylab)
##         ##       rgl.texts(-.05,-.05,zat,zlab)
##         ##       rgl.texts(c(0.5,-.15,-.15),c(-.15,.5,-.15),c(-.15,-.15,.5),
##         ##                 c(deparse(substitute(x)),deparse(substitute(y)),deparse(substitute(z))))
##         ##     }
           
##         ##    rgl.spheres(x01,y01,z01,.01,color=cols)
##         ##  }

##         ## #and here is how you call it
##         ## library(rgl)
##         ## data(iris)

##         ## iris.pc<-prcomp(iris[,1:4],scale=T)
##         ## rgl.plot3d(iris.pc$x[,1],iris.pc$x[,2],iris.pc$x[,3])
##         ## # different colors
##         ## rgl.plot3d(iris.pc$x[,1],iris.pc$x[,2],iris.pc$x[,3],col=unclass(iris[,5])+1)

##       }

        
##     } # close 3d loop


##   } # close function
