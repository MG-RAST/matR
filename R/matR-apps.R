# This contains applications from Kevin's matR-apps repo
# These are close to final form, already functional enough 
# for inclusion in the package, but may need additional
# optimization and documentation

# RESOLVE CREATE_COLORS SUB


######################################################################################################################
################################### FUNCTION FOR PREPROCESSING THE DATA ##############################################
######################################################################################################################
matR_preprocessing <<- function(
                                  data_in,     # name of the input file (tab delimited text with the raw counts) or R matrix
                                  data_type             ="file",  # c(file, r_matrix)
                                  output_object         ="default", # output R object (matrix)
                                  output_file           ="default", # output flat file                       
                                  removeSg              = TRUE, # boolean to remove singleton counts
                                  removeSg_valueMin     = 2, # lowest retained value (lower converted to 0)
                                  removeSg_rowMin       = 4, # lowest retained row sum (lower, row is removed)
                                  log_transform         = FALSE,
                                  norm_method           = "DESeq", #c("standardize", "quantile", "DESeq", none),
                                  DESeq_metadata_in     = NULL, # only used if method is other than "blind"
                                  DESeq_metadata_column = NULL, # only used if method is other than "blind"
                                  DESeq_metadata_type   = "file",           # c( "file", "r_matrix" )
                                  DESeq_method          = "blind",  # c( "pooled", "pooled-CR", "per-condition", "blind" ) # blind, treat everything as one group
                                  DESeq_sharingMode     = "maximum",  # c( "maximum", "fit-only", "gene-est-only" ) # maximum is the most conservative choice
                                  DESeq_fitType         = "local",          # c( "parametric", "local" )
                                  scale_0_to_1          = FALSE,
                                  produce_boxplots      = FALSE,
                                  boxplot_height_in     = "default", # 11,
                                  boxplot_width_in      = "default", #"8.5,
                                  boxplot_res_dpi       = 300,
                                  create_log            = TRUE,
                                  debug                 = FALSE                                  
                                  )

  {     
    # check for necessary packages, install if they are not there
    #require(matR) || install.packages("matR", repo="http://mcs.anl.gov/~braithwaite/R", type="source")
    #chooseCRANmirror()
    setRepositories(ind=1:2)
    require(preprocessCore) || install.packages("preprocessCore")
    #source("http://bioconductor.org/biocLite.R")
    require(DESeq) || biocLite("DESeq")
    # (DESeq): www.ncbi.nlm.nih.gov/pubmed/20979621
    
    #library(preprocessCore)
    #library(DESeq)
    ###### MAIN
 
    # get the name of the data object if an object is used -- use the filename if input is filename string
    if ( identical( data_type, "file") ){
      input_name <- data_in
    }else if( identical( data_type, "r_matrix") ){
      input_name <- deparse(substitute(data_in))
    }else{
      stop( paste( data_type, " is not a valid option for data_type", sep="", collapse=""))
    }

    # Generate names for the output file and object
    if ( identical( output_object, "default") ){
      output_object <- paste( input_name, ".", norm_method, ".PREPROCESSED" , sep="", collapse="")
    }
    if ( identical( output_file, "default") ){
      output_file <- paste( input_name, ".", norm_method, ".PREPROCESSED.txt" , sep="", collapse="")
    }
    
    # Input the data
    if ( identical( data_type, "file") ){
      input_data <- data.matrix(read.table(data_in, row.names=1, header=TRUE, sep="\t", comment.char="", quote=""))
    }else if( identical( data_type, "r_matrix") ){
      input_data <- data.matrix(data_in)
    }else{
      stop( paste( data_type, " is not a valid option for data_type", sep="", collapse=""))
    }
    
    # sort the data (COLUMNWISE) by id
    input_data <- input_data[,order(colnames(input_data))]
    
    # make a copy of the input data that is not processed
    input_data.og <- input_data
 
    # non optional, convert "na's" to 0
    input_data[is.na(input_data)] <- 0
    
    # remove singletons
    if(removeSg==TRUE){
      input_data <- remove.singletons(x=input_data, lim.entry=removeSg_valueMin, lim.row=removeSg_rowMin, debug=debug)
    }
    
    # log transform log(x+1)2
    if ( log_transform==TRUE ){
      input_data <- log_data(input_data)
    }

    regression_message <- "DESeq regression:      NA"
    # Normalize -- stadardize or quantile norm (depends on user selection)
    switch(
           norm_method,
           standardize={
             input_data <- standardize_data(input_data)
           },
           quantile={
             input_data <- quantile_norm_data(input_data)
           },
           DESeq={
             regression_filename = paste(  input_name, ".DESeq_regression.png", sep="", collapse="" )
             regression_message <- paste("DESeq regression:      ", regression_filename, sep="", collapse="" )
             input_data <- DESeq_norm_data(input_data, regression_filename,
                                           DESeq_metadata_in, DESeq_metadata_column, DESeq_metadata_type,
                                           DESeq_method, DESeq_sharingMode, DESeq_fitType, debug)
             
           },
           none={
             input_data <- input_data
           },
           {
             stop( paste( norm_method, " is not a valid option for method", sep="", collapse=""))
           }
           )
    
    # scale normalized data [max..min] to [0..1] over the entire dataset 
    if ( scale_0_to_1==TRUE ){
      input_data <- scale_data(input_data)
    }
    
    # create object, with specified name, that contains the preprocessed data
    do.call("<<-",list(output_object, input_data))
 
    # write flat file, with specified name, that contains the preprocessed data
    write.table(input_data, file=output_file, sep="\t", col.names = NA, row.names = TRUE, quote = FALSE, eol="\n")
    
    # produce boxplots
    boxplot_message <- "output boxplot:        NA"
    if ( produce_boxplots==TRUE ) {
      boxplots_file <- paste(input_name, ".boxplots.png", "\n", sep="", collapse="")
      
      if( identical(boxplot_height_in, "default") ){ boxplot_height_in <- 11 }
      if( identical(boxplot_width_in, "default") ){ boxplot_width_in <- round(ncol(input_data)/14) }

      png(
          filename = boxplots_file,
          height = boxplot_height_in,
          width = boxplot_width_in,
          res = boxplot_res_dpi,
          units = 'in'
          )
      plot.new()
      split.screen(c(2,1))
      screen(1)
      graphics::boxplot(input_data.og, main=(paste(input_name," RAW", sep="", collapse="")), las=2, cex.axis=0.5)
      screen(2)
      graphics::boxplot(input_data, main=(paste(input_name," PREPROCESSED (", norm_method, " norm)", sep="", collapse="")),las=2, cex.axis=0.5)
      dev.off()
      boxplot_message <- paste("output boxplot:       ", boxplots_file, "\n", sep="", collapse="")
    }

    # message to send to the user after completion, given names for object and flat file outputs
    #writeLines( paste("Data have been preprocessed. Proprocessed, see ", log_file, " for details", sep="", collapse=""))
    
    if ( create_log==TRUE ){
      # name log file
      log_file <- paste( output_file, ".log", sep="", collapse="")
      # write log
      writeLines(
                 paste(
                       "##############################################################\n",
                       "###################### INPUT PARAMETERS ######################\n",
                       "data_in:               ", data_in, "\n",
                       "data_type:             ", data_type, "\n",
                       "output_object:         ", output_object, "\n",
                       "output_file:           ", output_file, "\n",
                       "removeSg:              ", as.character(removeSg),
                       "removeSg_valueMin:     ", removeSg_valueMin, "\n",
                       "removeSg_rowMin:       ", removeSg_rowMin, "\n",
                       "log_transform          ", as.character(log_transform), "\n",
                       "norm_method:           ", norm_method, "\n",
                       "DESeq_metadata_in:     ", as.character(DESeq_metadata_in), "\n",
                       "DESeq_metadata_column: ", DESeq_metadata_column, "\n",
                       "DESeq_metadata_type:   ", DESeq_metadata_type, "\n",
                       "DESeq_method:          ", DESeq_method, "\n",
                       "DESeq_sharingMode:     ", DESeq_sharingMode, "\n",
                       "DESeq_fitType:         ", DESeq_fitType, "\n",
                       "scale_0_to_1:          ", as.character(scale_0_to_1), "\n",
                       "produce_boxplots:      ", as.character(produce_boxplots), "\n",
                       "boxplot_height_in:     ", boxplot_height_in, "\n",
                       "boxplot_width_in:      ", boxplot_width_in, "\n",
                       "debug as.character:    ", as.character(debug), "\n",
                       "####################### OUTPUT SUMMARY #######################\n",
                       "output object:         ", output_object, "\n",
                       "otuput file:           ", output_file, "\n",
                       boxplot_message, "\n",
                       regression_message, "\n",
                       "##############################################################",
                       sep="", collapse=""
                       ),
                 con=log_file
                 )
    }    
  }

######################################################################################################################
######################################################################################################################
######################################################################################################################



######################################################################################################################
###################################           CREATE PCO FLAT FILE      ##############################################
######################################################################################################################
plot_pco <- function(
                     file_in,
                     input_dir = "./",
                     output_PCoA_dir = "./",
                     print_dist = 1,
                     output_DIST_dir = "./",
                     dist_method = "euclidean",
                     headers = 1
                     ) 
{
  # load packages
  suppressPackageStartupMessages(library(matlab))      
  suppressPackageStartupMessages(library(ecodist))
  #suppressPackageStartupMessages(library(Cairo))
  #suppressPackageStartupMessages(library(gplots))

  # define sub functions
  func_usage <- function() {
    writeLines("
     You supplied no arguments

     DESCRIPTION: (MGRAST_plot_pco.r):
     This script will perform a PCoA analysis on the inputdata
     using the selected distance metric.  Output always produces a
     *.PCoA file that has the normalized eigenvalues (top n lines)
     and eigenvectors (bottom n x m matris, n lines) where n is the
     number of variables (e.g.subsystems), and m the number of
     samples. You can also choose to produce *.DIST files that contain
     the distance matrix used to generate the PCoA.

     USAGE: MGRAST_plot_pca(
                            file_in = no default arg                               # (string)  input data file            
                            input_dir = \"./\"                                       # (string)  directory(path) of input
                            output_PCoA_dir = \"./\"                                 # (string)  directory(path) for output PCoA file
                            print_dist = 0                                         # (boolean) print the DIST file (distance matrix)
                            output_DIST_dir = \"./\"                                 # (string)  directory(path) for output DIST file 
                            dist_method = \"bray-curtis\"                            # (string)  distance/dissimilarity metric,
                                          (choose from one of the following options)
                                          \"euclidean\" | \"maximum\"     | \"canberra\"    |
                                          \"binary\"    | \"minkowski\"   | \"bray-curtis\" |
                                          \"jacccard\"  | \"mahalanobis\" | \"sorensen\"    |
                                          \"difference\"| \"manhattan\"
                            headers = 0                                            # (booealan) print headers in output PCoA file 
                            )\n"
               )
    stop("MGRAST_plot_pco stopped\n\n")
  }
  
  find_dist <- function(my_data, dist_method)
    {
      switch(dist_method,
             "euclidean" = dist(my_data, method = "euclidean"), 
             "maximum" = dist(my_data, method = "maximum"),
             "manhattan" = dist(my_data, method = "manhattan"),
             "canberra" = dist(my_data, method = "canberra"),
             "binary" = dist(my_data, method = "binary"),
             "minkowski" = dist(my_data, method = "minkowski"),
             
             #"bray-curtis" = distance(my_data, method = "bray-curtis"), # could not handle large data 1-12-12
             
             "bray-curtis" = bcdist(my_data), # 1-12-12
             #"bray-curtis" = vegdist(my_data, method="bray"), # 1-12-12
             #"bray-curtis" = designdist(my_data, method = "(A+B-2*J)/(A+B)") # 1-12-12
             
             "jaccard" = distance(my_data, method = "jaccard"),
             "mahalanobis" = distance(my_data, method = "mahalanobis"),
             "sorensen" = distance(my_data, method = "sorensen"),
             "difference" = distance(my_data, method = "difference")
             # unifrac
             # weighted_unifrac

             # distance methods with {stats}dist: dist(x, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
             #      euclidean maximum manhattan canberra binary minkowski

             # distance methods with {ecodist}distance: distance(x, method = "euclidean")
             #      euclidean bray-curtis manhattan mahalanobis jaccard "simple difference" sorensen

             )
    }


  # stop and give the usage if the proper number of arguments is not given
  if ( nargs() == 0 ){
    func_usage()
  }

  # load data

  #writeLines("FILE-IN")
  #writeLines(file_in)
  input_data_path = gsub(" ", "", paste(input_dir, file_in))
  #writeLines("INPUT-DATA-PATH")
  #writeLines(input_data_path)
  #my_data <<- flipud(rot90(data.matrix(read.table(input_data_path, row.names=1, header=TRUE, sep="\t", comment.char="", quote="")))) # edited on 12-14-12, stop character conversions in column names
  my_data <<- flipud(rot90(data.matrix(read.table(input_data_path, row.names=1, check.names=FALSE, header=TRUE, sep="\t", comment.char="", quote=""))))

  
  num_data_rows = dim(my_data)[1] # substitute 0 for NA's if they exist in the data
  num_data_cols = dim(my_data)[2]
  for (row_num in (1:num_data_rows)){
    for (col_num in (1:num_data_cols)){
      #my_data[row_num, col_num] = as.integer(my_data[row_num, col_num]) # added 1-12-12 to fix "Error in vector("double", length) : vector size cannot be NA ...
      if (is.na(my_data[row_num, col_num])){
        my_data[row_num, col_num] <<- 0
      }
    }
  }


   
  # calculate distance matrix
  dist_matrix <<- find_dist(my_data, dist_method)
  DIST_file_out <- gsub(" ", "", paste(output_DIST_dir, file_in, ".", dist_method, ".DIST"))
  if (print_dist > 0) { write_file(file_name = DIST_file_out, data = data.matrix(dist_matrix)) }

  # perform the pco
  my_pco <<- pco(dist_matrix)

  # scale eigen values from 0 to 1, and label them
  eigen_values <<- my_pco$values
  scaled_eigen_values <<- (eigen_values/sum(eigen_values))
  for (i in (1:dim(as.matrix(scaled_eigen_values))[1])) {names(scaled_eigen_values)[i]<<-gsub(" ", "", paste("PCO", i))}
  scaled_eigen_values <<- data.matrix(scaled_eigen_values)
  #for (i in (1:dim(as.matrix(scaled_ev))[1])) dimnames(scaled_ev)[i]<<-gsub(" ", "", paste("PCO", i))

  # label the eigen vectors
  eigen_vectors <<- data.matrix(my_pco$vectors) 
  dimnames(eigen_vectors)[[1]] <<- dimnames(my_data)[[1]]

  # write eigen values and then eigen vectors to file_out
  PCoA_file_out = gsub(" ", "", paste(output_PCoA_dir, file_in, ".", dist_method, ".PCoA"))

  if ( headers == 1 ){
    write(file = PCoA_file_out, paste("# file_in    :", file_in,
            "\n# dist_method:", dist_method,
            "\n#________________________________",
            "\n# EIGEN VALUES (scaled 0 to 1) >",
            "\n#________________________________"),
          append=FALSE)
    write.table(scaled_eigen_values, file=PCoA_file_out, col.names=FALSE, row.names=TRUE, append = TRUE, sep="\t", eol="\n")
  }else{
    write.table(scaled_eigen_values, file=PCoA_file_out, col.names=FALSE, row.names=TRUE, append = FALSE, sep="\t", eol="\n")
  }
  
  if ( headers == 1 ){
    write(file = PCoA_file_out, paste("#________________________________",
            "\n# EIGEN VECTORS >",
            "\n#________________________________"),
          append=TRUE)
  }

  #write.table(eigen_vectors, file=PCoA_file_out, col.names=FALSE, row.names=TRUE, append = TRUE, sep="\t")
  write.table(eigen_vectors, file=PCoA_file_out, col.names=FALSE, row.names=TRUE, append = TRUE, sep="\t", eol="\n")
  
}
######################################################################################################################
######################################################################################################################
######################################################################################################################



######################################################################################################################
###############################     RENDER PCO FROM PRECALCULATED PCO AND METADATA    ################################
######################################################################################################################
matR_render_pcoa <- function(
                            PCoA_in="", # annotation abundance table (raw or normalized values)
                            image_out="default",
                            figure_main ="principal coordinates",
                            components=c(1,2,3), # R formated string telling which coordinates to plot, and how many (2 or 3 coordinates)
                            label_points=FALSE, # default is off
                            metadata_table=NA, # matrix that contains colors or metadata that can be used to generate colors
                            metadata_column_index=1, # column of the color matrix to color the pcoa (colors for the points in the matrix) -- rows = samples, columns = colorings
                            amethst_groups=NA,        
                            color_list=NA, # use explicit list of colors - trumps table if both are supplied
                            pch_table=NA, # additional matrix that allows users to specify the shape of the data points
                            pch_column=1,
                            image_width_in=22,
                            image_height_in=17,
                            image_res_dpi=300,
                            width_legend = 0.2, # fraction of width used by legend
                            width_figure = 0.8, # fraction of width used by figure
                            title_cex = 2, # cex for the title of title of the figure
                            legend_cex = 2, # cex for the legend
                            figure_cex = 2, # cex for the figure
                            bar_cex = 2, 
                            use_all_metadata_columns=FALSE, # option to overide color_column -- if true, plots are generate for all of the metadata columns
                            debug=TRUE
                            )
  
{
  
  require(matR)
  require(scatterplot3d)
  
  argument_test <- is.na(c(metadata_table,amethst_groups,color_list)) # check that incompatible options were not selected
  if ( 3 - length(subset(argument_test, argument_test==TRUE) ) > 1){
    stop(
         paste(
               "\n\nOnly on of these can have a non NA value:\n",
               "     metadata_table: ", metadata_table,"\n",
               "     amethst_groups: ", amethst_groups, "\n",
               "     color_list    : ", color_list, "\n\n",
               sep="", collapse=""
               )
         )
  }
  
  ######################
  ######## MAIN ########
  ######################
  
  my_data <- load_pcoa_data(PCoA_in) # import PCoA data from *.PCoA file --- this is always done
  eigen_values <- my_data$eigen_values
  eigen_vectors <- my_data$eigen_vectors
  
  plot_pch <- load_pch(pch_table) # THIS PORTION WILL HANDLE PCH TABLE - NOT REALLY FUNCTIONAL AT PRESENT
  
  #####################################################################################
  ########## PLOT WITH NO METADATA OR COLORS SPECIFIED (all point same color) #########
  #####################################################################################
  if ( length(argument_test==TRUE)==0 ){ # create names for the output files
    if ( identical(image_out, "default") ){
      image_out = paste( PCoA_in,".NO_COLOR.PCoA.png", sep="", collapse="" )
      figure_main = paste( PCoA_in, ".NO_COLOR.PCoA", sep="", collapse="" )
    }else{
      image_out = paste(image_out, ".png", sep="", collapse="")
      figure_main = paste( image_out,".PCoA", sep="", collapse="")
    }
    
    column_levels <- "data" # assign necessary defaults for plotting
    num_levels <- 1
    color_levels <- 1
    ncol.color_matrix <- 1
    pcoa_colors <- "black"   

    create_plot( # generate the plot
                PCoA_in,
                ncol.color_matrix,
                eigen_values, eigen_vectors, components,
                column_levels, num_levels, color_levels, pcoa_colors, plot_pch,
                image_out,figure_main,
                image_width_in, image_height_in, image_res_dpi,
                width_legend, width_figure,
                title_cex, legend_cex, figure_cex, bar_cex, label_points
                )
  }
  #####################################################################################
    
  #####################################################################################
  ########### PLOT WITH AMETHST GROUPS (colors generated by load_metadata) ############
  #####################################################################################
  if ( identical( is.na(amethst_groups), FALSE ) ){ # create names for the output files
    if ( identical(image_out, "default") ){
      image_out = paste( PCoA_in,".AMETHST_GROUPS.PCoA.png", sep="", collapse="" )
      figure_main = paste( PCoA_in, ".AMETHST_GROUPS.PCoA", sep="", collapse="" )
    }else{
      image_out = paste(image_out, ".png", sep="", collapse="")
      figure_main = paste( image_out,".PCoA", sep="", collapse="")
    }

    con_grp <- file(amethst_groups) # get metadata and generate colors from amethst groups file
    open(con_grp)
    line_count <- 1
    groups.list <- vector(mode="character")
    while ( length(my_line <- readLines(con_grp,n = 1, warn = FALSE)) > 0) {
      new_line <- my_line
      split_line <- unlist(strsplit(my_line, split=","))
      split_line.list <- rep(line_count, length(split_line))
      names(split_line.list) <- split_line
      groups.list <- c(groups.list, split_line.list)
      line_count <- line_count + 1
    }
    close(con_grp)
    if ( length(groups.list) != length(unique(names(groups.list))) ){
      stop("One or more groups have redundant entries - this is not allowed for coloring the PCoA")
    }
    metadata_column <- matrix(groups.list, ncol=1)

    suppressWarnings( numericCheck <- as.numeric(metadata_column) ) # check to see if metadata are numeric, and sort accordingly
    if( is.na(numericCheck[1])==FALSE ){
      column_name = colnames(metadata_column)[1]
      row_names = rownames(metadata_column)
      metadata_column <- matrix(numericCheck, ncol=1)
      colnames(metadata_column) <- column_name
      rownames(metadata_column) <- row_names
    }
    metadata_column <- metadata_column[ order(metadata_column),,drop=FALSE ] # order the metadata by value
    color_column <- create_colors(metadata_column, color_mode = "auto")
    
    column_levels <- levels(as.factor(as.matrix(metadata_column))) 
    num_levels <- length(column_levels)
    color_levels <- col.wheel(num_levels)
    ncol.color_matrix <- 1
    
    colnames(metadata_column) <- "amethst_metadata"
    column_levels <- column_levels[ order(column_levels) ] # NEW (order by levels values)
    color_levels <- color_levels[ order(column_levels) ] # NEW (order by levels values)

    pcoa_colors <- as.character(color_column[,1]) # convert colors to a list after they've been used to sort the eigen vectors
    
    create_plot( # generate the plot
                PCoA_in,
                ncol.color_matrix,
                eigen_values, eigen_vectors, components,
                column_levels, num_levels, color_levels, pcoa_colors, plot_pch,
                image_out,figure_main,
                image_width_in, image_height_in, image_res_dpi,
                width_legend, width_figure,
                title_cex, legend_cex, figure_cex, bar_cex, label_points
                )
    
  }
  #####################################################################################
  
  #####################################################################################
  ############ PLOT WITH LIST OF COLORS (colors generated by load_metadata) ###########
  #####################################################################################
  if ( identical( is.na(color_list), FALSE ) ){ # create names for the output files
    if ( identical(image_out, "default") ){
      image_out = paste( PCoA_in,".color_List.PCoA.png", sep="", collapse="" )
      figure_main = paste( PCoA_in, ".color_list.PCoA", sep="", collapse="" )
    }else{
      image_out = paste(image_out, ".png", sep="", collapse="")
      figure_main = paste( image_out,".PCoA", sep="", collapse="")
    }

    column_levels <- levels(as.factor(as.matrix(color_list))) # get colors directly from list of colors
    num_levels <- length(column_levels)
    color_levels <- col.wheel(num_levels)
    ncol.color_matrix <- 1
    pcoa_colors <- color_list
    
    create_plot( # generate the plot
                PCoA_in,
                ncol.color_matrix,
                eigen_values, eigen_vectors, components,
                column_levels, num_levels, color_levels, pcoa_colors, plot_pch,
                image_out,figure_main,
                image_width_in, image_height_in, image_res_dpi,
                width_legend, width_figure,
                title_cex, legend_cex, figure_cex, bar_cex, label_points
                )
  }
  #####################################################################################

  #####################################################################################
  ########### PLOT WITH METADATA_TABLE (colors produced from color_matrix) ############
  ######## CAN HANDLE PLOTTING ALL OR A SINGLE SELECTED METADATA TABLE COLUMN #########
  #####################################################################################
  if ( identical( is.na(metadata_table), FALSE ) ){

    
    metadata_matrix <- as.matrix( # Load the metadata table (same if you use one or all columns)
                              read.table(
                                         file=metadata_table,row.names=1,header=TRUE,sep="\t",
                                         colClasses = "character", check.names=FALSE,
                                         comment.char = "",quote="",fill=TRUE,blank.lines.skip=FALSE
                                         )
                              )   
    metadata_matrix <- metadata_matrix[ order(rownames(metadata_matrix)),,drop=FALSE ]  # make sure that the metadata matrix is sorted (ROWWISE) by id
        
    if ( use_all_metadata_columns==TRUE ){ # AUTOGENERATE PLOTS FOR ALL COLUMNS IN THE METADATA FILE - ONE PLOT PER METADATA COLUMN

      ncol.color_matrix <- ncol( metadata_matrix) # get then number of columns in the metadata data file = number of plots
      for (i in 1:ncol.color_matrix){ # loop to process through all columns
        
        metadata_column <- metadata_matrix[ ,i,drop=FALSE ] # get column i from the metadata matrix
        if(debug==TRUE){ test1<<-metadata_column }
        
        image_out = paste(PCoA_in,".", colnames(metadata_column), ".pcoa.png", sep="", collapse="") # generate name for plot file
        figure_main = paste( PCoA_in,".", colnames(metadata_column),".PCoA", sep="", collapse="") # generate title for the plot
        
        suppressWarnings( numericCheck <- as.numeric(metadata_column) ) # check to see if metadata are numeric, and sort accordingly
        if( is.na(numericCheck[1])==FALSE ){
          column_name = colnames(metadata_column)[1]
          row_names = rownames(metadata_column)
          metadata_column <- matrix(numericCheck, ncol=1)
          colnames(metadata_column) <- column_name
          rownames(metadata_column) <- row_names
        }
        
        if(debug==TRUE){ test2<<-metadata_column }
        
        metadata_column <- metadata_column[ order(metadata_column),,drop=FALSE ] # order the metadata by value
        if(debug==TRUE){ test3<<-metadata_column }
        
        color_column <- create_colors(metadata_column, color_mode = "auto") # set parameters for plotting
        ncol.color_matrix <- 1 
        column_factors <- as.factor(metadata_column) 
        column_levels <- levels(as.factor(metadata_column))
        num_levels <- length(column_levels)
        color_levels <- col.wheel(num_levels)
        rownames(eigen_vectors) <- gsub("\"", "", rownames(eigen_vectors)) # make sure that vectors are sorted identically to the colors
        eigen_vectors <- eigen_vectors[ rownames(color_column), ]        
        pcoa_colors <- as.character(color_column[,1]) # convert colors to a list after they've been used to sort the eigen vectors
        create_plot( # generate the plot
                    PCoA_in,
                    ncol.color_matrix,
                    eigen_values, eigen_vectors, components,
                    column_levels, num_levels, color_levels, pcoa_colors, plot_pch,
                    image_out,figure_main,
                    image_width_in, image_height_in, image_res_dpi,
                    width_legend, width_figure,
                  title_cex, legend_cex, figure_cex, bar_cex, label_points
                    )        
      }
      
      
    }else if ( use_all_metadata_columns==FALSE ){ # ONLY CREATE A PLOT FOR THE SELECTED COLUMN IN THE METADATA FILE
      

      metadata_column <- metadata_matrix[ ,metadata_column_index,drop=FALSE ] # get column i from the metadata matrix
      if(debug==TRUE){ test1<<-metadata_column }
      
      image_out = paste(PCoA_in,".", colnames(metadata_column), ".pcoa.png", sep="", collapse="") # generate name for plot file
      figure_main = paste( PCoA_in,".", colnames(metadata_column),".PCoA", sep="", collapse="") # generate title for the plot
      
      suppressWarnings( numericCheck <- as.numeric(metadata_column) ) # check to see if metadata are numeric, and sort accordingly
      if( is.na(numericCheck[1])==FALSE ){
        column_name = colnames(metadata_column)[1]
        row_names = rownames(metadata_column)
        metadata_column <- matrix(numericCheck, ncol=1)
        colnames(metadata_column) <- column_name
        rownames(metadata_column) <- row_names
      }

      if(debug==TRUE){ test2<<-metadata_column }
      
      metadata_column <- metadata_column[ order(metadata_column),,drop=FALSE ] # order the metadata by value
      if(debug==TRUE){ test3<<-metadata_column }
      
      color_column <- create_colors(metadata_column, color_mode = "auto") # set parameters for plotting
      ncol.color_matrix <- 1 
      column_factors <- as.factor(metadata_column) 
      column_levels <- levels(as.factor(metadata_column))
      num_levels <- length(column_levels)
      color_levels <- col.wheel(num_levels)
      rownames(eigen_vectors) <- gsub("\"", "", rownames(eigen_vectors)) # make sure that vectors are sorted identically to the colors
      eigen_vectors <- eigen_vectors[ rownames(color_column), ]        
      pcoa_colors <- as.character(color_column[,1]) # convert colors to a list after they've been used to sort the eigen vectors
      create_plot( # generate the plot
                  PCoA_in,
                  ncol.color_matrix,
                  eigen_values, eigen_vectors, components,
                  column_levels, num_levels, color_levels, pcoa_colors, plot_pch,
                  image_out,figure_main,
                  image_width_in, image_height_in, image_res_dpi,
                  width_legend, width_figure,
                  title_cex, legend_cex, figure_cex, bar_cex, label_points
                  )
      
    }else{
      stop(paste("invalid value for use_all_metadata_columns(", use_all_metadata_columns,") was specified, please try again", sep="", collapse=""))
    }
  }
  
}
######################################################################################################################
######################################################################################################################
######################################################################################################################



######################################################################################################################
#####################     HEATMAP DENDROGRAM _ WITH BELLS AND WHISTLES    ############################################
######################################################################################################################
# Majority of this code is adapted from heatmap.2{gplots}
heatmap_dendrogram.from_file <- function (

                                          file_in,

                                          metadata_table=NA,
                                          metadata_column=1,
                                          metadata_colors=NA,
                                          legend_cex=1,

                                          produce_flat_output = TRUE,
                                          file_out = if (produce_flat_output) paste(file_in, ".HD_sorted.txt", sep="", collapse="") else NA, # produce HD sorted flat output
     
                                          return_heatmap_object = TRUE,
                                          heatmap_objectname = if (return_heatmap_object) paste(file_in, ".heatmap", sep="", collapse=""),

                                          scale_0_to_1 = TRUE,                  # scale all values in matrix between 0 and 1
                                          figure_type   = "png",                # c("jpg" "pdf" "ps" or "png") # added this as an input argument 8-10-10
                                          image_out = gsub(" ", "", paste(file_in, ".HD.", figure_type)),
                                          image_title = image_out, # image_out
                                                           
                                          # colors
                                          #heat_color1="red",                   # two colors for the the gradient that will be created for the heatmap
                                          #heat_color2="green",
                                          col = c("red","black","green"),      #"heat.colors", # <------ Kevin 1-27-10 - MADE VARIABLE in loop below
                                          palette_n=150,                        # 255 is the max value
       
                                          labRow = NA,                          # Kevin 1-27-10 - Dendrogram row labels (NULL for default; NA to remove)
                                          labCol = NA,                          # Kevin 1-27-10 - Dendrogram column labels (NULL for default; NA to remove)
                                          # par (las=2 (labels perp to axis)
                                          hclustfun_method = "ward",            # clustering method: from hclust{stats}, default is "complete"
                                                                                # hclustfun_method = c("ward", "single", "complete", "average", "mcquitty", "median" or "centroid")
                                 
                                       # figure output parameters (units vary depending on selected figure_type (bleow)
                                          figure_width  = 22,                   # usually pixels, inches if eps is selected; png is default
                                          figure_height = 10,                   # usually pixels, inches if eps is selected; png is default
                                          my_units = "in",
                                          figure_res = 200,                     # usually pixels, inches if eps is selected; png is default      
                                       #figure_type   = "png",                  # c("jpg" "pdf" "ps" or "png") # added this as an input argument 8-10-10
                                       
                                       # dendrogram control
                                          dendrogram = "both",                  # dendrogram = c("both","row", "column", "none")
                                                                                # character string indicating whether to draw 'none', 'row', 'column' or 'both'
                                                                                # dendrograms. Defaults to 'both'. However, if Rowv (or Colv) is FALSE or NULL
                                                                                # and dendrogram is 'both', then a warning is issued and Rowv (or Colv)
                                                                                # arguments are honoured.
                                          
                                          symm = FALSE,                         # logical indicating if x should be treated symmetrically;
                                                                                # can only be true when x is a square matrix.
                                          
                                          Rowv = TRUE,                          # determines if and how the row dendrogram should be reordered.
                                                                                # By default, it is TRUE, which implies dendrogram is computed
                                                                                # and reordered based on row means. If NULL or FALSE, then no dendrogram
                                                                                # is computed and no reordering is done. If a dendrogram, then it is
                                                                                # used "as-is", ie without any reordering. If a vector of integers,
                                                                                # then dendrogram is computed and reordered based on the order of the vector.
                                          
                                          Colv = if (symm) "Rowv" else TRUE,    # determines if and how the column dendrogram should be reordered.
                                                                                # Has the options as the Rowv argument above and additionally when x
                                                                                # is a square matrix, Colv = "Rowv" means that columns should be treated
                                                                                # identically to the rows.
                                          
                                          distfun = dist,                       # dist method: from dist{stats}, default is euclidean
                                                                                # "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"
                                                                                # try ecodist distance ?
                                          
                                          hclustfun = hclust,                   # <------ Kevin 2-8-10 - forces "complete" method # made variable directly below 2-24-10
                                                                                # other options = c("ward", "single", "complete", "average", "mcquitty", "median" or "centroid") 
                                       
                                        # data scaling
                                          scale = "none",                       # scale = c("none", "row", "column")
                                          na.rm = TRUE,                         # logical indicating whether NA's should be removed.
                                 
                                        # image plot
                                          revC = identical(Colv, "Rowv"),
                                          add.expr,
                                 
                                        # mapping data to colors
                                          breaks,
                                          symbreaks = min(x < 0, na.rm = TRUE) || scale != "none",
                                 
                                        

                                        # block sepration
                                          colsep,
                                          rowsep, 
                                          sepcolor = "white",
                                          sepwidth = c(0.05, 0.05),

                                        # cell labeling
                                          cellnote,
                                          notecex = 1, 
                                          notecol = "cyan",
                                          na.color = par("bg"),
                                       
                                        # level trace
                                          trace = "none",                                     # trace = c("column", "row", "both", "none")
                                          tracecol = "cyan",
                                          hline = median(breaks), 
                                          vline = median(breaks),
                                          linecol = tracecol,

                                        # Row/Column Labeling
                                          margins = c(5, 1),                                  ##### <------ Kevin 1-27-10 - specifcy the size of the margins

                                          ColSideColors, # (optional) character vector of length ncol(x) containing the color names for a
                                                         # horizontal side bar that may be used to annotate the columns of x.
                                          RowSideColors, # (optional) character vector of length nrow(x) containing the color names for a
                                                         # vertical side bar that may be used to annotate the rows of x.

                                          row_lab_mult = 2, # <-----                          # used below to adjust font size of row labels - Kevin 3-9-10
                                          col_lab_mult = 3, # <-----                          # used below to adjust font size of column labels - Kevin 3-9-10
                                          cexRow = row_lab_mult*(1/log10(nr)),                # 0.1 + 1/log10(nr),  ##### <------ Kevin 1-27-10 (Dendogram row font size)  
                                          cexCol = col_lab_mult*(1/log10(nc)),                # 0.1 + 1/log10(nc),   ##### <------ Kevin 1-27-10 (Dendogram column font size)
                                        #labRow = NULL,                                      # Kevin 1-27-10 - Dendrogram row labels (NA to remove)
                                        #labCol = NULL,                                      # Kevin 1-27-10 - Dendrogram column labels (NA to remove)
                                 
                                        # color key + density info
                                          key = TRUE,                                         # <------ Kevin 9-28-10 key scaling needs work
                                          keysize = .9,                                       ##### <------ Kevin 1-27-10 size of the key
                                          key_lines = 1,                                      # Kevin ADDED 1-27-10 0=no 1=yes for trace lines in the key (edited in loop below)
                                          key_text = "Heat Color Key (min to max)",                      #\nand Histogram", # Kevin  1-27-10 - ADDED - MADE VARIABLE
                                          key_text_cex = 0.5,                                 # Kevin made this variable 4-7-10
                                          key_xlabel = NULL,                                  #"Value", # Kevin  1-27-10 - ADDED - MADE VARIABLE 
                                          key_ylabel = NULL,                                  #"Count", # Kevin  1-27-10 - ADDED - MADE VARIABLE
                                          density.info = c("histogram", "density", "none"),
                                          denscol = tracecol,                                 # <------ Kevin 1-27-10 - spcify color for key traceline
                                          symkey = min(x < 0, na.rm = TRUE) || symbreaks,
                                          densadj = 0.25,
                                 
                                        # plot labels 
                                          xlab = NULL,
                                          ylab = NULL,
                                 
                                        # plot layout
                                          lmat = NULL,
                                          # lmat = rbind(4:3, 2:1) # is the default
                                          # lhei = NULL,                                        # <--- line height multiplier
                                          lhei=c(0.2,0.8),
                                          # lwid = NULL,
                                          lwid = c(0.05, 0.95),
                                 
                                        # extras ...
                                 
                                 ...) # close input arguments
  
{
  
  
###### load the neccessary packages
  #library(Cairo)
  library(gplots)
  library(matlab)
  library(gtools) # for the invalid function

###### sub to import the input_file
#import_data <- function(file_name)
  {
    x = data.matrix(read.table(file_in, row.names=1, check.names=FALSE, header=TRUE, sep="\t", comment.char="", quote=""))
  }


# Import metadata, and use to generate color bar for the columns
  if ( identical( is.na(metadata_table), FALSE )==TRUE ) {

    my_colors <- load_metadata(metadata_table, metadata_column)

    metadata_levels <- my_colors$metadata_levels
    color_levels <- my_colors$color_levels
    ColSideColors <- my_colors$all_colors

    png(filename="heatmap_legend.png", width = 4, height = 10, pointsize = 12, res = 150 , units = "in")
    plot.new()
    legend( x="center", legend=metadata_levels, pch=15, col=color_levels, cex=2)
    dev.off()
    # return( list(metadata_levels=metadata_levels, color_levels=color_levels, all_colors=all_colors) )
  }

  
###### get the diensions of the input object  
  number_entries = (dim(x)[1]) # number rows
  number_samples = (dim(x)[2]) # number columns

###### Scale all values in matrix from 0 to 1 if that default (scale_0_to_1) is TRUE

  if( scale_0_to_1 == TRUE ){

    min_value <- min(x)
    max_value <- max(x)
    
    for ( entry_row in 1:number_entries ){
    for ( sample_col in 1:number_samples ){
      x[entry_row,sample_col] <- (( x[entry_row,sample_col] - min_value)/( max_value - min_value)) 
    }
  }
  }

  
###### create the "main" or title for the figure - also used as the name of the output file
  main = gsub(" ", "", paste(image_title, "::", hclustfun_method, "_clustering"))
  main = gsub(" ", "", main)

###### Sub function that creates the color palette for the heatmap from selected colors (red to gren is default) 
  #heat_palette<-function (heat_color1, heat_color2, n=palette_n)  
    { 
      #ramp <- colorRamp(c(heat_color1, heat_color2))
      ramp <- colorRamp(col)
      custom_palette<<- rgb(ramp(seq(0, 1, length = palette_n)), max = 255)
    }
  #heat_palette()                   # 
  col = custom_palette             # - custom_palette is the output from heat_palette
 
###### Produce output as png (Default)
  if(identical(figure_type, "png")){
    #png_filename = paste(main, "_heatmap_dendrogram.png") # added 5-12-10
    #png_filename = gsub(" ", "", png_filename) # added 6-15-10
    #CairoPNG(png_filename, width = figure_width, height = figure_height, pointsize = 12, res = fiure_res , units = "px")
    #CairoPNG(image_out, width = figure_width, height = figure_height, pointsize = 12, res = figure_res , units = "px")
    png(filename=image_out, width = figure_width, height = figure_height, pointsize = 12, res = figure_res , units = my_units)
  }
  
###### Produce output as jpeg
  if(identical(figure_type, "jpg")){
    #jpeg_filename = paste(main, "_heatmap_dendrogram.jpg") # added 2-24-10
    #jpeg_filename = gsub(" ", "", jpeg_filename) # added 6-15-10
    #CairoJPEG(jpeg_filename, quality=100, width = figure_width, height = figure_height, res = figure_res, units = "px")  # moved here 6-14-10
    CairoJPEG(image_out, quality=100, width = figure_width, height = figure_height, res = figure_res, units = "px")
  }
    
###### Produce output as pdf
  if(identical(figure_type, "pdf")){
    #pdf_filename = paste(main, "_heatmap_dendrogram.pdf")
    #pdf_filename = gsub(" ", "", pdf_filename)
    #CairoPDF(file = pdf_filename, width = figure_width, height = figure_height, res = fiure_res, units = "px")
    CairoPDF(file = image_out, width = figure_width, height = figure_height, res = figure_res, units = "px")
  }

###### Produce output as eps *** eps figures have their dimensions in inches, not pixels
  if (identical(figure_type, "ps")){
    #ps_filename = paste(main, "_heatmap_dendrogram.ps")
    #ps_filename = gsub(" ", "", ps_filename)
    #CairoPS(file = ps_filename, width = figure_width, height = figure_height, res = fiure_res, units = "px")
    #CairoPS(file = image_out, width = figure_width, height = figure_height, res = figure_res, units = "px")
    #CairoPS(file = image_out, width = figure_width, height = figure_height, print.it = TRUE)
    #CairoPS(file = image_out)
    #postscript(file = image_out, width = figure_width, height = figure_height)
    postscript(file = image_out)
  }



###### Kevin's heavily edited version of gplots heatmap.2
  if (trace=="none"){                                                           # Kevin ADDED 1-27-10 trace line is no also removed from the key
    key_lines = 0  
  }                  
  
  scale01 <- function(x, low = min(x), high = max(x)) {
    x <- (x - low)/(high - low)
    x
  }
  retval <- list()
  scale <- if (symm && missing(scale)) 
    "none"
  else match.arg(scale)
  dendrogram <- match.arg(dendrogram)
  trace <- match.arg(trace)
  density.info <- match.arg(density.info)
  if (length(col) == 1 && is.character(col)) 
    col <- get(col, mode = "function")
  if (!missing(breaks) && (scale != "none")) 
    warning("Using scale=\"row\" or scale=\"column\" when breaks are", 
            "specified can produce unpredictable results.", "Please consider using only one or the other.")
  if (is.null(Rowv) || is.na(Rowv)) 
    Rowv <- FALSE
  if (is.null(Colv) || is.na(Colv)) 
    Colv <- FALSE
  else if (Colv == "Rowv" && !isTRUE(Rowv)) 
    Colv <- FALSE
  if (length(di <- dim(x)) != 2 || !is.numeric(x)) 
    stop("`x' must be a numeric matrix")
  nr <- di[1]
  nc <- di[2]
  if (nr <= 1 || nc <= 1) 
    stop("`x' must have at least 2 rows and 2 columns")
  if (!is.numeric(margins) || length(margins) != 2) 
    stop("`margins' must be a numeric vector of length 2")
  if (missing(cellnote)) 
    cellnote <- matrix("", ncol = ncol(x), nrow = nrow(x))
  if (!inherits(Rowv, "dendrogram")) {
    if (((!isTRUE(Rowv)) || (is.null(Rowv))) && (dendrogram %in% 
                                                 c("both", "row"))) {
      if (is.logical(Colv) && (Colv)) 
        dendrogram <- "column"
      else dedrogram <- "none"
      warning("Discrepancy: Rowv is FALSE, while dendrogram is `", 
              dendrogram, "'. Omitting row dendogram.")
    }
  }
  if (!inherits(Colv, "dendrogram")) {
    if (((!isTRUE(Colv)) || (is.null(Colv))) && (dendrogram %in% 
                                                 c("both", "column"))) {
      if (is.logical(Rowv) && (Rowv)) 
        dendrogram <- "row"
      else dendrogram <- "none"
      warning("Discrepancy: Colv is FALSE, while dendrogram is `", 
              dendrogram, "'. Omitting column dendogram.")
    }
  }
  if (inherits(Rowv, "dendrogram")) {
    ddr <- Rowv
    rowInd <- order.dendrogram(ddr)
  }
  else if (is.integer(Rowv)) {
    hcr <- hclustfun(distfun(x), method = hclustfun_method) # <--- Does the row dendrogram - Kevin 1-27-10 # added the hclustfun_method argument on 2-24-10
    ddr <- as.dendrogram(hcr)
    ddr <- reorder(ddr, Rowv)
    rowInd <- order.dendrogram(ddr)
    if (nr != length(rowInd)) 
      stop("row dendrogram ordering gave index of wrong length")
  }
  else if (isTRUE(Rowv)) {
    Rowv <- rowMeans(x, na.rm = na.rm) # <--- Does the row dendrogram - Kevin 1-27-10
    hcr <- hclustfun(distfun(x),  method = hclustfun_method) # added the hclustfun_method argument on 2-24-10
    ddr <- as.dendrogram(hcr)
    ddr <- reorder(ddr, Rowv)
    rowInd <- order.dendrogram(ddr)
    if (nr != length(rowInd)) 
      stop("row dendrogram ordering gave index of wrong length")
  }
  else {
    rowInd <- nr:1
  }
  if (inherits(Colv, "dendrogram")) {
    ddc <- Colv
    colInd <- order.dendrogram(ddc)
  }
  else if (identical(Colv, "Rowv")) {
    if (nr != nc) 
      stop("Colv = \"Rowv\" but nrow(x) != ncol(x)")
    if (exists("ddr")) {
      ddc <- ddr
      colInd <- order.dendrogram(ddc)
    }
    else colInd <- rowInd
  }
  else if (is.integer(Colv)) {
    hcc <- hclustfun(distfun(if (symm) # <--- Does the column dendrogram - Kevin 1-27-10
                             x
    else t(x)), method = hclustfun_method) # added the hclustfun_method argument on 2-24-10
    ddc <- as.dendrogram(hcc)
    ddc <- reorder(ddc, Colv)
    colInd <- order.dendrogram(ddc)
    if (nc != length(colInd)) 
      stop("column dendrogram ordering gave index of wrong length")
  }
  else if (isTRUE(Colv)) {
    Colv <- colMeans(x, na.rm = na.rm)  # <--- Does the column dendrogram - Kevin 1-27-10
    hcc <- hclustfun(distfun(if (symm) 
                             x
    else t(x)), method = hclustfun_method) # added the hclustfun_method argument on 2-24-10
    ddc <- as.dendrogram(hcc)
    ddc <- reorder(ddc, Colv)
    colInd <- order.dendrogram(ddc)
    if (nc != length(colInd)) 
      stop("column dendrogram ordering gave index of wrong length")
  }
  else {
    colInd <- 1:nc
  }
  retval$rowInd <- rowInd
  retval$colInd <- colInd
  retval$call <- match.call()
  x <- x[rowInd, colInd]
  x.unscaled <- x
  cellnote <- cellnote[rowInd, colInd]
  if (is.null(labRow)) 
    labRow <- if (is.null(rownames(x))) 
      (1:nr)[rowInd]
    else rownames(x)
  else labRow <- labRow[rowInd]
  if (is.null(labCol)) 
    labCol <- if (is.null(colnames(x))) 
      (1:nc)[colInd]
    else colnames(x)
  else labCol <- labCol[colInd]
  if (scale == "row") {
    retval$rowMeans <- rm <- rowMeans(x, na.rm = na.rm)
    x <- sweep(x, 1, rm)
    retval$rowSDs <- sx <- apply(x, 1, sd, na.rm = na.rm)
    x <- sweep(x, 1, sx, "/")
  }
  else if (scale == "column") {
    retval$colMeans <- rm <- colMeans(x, na.rm = na.rm)
    x <- sweep(x, 2, rm)
    retval$colSDs <- sx <- apply(x, 2, sd, na.rm = na.rm)
    x <- sweep(x, 2, sx, "/")
  }
  if (missing(breaks) || is.null(breaks) || length(breaks) < 
      1) {
    if (missing(col) || is.function(col)) 
      breaks <- 16
    else breaks <- length(col) + 1
  }
  if (length(breaks) == 1) {
    if (!symbreaks) 
      breaks <- seq(min(x, na.rm = na.rm), max(x, na.rm = na.rm), 
                    length = breaks)
    else {
      extreme <- max(abs(x), na.rm = TRUE)
      breaks <- seq(-extreme, extreme, length = breaks)
    }
  }
  nbr <- length(breaks)
  ncol <- length(breaks) - 1
  if (class(col) == "function") 
    col <- col(ncol)
  min.breaks <- min(breaks)
  max.breaks <- max(breaks)
  x[x < min.breaks] <- min.breaks
  x[x > max.breaks] <- max.breaks
  if (missing(lhei) || is.null(lhei)) 
    lhei <- c(keysize, 4)
  if (missing(lwid) || is.null(lwid)) 
    lwid <- c(keysize, 4)
  if (missing(lmat) || is.null(lmat)) {
    lmat <- rbind(4:3, 2:1)
    if (!missing(ColSideColors)) {
      if (!is.character(ColSideColors) || length(ColSideColors) != 
          nc) 
        stop("'ColSideColors' must be a character vector of length ncol(x)")
      lmat <- rbind(lmat[1, ] + 1, c(NA, 1), lmat[2, ] + 
                    1)
      lhei <- c(lhei[1], 0.2, lhei[2])
    }
    if (!missing(RowSideColors)) {
      if (!is.character(RowSideColors) || length(RowSideColors) != 
          nr) 
        stop("'RowSideColors' must be a character vector of length nrow(x)")
      lmat <- cbind(lmat[, 1] + 1, c(rep(NA, nrow(lmat) - 
                                         1), 1), lmat[, 2] + 1)
      lwid <- c(lwid[1], 0.2, lwid[2])
    }
    lmat[is.na(lmat)] <- 0
  }
  if (length(lhei) != nrow(lmat)) 
    stop("lhei must have length = nrow(lmat) = ", nrow(lmat))
  if (length(lwid) != ncol(lmat)) 
    stop("lwid must have length = ncol(lmat) =", ncol(lmat))
  op <- par(no.readonly = TRUE)
  on.exit(par(op))

  # create the layout: default is lmat <- rbind(4:3, 2:1)
  layout(lmat, widths = lwid, heights = lhei, respect = FALSE)
  if (!missing(RowSideColors)) {
    par(mar = c(margins[1], 0, 0, 0.5))
    image(rbind(1:nr), col = RowSideColors[rowInd], axes = FALSE)
  }
  if (!missing(ColSideColors)) {
    par(mar = c(0.5, 0, 0, margins[2]))
    image(cbind(1:nc), col = ColSideColors[colInd], axes = FALSE)
  }
  par(mar = c(margins[1], 0, 0, margins[2]))
  x <- t(x)
  cellnote <- t(cellnote)
  if (revC) {
    iy <- nr:1
    if (exists("ddr")) 
      ddr <- rev(ddr)
    x <- x[, iy]
    cellnote <- cellnote[, iy]
  }
  else iy <- 1:nr

  # plot 1 (recall default layout ( lmat <- rbind(4:3, 2:1) ))
  image(1:nc, 1:nr, x, xlim = 0.5 + c(0, nc), ylim = 0.5 + 
        c(0, nr), axes = FALSE, xlab = "", ylab = "", col = col, 
        breaks = breaks, ...)
  retval$carpet <- x
  if (exists("ddr")) 
    retval$rowDendrogram <- ddr
  if (exists("ddc")) 
    retval$colDendrogram <- ddc
  retval$breaks <- breaks
  retval$col <- col
  if (!invalid(na.color) & any(is.na(x))) {
    mmat <- ifelse(is.na(x), 1, NA)
    image(1:nc, 1:nr, mmat, axes = FALSE, xlab = "", ylab = "", 
          col = na.color, add = TRUE)
  }
  # axis below
  axis(1, 1:nc, labels = labCol, las = 2, line = -0.5, tick = 0, # las = 2 is perp to axis 8-16-10 see ?par
       cex.axis = cexCol)
  if (!is.null(xlab)) 
    mtext(xlab, side = 1, line = margins[1] - 1.25)
  # axis to right
  axis(4, iy, labels = labRow, las = 2, line = -0.5, tick = 0,   # las = 2 is perp to axis 8-16-10 see ?par
       cex.axis = cexRow)
  if (!is.null(ylab)) 
    mtext(ylab, side = 4, line = margins[2] - 1.25)
  if (!missing(add.expr)) 
    eval(substitute(add.expr))
  if (!missing(colsep)) 
    for (csep in colsep) rect(xleft = csep + 0.5, ybottom = rep(0, 
                                                    length(csep)), xright = csep + 0.5 + sepwidth[1], 
                              ytop = rep(ncol(x) + 1, csep), lty = 1, lwd = 1, 
                              col = sepcolor, border = sepcolor)
  if (!missing(rowsep)) 
    for (rsep in rowsep) rect(xleft = 0, ybottom = (ncol(x) + 
                                1 - rsep) - 0.5, xright = nrow(x) + 1, ytop = (ncol(x) + 
                                                   1 - rsep) - 0.5 - sepwidth[2], lty = 1, lwd = 1, 
                              col = sepcolor, border = sepcolor)
  min.scale <- min(breaks)
  max.scale <- max(breaks)
  x.scaled <- scale01(t(x), min.scale, max.scale)
  if (trace %in% c("both", "column")) {
    retval$vline <- vline
    vline.vals <- scale01(vline, min.scale, max.scale)
    for (i in colInd) {
      if (!is.null(vline)) {
        abline(v = i - 0.5 + vline.vals, col = linecol, 
               lty = 2)
      }
      xv <- rep(i, nrow(x.scaled)) + x.scaled[, i] - 0.5
      xv <- c(xv[1], xv)
      yv <- 1:length(xv) - 0.5
      lines(x = xv, y = yv, lwd = 1, col = tracecol, type = "s")
    }
  }
  if (trace %in% c("both", "row")) {
    retval$hline <- hline
    hline.vals <- scale01(hline, min.scale, max.scale)
    for (i in rowInd) {
      if (!is.null(hline)) {
        abline(h = i + hline, col = linecol, lty = 2)
      }
      yv <- rep(i, ncol(x.scaled)) + x.scaled[i, ] - 0.5
      yv <- rev(c(yv[1], yv))
      xv <- length(yv):1 - 0.5
      lines(x = xv, y = yv, lwd = 1, col = tracecol, type = "s")
    }
  }
  if (!missing(cellnote)) 
    text(x = c(row(cellnote)), y = c(col(cellnote)), labels = c(cellnote), 
         col = notecol, cex = notecex)
  par(mar = c(margins[1], 0, 0, 0))


  # plot the horizontal dendrogram ?
  if (dendrogram %in% c("both", "row")) {

    if( identical( is.na(metadata_table), FALSE )==TRUE ){
      plot.new()
      legend( x="center", legend=metadata_levels, pch=15, col=color_levels, cex=legend_cex)
    }else{
      plot(ddr, horiz = TRUE, axes = FALSE, yaxs = "i", leaflab = "none")
    }

  }
  else

    if( identical( is.na(metadata_table), FALSE )==TRUE ){
      plot.new()
      legend( x="center", legend=metadata_levels, pch=15, col=color_levels, cex=legend_cex)
    }else{
      plot.new() # empty plot if "both or "row" are not chosen
    }

  par(mar = c(0, 0, if (!is.null(main)) 5 else 0, margins[2]))
  ##############################################################
  
  # plot the vertical dendrogram?
  if (dendrogram %in% c("both", "column")) {
    plot(ddc, axes = FALSE, xaxs = "i", leaflab = "none")
  }
  else plot.new() # empty plot if "both or "row" are not chosen
  ##############################################################

  if (!is.null(main)) 
    title(main, cex.main = .9 * op[["cex.main"]])
  if (key) {
    par(mar = c(5, 4, 2, 1), cex = 0.75)
    tmpbreaks <- breaks
    if (symkey) {
      max.raw <- max(abs(c(x, breaks)), na.rm = TRUE)
      min.raw <- -max.raw
      tmpbreaks[1] <- -max(abs(x))
      tmpbreaks[length(tmpbreaks)] <- max(abs(x))
    }
    else {
      min.raw <- min(x, na.rm = TRUE)
      max.raw <- max(x, na.rm = TRUE)
    }
    z <- seq(min.raw, max.raw, length = length(col))
    # looks like the color bar in the key
    image(z = matrix(z, ncol = 1), col = col, breaks = tmpbreaks, 
          xaxt = "n", yaxt = "n")
    par(usr = c(0, 1, 0, 1))
    lv <- pretty(breaks)
    xv <- scale01(as.numeric(lv), min.raw, max.raw)
    axis(1, at = xv, labels = lv)
    if (scale == "row") 
      mtext(side = 1, "Row Z-Score", line = 2)
    else if (scale == "column") 
      mtext(side = 1, "Column Z-Score", line = 2)
    else mtext(side = 1, key_xlabel, line = 2) # <---- Kevin 1-27-10 (make option) - the x axis label for key - MADE VARIABLE

    if (density.info == "density") { # This is for the "color key + density info" - Kevin 1-27-10
      dens <- density(x, adjust = densadj, na.rm = TRUE)
      omit <- dens$x < min(breaks) | dens$x > max(breaks)
      dens$x <- dens$x[-omit]
      dens$y <- dens$y[-omit]
      dens$x <- scale01(dens$x, min.raw, max.raw)
      if (key_lines > 0){ # Kevin 1-27-10 added loop to make this optional
        lines(dens$x, dens$y/max(dens$y) * 0.95, col = denscol, # <-- This is the part that adds the line trace to the key - Kevin 1-27-10 (make option)
              lwd = 1)
        axis(2, at = pretty(dens$y)/max(dens$y) * 0.95, pretty(dens$y))  # axis 2 is for the "color key + density info" - Kevin 1-27-10
      }
      title(key_text) # Kevin 1-27-10 (changed to a variable argument)
      par(cex = key_text_cex)
      mtext(side = 2, "Density", line = 2)
    }

    else if (density.info == "histogram") { # axis 2 is for the "color key + density info" - Kevin 1-27-10
      h <- hist(x, plot = FALSE, breaks = breaks)
      hx <- scale01(breaks, min.raw, max.raw)
      hy <- c(h$counts, h$counts[length(h$counts)])
      if (key_lines > 0){ # Kevin 1-27-10 added loop to make this optional
        lines(hx, hy/max(hy) * 0.95, lwd = 1, type = "s", # <-- This is the part that adds the line trace to the key - Kevin 1-27-10 (make option)
              col = denscol)
        axis(2, at = pretty(hy)/max(hy) * 0.95, pretty(hy))
      }
      title(key_text) # Kevin 1-27-10 (changed to a variable argument)
      par(cex = key_text_cex) # Kevin 4-7-10 made variable from = cex = 0.5
      mtext(side = 2, key_ylabel, line = 2) # <---- Kevin 1-27-10 (make option) - the y axis label for key - MADE VARIABLE
    }
    
    else title(key_text) # Kevin 1-27-10 (changed to a variable argument)
  }

  # This does the main table ?
  else plot.new()
  retval$colorTable <- data.frame(low = retval$breaks[-length(retval$breaks)], 
                                  high = retval$breaks[-1], color = retval$col)
  # Produce heatmap sorted output if specified
  if ( produce_flat_output==TRUE){
    ## heatmap_to_file(my_heatmap=retval, file_out=file_out)
    rot_x <- t(x[1:nrow(x),])
    rowsort_rot_x <- rot_x[nrow(rot_x):1,]
    #output_filename <- gsub(" ", "", paste(file_in, ".HD_sorted_table.txt"))
    write.table(rot_x, file = file_out, col.names=NA, row.names = TRUE, sep="\t", quote=FALSE)
    print(paste("Wrote heatmap dendrogram sorted data as file: ", file_out, sep="", collapse=""))
  }

  # Return heatmap object if sepcified
  if ( return_heatmap_object==TRUE){
    do.call("<<-",list(heatmap_objectname, retval))
    print(paste("Wrote heatmap dendrogram to object: ", heatmap_objectname, sep="", collapse=""))
  }

  invisible(retval)
  
  dev.off()
    
}
######################################################################################################################
######################################################################################################################
######################################################################################################################



######################################################################################################################
##########################                  STATS FUNCTION                ############################################
######################################################################################################################
stats_from_files <<- function(
                               data_table="HMP_WGS.data.quantile_norm.txt",
                               metadata_table="HMP_WGS.meta_data.tab_delim.txt.enviroment_package.groups.txt",
                               metadata_column=8,
                               stat_test="Kruskal-Wallis", # c("Kruskal-Wallis", "t-test-paired", "Wilcoxon-paired", "t-test-unpaired", "Mann-Whitney-unpaired-Wilcoxon", "ANOVA-one-way")
                               file_out="default",
                               append_group_headers=TRUE,
                               order_by=NULL,
                               order_decreasing=FALSE,
                               ...)
{
  
  # create name for the output file
  if ( identical(file_out, "default") ){
    file_out = paste(data_table, ".STATS_RESULTS.txt", sep="", collapse="")
  }
  
  # read in the abundance data
  data_matrix <- data.matrix(read.table(
                                        data_table,
                                        row.names=1,
                                        header=TRUE,
                                        sep="\t",
                                        comment.char="",
                                        quote="",
                                        check.names=FALSE
                                        )
                             )
  # Here, make sure that the data are sorted COLUMNWISE by mgid
  data_matrix <-  data_matrix[,order(colnames(data_matrix))]
  
  # read in the metadata
  metadata_matrix <- as.matrix(
                               read.table(
                                          metadata_table,
                                          row.names=1,
                                          header=TRUE,
                                          sep="\t",
                                          colClasses = "character",
                                          check.names=FALSE,
                                          comment.char = "",
                                          quote="",
                                          fill=TRUE,
                                          blank.lines.skip=FALSE
                                          )
                               )
  # make sure that the color matrix is sorted (ROWWISE) by mgid
  metadata_matrix <-  metadata_matrix[order(rownames(metadata_matrix)),]

  # retrieve the selected grouping
  #groups.list <- as.list(metadata_matrix[,metadata_column])
  groups.list <- (metadata_matrix[,metadata_column])
  names(groups.list) <- rownames(metadata_matrix) 
 
# sigtest to perform stats
# place selected stat in variable for use below
# my_stat = "Kruskal-Wallis"
# perform stat tests (uses matR)
  my_stats <- sigtest(data_matrix, groups.list, stat_test)

# Create headers for the data columns
  for (i in 1:dim(data_matrix)[2]){
    if ( append_group_headers==TRUE ){ # append group to data column header if selected
       colnames(data_matrix)[i] <- paste( colnames(data_matrix)[i], "::", (groups.list)[i], sep="" )
    }else{
       colnames(data_matrix)[i] <- colnames(data_matrix)[i]
    }
  }
  for (i in 1:dim(my_stats$mean)[2]){
    colnames(my_stats$mean)[i] <- paste( colnames(my_stats$mean)[i], "::group_mean", sep="" )
  }
  for (i in 1:dim(my_stats$sd)[2]){
    colnames(my_stats$sd)[i] <- paste( colnames(my_stats$sd)[i], "::group_sd", sep="" )
  }
  my_stats.statistic <- as.matrix(my_stats$statistic)
  colnames(my_stats.statistic) <- paste(stat_test, "::stat", sep="")
  my_stats.p <- as.matrix(my_stats$p.value)
  colnames(my_stats.p) <- paste(stat_test, "::p", sep="")
  my_stats.fdr <- as.matrix(p.adjust(my_stats$p.value))
  colnames(my_stats.fdr) <- paste(stat_test, "::fdr", sep="")

# generate a summary object - used to generate the plots, and can be used to create a flat file output
  my_stats.summary <- cbind(data_matrix, my_stats$mean, my_stats$sd, my_stats.statistic, my_stats.p, my_stats.fdr)

# make sure that order_by value, if other than NULL is supplied, is valid
  if (is.null(order_by)){ # use last column by default, or specified column otherwise
    order_by <- ( ncol(my_stats.summary) )
  } else {
    if (is.integer(order_by)){
      if ( order_by > ncol(my_stats.summary) ){
        stop( paste(
                    "\n\norder_by (", order_by,") must be an integer between 1 and ",
                    ncol(my_stats.summary),
                    " (max number of columns in the output)\n\n",
                    sep="",
                    collaps=""
                    ) )
      }
    }
  }

# order the data by the selected column - placing ordered data in a new object
  my_stats.summary.ordered <- my_stats.summary[ order(my_stats.summary[,order_by], decreasing=order_decreasing), ]

# flat file output of the summary file
  write.table(my_stats.summary.ordered, file = file_out, col.names=NA, sep="\t", quote=FALSE)
  
}
######################################################################################################################
######################################################################################################################
######################################################################################################################



######################################################################################################################
#################################       BATCH DOWNLOAD      ##########################################################
######################################################################################################################
matR_batch_dl <- function(
                          mgid_list,    # file with list of IDs - no header
                          list_is_file=TRUE,
                          print_list=FALSE, # print copy of list of ids to variable "my_list"
                          #start_sample=1, # list entry to start with; NOTE start sample overides start batch
                          start_batch=1, # batch to start with: NOTE start_batch is overridden by start_sample
                          use_auth=FALSE,
                          auth="~/my_auth", # file with auth key
                          sleep_int = 10, # initial sleep time (in seconds) -- incremented by 10 with each sleep
                          my_log = "default", # name for the log file
                          batch_size = 50, # number of IDs to process at one time (100 is the hard coded limit for the API - would suggest much smaller)
                          my_entry="counts", 
                          my_annot="function",
                          my_source="Subsystems",
                          my_level="level3",
                          #my_data_name = "default", # name for the data object
                          output_prefix="my_data_matrix",
                          debug=FALSE,
                          verbose=TRUE
                          ){

  # NOTE: -- If this fails -- make sure that you are using the most up to date matR
  # get the zip from here https://github.com/MG-RAST/matR
  # install it like this install.packages("~/some_path/matR-master", repos=NULL, type="source")
  # also make sure that RCurl and RJSONIO are installed
  
  # check for necessary arguments - show usage if they are not supplied
  if ( nargs() == 0){print_dl_usage()} 
  if (identical(mgid_list, "") ){print_dl_usage()}

  # create name for "default" log
  if ( identical(my_log, "default")==TRUE ){ my_log=paste(mgid_list,".download_log",sep="", collapse="") }
  
  # load required pacakges
  require(matR) 
  require(RJSONIO)
  require(RCurl)

  if( debug==TRUE ){ print("made it here 1") }
  # source Dan's matR object merging script
  ##source_https("https://raw.github.com/braithwaite/matR-apps/master/collection-merge.R") # get the merge function
  
  # Set authentication (key is in file)
  if (use_auth==TRUE){
    msession$setAuth(file=auth)
  }
  
  # delete old log file if it exists
  if ( file.exists(my_log)==TRUE ){ # delete old log if it exist 
    unlink(my_log)
     print( paste("deleted old log:", my_log) )
   }

  # delete my_data object if it exists
  if ( exists("my_data")==TRUE ){
    suppressWarnings(rm(my_data))
    print("deleted previous object named my_data")
  } 

  ## check to see if mgid_list is a character vector or file
  ## If it's a file check for columns - one column, assume it's the ids
  ## If it's two columns, first is ids, second is name
  #if ( length(mgid_list) > 1 ){

  if (list_is_file==TRUE){
    temp_list <- read.table(mgid_list)
    num_samples <- dim(temp_list)[1]
    new_list <- vector(mode="character", length=num_samples)
    for( i in 1:num_samples ){ # add ids to list
      new_list[i] <- as.character(temp_list[i,1])
    }
    if( dim(temp_list)[2] == 2 ){ # name the ids in the list -- if names were supplied
      for( j in 1:num_samples ){
        names(new_list)[j] <- as.character(temp_list[j,2])
      }
    }
    #mgid_list <- new_list[start_sample:num_samples]
    mgid_list <- new_list[1:num_samples]
  }

  # make sure the id list has only unique ids
  mgid_list <- levels(as.factor(mgid_list))

  if (print_list==TRUE){ my_list <<- mgid_list }
  
  write( date(), file = my_log, append = TRUE)
  # calculate and print some information to the log
  num_batch <- as.integer( length(mgid_list)%/%batch_size )
  batch_remainder <- length(mgid_list)%%batch_size

  ## if( start_sample > 1 ){
  ##   num_batch.original <- num_batch
  ##   num_batch <- as.integer( (length(mgid_list)-start_sample)%/%batch_size )
  ##   batch_remainder <- (length(mgid_list)-start_sample+1)%%batch_size
  ##   start_batch <- 1
  ## }

  write(
        paste(
              "# Num unique samples:   ", length(mgid_list), "\n",
              #"# Start sample:         ", start_sample, "\n",
              "# Batch size:           ", batch_size, "\n",
              "# Start batch:          ", start_batch, "\n",
              "# Num complete batches: ", num_batch, "\n",
              "# Remainder batch size: ", batch_remainder, "\n",
              sep="",
              collapse=""
              ),
        file = my_log,
        append = TRUE
        )

  ############################################################################
  # MAIN LOOP - PROCESSES ALL BATCHES EXCEPT (IF THERE IS ONE) THE REMAINDER #
  ############################################################################
  this_is_first_batch = TRUE
  for (batch_count in start_batch:(num_batch)){

    # Process the first batch
    if( this_is_first_batch==TRUE ){
      this_is_first_batch=FALSE
      #if (batch_count == 1){ 

      # Get the first batch of data and use to initialize my_data object  (will be 1 and batch_size unless first_batch is > 1)
      # calculate start and stop 
      batch_start <- ((batch_count-1)*batch_size)+1
      # batch_start <- 1
      batch_end <- (batch_count*batch_size)
      # batch_end <- batch_size
      
      first_batch <- process_batch(batch_count, batch_start, batch_end, mgid_list, my_log, my_entry, my_annot, my_source, my_level, sleep_int, debug)
      my_data <- data.matrix(first_batch$count)

      # write information to the log
      write(
            paste(
                  "# finished with batch (", batch_count, ") :: with (", (batch_end - batch_start + 1), ") metagenomes",
                  sep="",
                  collapse=""
                  ),
            file = my_log,
            append = TRUE
            )
      write( date(), file = my_log, append = TRUE)
      write("\n\n", file = my_log, append = TRUE)

      # replace NA's with 0
      my_data[ is.na(my_data) ]<-0

      # write current data to file
      my_output = gsub(" ", "", paste(output_prefix,".BATCH_", batch_count,".", my_entry, ".txt"))
      write.table(my_data, file = my_output, col.names=NA, row.names = TRUE, sep="\t", quote=FALSE)
      
    }else{ # process all batches except first and remainder

      # Process the continuing (next) batch
      batch_start <- ((batch_count-1)*batch_size)+1
      batch_end <- (batch_count*batch_size)
      next_batch <- process_batch(batch_count, batch_start, batch_end, mgid_list, my_log, my_entry, my_annot, my_source, my_level, sleep_int, debug)
      
      # Add the next batch to my_data
      my_data <- merge(my_data, data.matrix(next_batch$count), by="row.names", all=TRUE) # This does not handle metadata yet
      rownames(my_data) <- my_data$Row.names
      my_data$Row.names <- NULL

      # write information to the log
      write(
            paste(
                  "# finished with batch (", batch_count, ") :: with (", (batch_end - batch_start + 1), ") metagenomes",
                  sep="",
                  collapse=""
                  ),
            file = my_log,
            append = TRUE
            )
      write( date(), file = my_log, append = TRUE)
      write("\n\n", file = my_log, append = TRUE)
      
      # replace NA's with 0
      my_data[ is.na(my_data) ]<-0

      # write current data to file
      my_output = gsub(" ", "", paste(output_prefix,".BATCH_", start_batch,"_to_", batch_count,".", my_entry, ".txt"))
      write.table(my_data, file = my_output, col.names=NA, row.names = TRUE, sep="\t", quote=FALSE)
      
    }
  }

  # process remainder batch
  if ( batch_remainder > 0 ){ 

    # Process the last batch (if there is a remainder
    batch_start <- (num_batch*batch_size)+1
    batch_end <- length(mgid_list)
    last_batch <- process_batch( (batch_count+1) , batch_start, batch_end, mgid_list, my_log, my_entry, my_annot, my_source, my_level, sleep_int, debug)

    # Add the next batch to my_data
    my_data <- merge(my_data, data.matrix(last_batch$count), by="row.names", all=TRUE) # This does not handle metadata yet
    rownames(my_data) <- my_data$Row.names
    my_data$Row.names <- NULL

    # write information to the log
    write(
          paste(
                "# finished with batch", batch_count, ":: with", (batch_end - batch_start + 1), "metagenomes",
                sep="",
                collapse=""
                ),
            file = my_log,
          append = TRUE
          )
    write( date(), file = my_log, append = TRUE)
    write("\n\n", file = my_log, append = TRUE)
      
    # replace NA's with 0
    my_data[ is.na(my_data) ]<-0

    # write current data to file
    my_output = gsub(" ", "", paste(output_prefix,".BATCH_", start_batch, "_to_", (batch_count+1) ,".", my_entry, ".txt"))
    write.table(my_data, file = my_output, col.names=NA, row.names = TRUE, sep="\t", quote=FALSE)
    #rm(my_data)
    
  }

  # write final outputs
  #my_output = gsub(" ", "", paste(output_prefix,".ALL_BATCHES.", my_entry, ".txt"))
  #write.table(my_data, file = my_output, col.names=NA, row.names = TRUE, sep="\t", quote=FALSE)

  ## # rename the R object in memory if that option was selected - otherwise, named as mgid_list.data
  ## if ( identical(my_data_name, "default")==TRUE ){
  ##   if ( debug==TRUE ){ print("made it into rename loop A") }
  ##   my_data_name <- paste(mgid_list, ".data", sep="", collapse="" )
  ##   data_name <- my_data_name
  ##   assign( my_data_name, my_data )
  ## }else{
  ##   if ( debug==TRUE ){ print("made it into rename loop B") }
  ##   data_name <- my_data_name
  ##   assign( my_data_name, my_data )
  ## }
  
  # create named object with downloaded data avaialable in the workplace
  my_data <<- my_data
    
  #print(paste("data available as data.matrix: my_data and as flat file ", my_output, sep="", collapse=""))
  write( paste("data available as data.matrix: ", "my_data"," and as flat file: ", my_output, sep="", collapse=""), file = my_log, append = TRUE ) 
  print( paste("data available as data.matrix: ", "my_data"," and as flat file: ", my_output, sep="", collapse=""), file = my_log, append = TRUE )
   
}
  
######################################################################################################################
######################################################################################################################
######################################################################################################################







######################################################################################################################
###################################                   SUBS              ##############################################
######################################################################################################################

######################################################################################################################
# func to import the data; columns are samples, rows are categories
import_data <- function(data_in){
  data_matrix <- data.matrix(read.table(data_in, row.names=1, check.names=FALSE, header=TRUE, sep="\t", comment.char="", quote=""))
  return(data_matrix)
}
######################################################################################################################

######################################################################################################################
# func to import metadata; columns are metadata conditions, rows are samples
import_metadata <- function(metadata_in){
  metadata_matrix <- as.matrix( # Load the metadata table (same if you use one or all columns)
                               read.table(
                                          file=metadata_in,row.names=1,header=TRUE,sep="\t",
                                          colClasses = "character", check.names=FALSE,
                                          comment.char = "",quote="",fill=TRUE,blank.lines.skip=FALSE
                                          )
                               )   
  
  return(metadata_matrix)
}
######################################################################################################################

######################################################################################################################
# SUB( ): Function to load the metadata/ generate or import colors for the points
######################################################################################################################
load_metadata <- function(metadata_table, metadata_column){
  metadata_matrix <- as.matrix( # Import metadata table, use it to generate colors
                               read.table(
                                          file=metadata_table,row.names=1,header=TRUE,sep="\t",
                                          colClasses = "character", check.names=FALSE,
                                          comment.char = "",quote="",fill=TRUE,blank.lines.skip=FALSE
                                          )
                               )
  color_matrix <- create_colors(metadata_matrix, color_mode = "auto")
  ncol.color_matrix <- ncol(color_matrix)
  metadata_factors <- as.factor(metadata_matrix[,metadata_column])
  metadata_levels <- levels(as.factor(metadata_matrix[,metadata_column]))
  num_levels <- length(metadata_levels)
  color_levels <- col.wheel(num_levels)
  all_colors <- color_matrix[,metadata_column]
  return( list(metadata_levels=metadata_levels, color_levels=color_levels, all_colors=all_colors) )  
}
######################################################################################################################

######################################################################################################################
write_file <- function(file_name, data) {
  write.table(data, file=file_name, col.names=NA, row.names=TRUE, append = FALSE, sep="\t", quote = FALSE, eol="\n")
}
######################################################################################################################

######################################################################################################################
######## SUB(1): Function to import the data from a pre-calculated PCoA
######################################################################################################################
load_pcoa_data <- function(PCoA_in){
  con_1 <- file(PCoA_in)
  con_2 <- file(PCoA_in)
  # read through the first time to get the number of samples
  open(con_1);
  num_values <- 0
  data_type = "NA"
  while ( length(my_line <- readLines(con_1,n = 1, warn = FALSE)) > 0) {
    if ( length( grep("PCO", my_line) ) == 1  ){
      num_values <- num_values + 1
    }
  }
  close(con_1)
  # create object for values
  eigen_values <- matrix("", num_values, 1)
  dimnames(eigen_values)[[1]] <- 1:num_values
  eigen_vectors <- matrix("", num_values, num_values)
  dimnames(eigen_vectors)[[1]] <- 1:num_values
  # read through a second time to populate the R objects
  value_index <- 1
  vector_index <- 1
  open(con_2)
  current.line <- 1
  data_type = "NA"
  while ( length(my_line <- readLines(con_2,n = 1, warn = FALSE)) > 0) {
    if ( length( grep("#", my_line) ) == 1  ){
      if ( length( grep("EIGEN VALUES", my_line) ) == 1  ){
        data_type="eigen_values"
      } else if ( length( grep("EIGEN VECTORS", my_line) ) == 1 ){
        data_type="eigen_vectors"
      }
    }else{
      split_line <- noquote(strsplit(my_line, split="\t"))
      if ( identical(data_type, "eigen_values")==TRUE ){
        dimnames(eigen_values)[[1]][value_index] <- noquote(split_line[[1]][1])
        eigen_values[value_index,1] <- noquote(split_line[[1]][2])       
        value_index <- value_index + 1
      }
      if ( identical(data_type, "eigen_vectors")==TRUE ){
        dimnames(eigen_vectors)[[1]][vector_index] <- noquote(split_line[[1]][1])
        for (i in 2:(num_values+1)){
          eigen_vectors[vector_index, (i-1)] <- as.numeric(noquote(split_line[[1]][i]))
        }
        vector_index <- vector_index + 1
      }
    }
  }
  close(con_2)
  # finish labeling of data objects
  dimnames(eigen_values)[[2]] <- "EigenValues"
  dimnames(eigen_vectors)[[2]] <- dimnames(eigen_values)[[1]]
  class(eigen_values) <- "numeric"
  class(eigen_vectors) <- "numeric"
  return(list(eigen_values=eigen_values, eigen_vectors=eigen_vectors))
  
}
######################################################################################################################
  
######################################################################################################################
# SUB(3): Function to import the pch information for the points # load pch matrix if one is specified
######################################################################################################################
load_pch <- function(pch_table){
  if ( identical( is.na(pch_table), FALSE ) ){
    pch_matrix <- data.matrix(read.table(file=pch_table, row.names=1, header=TRUE, sep="\t", comment.char="", quote="", check.names=FALSE))
    pch_matrix <- pch_matrix[order(rownames(pch_matrix)),]
    plot_pch <- pch_matrix[,pch_column]
  }else{
    plot_pch <- 19
  }
  return(plot_pch)
}
######################
  
######################################################################################################################
# SUB(4): Workhorse function that creates the plot
######################################################################################################################
create_plot <- function(
                        PCoA_in,
                        ncol.color_matrix,
                        eigen_values, eigen_vectors, components,
                        column_levels, num_levels, color_levels, pcoa_colors, plot_pch,
                        image_out,figure_main,
                        image_width_in, image_height_in, image_res_dpi,
                        width_legend, width_figure,
                        title_cex, legend_cex, figure_cex, bar_cex, label_points 
                        ){                      
  png( # initialize the png 
      filename = image_out,
      width = image_width_in,
      height = image_height_in,
      res = image_res_dpi,
      units = 'in'
      )
  # CREATE THE LAYOUT
  my_layout <- layout(  matrix(c(1,1,2,3,4,4), 3, 2, byrow=TRUE ), widths=c(width_legend,width_figure), heights=c(0.1,0.8,0.1) )
  layout.show(my_layout)
  # PLOT THE TITLE
  plot.new()
  text(x=0.5, y=0.5, figure_main, cex=title_cex)
  # PLOT THE LEGEND
  plot.new()
  legend( x="center", legend=column_levels, pch=15, col=color_levels, cex=legend_cex)
  # PLOT THE FIGURE
  # set par options (Most of the code in this section is copied/adapted from Dan Braithwaite's pco plotting in matR)
  par <- list ()
  par$main <- ""#figure_main
  #par$labels <- if (length (names (x)) != 0) names (x) else samples (x)
  if ( label_points==TRUE ){
    par$labels <-  rownames(eigen_vectors)
  } else {
    par$labels <- NA
  }
  #if (length (groups (x)) != 0) par$labels <- paste (par$labels, " (", groups (x), ")", sep = "")
  par [c ("xlab", "ylab", if (length (components) == 3) "zlab" else NULL)] <- paste ("PC", components, ", R^2 = ", format (eigen_values [components], dig = 3), sep = "")
  par$cex <- figure_cex
  # main plot paramters - create the 2d or 3d plot
  i <- eigen_vectors [ ,components [1]]
  j <- eigen_vectors [ ,components [2]]
  k <- if (length (components) == 3) eigen_vectors [ ,components [3]] else NULL
  if (is.null (k)) {
  #par$col <- col
    par$col <- pcoa_colors ####<--------------
    par$pch <- plot_pch
    par <- resolveMerge (list (...), par)
    xcall (plot, x = i, y = j, with = par, without = "labels")
    xcall (points, x = i, y = j, with = par, without = "labels")
    grid ()
  } else {
    # parameter "color" has to be specially handled.
    # "points" above wants "col", scatterplot3d wants "color", and we
    # want the user not to worry about it...
    # par$color <- col
    par$color <- pcoa_colors
    par$pch <- plot_pch
    par$type <- "h"
    par$lty.hplot <- "dotted"
    par$axis <- TRUE
    par$box <- FALSE
    #par <- resolveMerge (list (...), par)
    reqPack ("scatterplot3d")
    xys <- xcall (scatterplot3d, x = i, y = j, z = k, with = par,
                  without = c ("cex", "labels")) $ xyz.convert (i, j, k)
    i <- xys$x ; j <- xys$y
  }
  text (x = i, y = j, labels = par$labels, pos = 4, cex = par$cex)
  # PLOT THE COLOR BAR
  bar_x <- 1:num_levels
  bar_y <- 1
  bar_z <- matrix(1:num_levels, ncol=1)
  image(x=bar_x,y=bar_y,z=bar_z,col=color_levels,axes=FALSE,xlab="",ylab="")
  loc <- par("usr")
  text(loc[1], loc[1], column_levels[1], pos = 4, xpd = T, cex=bar_cex, adj=c(0,0))
  text(loc[2], loc[3], column_levels[num_levels], pos = 2, xpd = T, cex=bar_cex, adj=c(0,1))
  graphics.off()
}
######################################################################################################################
  
######################################################################################################################
# SUB(6): Create optimal contrast color selection using a color wheel
# adapted from https://stat.ethz.ch/pipermail/r-help/2002-May/022037.html 
######################################################################################################################
col.wheel <- function(num_col, my_cex=0.75) {
  cols <- rainbow(num_col)
  col_names <- vector(mode="list", length=num_col)
  for (i in 1:num_col){
    col_names[i] <- getColorTable(cols[i])
  }
  cols
}
######################################################################################################################

######################################################################################################################
# SUB(7): The inverse function to col2rgb()
# adapted from https://stat.ethz.ch/pipermail/r-help/2002-May/022037.html
######################################################################################################################
rgb2col <- function(rgb) {
  rgb <- as.integer(rgb)
  class(rgb) <- "hexmode"
  rgb <- as.character(rgb)
  rgb <- matrix(rgb, nrow=3)
  paste("#", apply(rgb, MARGIN=2, FUN=paste, collapse=""), sep="")
}
######################################################################################################################
  
######################################################################################################################
# SUB(8): Convert all colors into format "#rrggbb"
# adapted from https://stat.ethz.ch/pipermail/r-help/2002-May/022037.html
######################################################################################################################
getColorTable <- function(col) {
  rgb <- col2rgb(col);
  col <- rgb2col(rgb);
  sort(unique(col))
}
######################################################################################################################

######################################################################################################################
# SUB(9): Automtically generate colors from metadata with identical text or values
######################################################################################################################
create_colors <- function(metadata_column, color_mode = "auto"){ # function to     
  my_data.color <- data.frame(metadata_column)
  column_factors <- as.factor(metadata_column[,1])
  column_levels <- levels(as.factor(metadata_column[,1]))
  num_levels <- length(column_levels)
  color_levels <- col.wheel(num_levels)
  levels(column_factors) <- color_levels
  my_data.color[,1]<-as.character(column_factors)
  return(my_data.color)
}
######################################################################################################################





## ######################
## # SUB(9): Automtically generate colors from metadata with identical text or values
## ######################
## create_colors <- function(color_matrix, color_mode = "auto"){ # function to     
##   my_data.color <- data.frame(color_matrix)
##   ids <- rownames(color_matrix)
##   color_categories <- colnames(color_matrix)
##   for ( i in 1:dim(color_matrix)[2] ){
##     column_factors <- as.factor(color_matrix[,i])
##     column_levels <- levels(as.factor(color_matrix[,i]))
##     num_levels <- length(column_levels)
##     color_levels <- col.wheel(num_levels)
##     levels(column_factors) <- color_levels
##     my_data.color[,i]<-as.character(column_factors)
##   }
##   return(my_data.color)
## }
## ######################
## ######################






######################################################################################################################
# functions that will cull data from abundance table and metadata file for a list of ids
######################################################################################################################
data_cull <- function( data_in=NULL, metadata_in=NULL, cull_list="cull_ids.txt", pass_file_suffix="PASS", culled_file_suffix="CULLED", debug=FALSE){

  # import list of ids to cull
 id_list <- import_idList(cull_list)
 
  # cull data file
  if( is.null(data_in)==FALSE ){
    data_matrix <- import_data(data_in)
    # file  with data retained
    pass_data_matrix <- data_matrix[ ,!(colnames(data_matrix) %in% id_list)]
    pass_data_file_name = gsub("\\.\\.", "\\.", paste(data_in, ".", pass_file_suffix, ".txt", sep="", collapse="" ))
    if(debug==TRUE){ test1 <<- pass_data_matrix; print(pass_data_file_name) }
    write.table(pass_data_matrix, file=pass_data_file_name, sep="\t", col.names = NA, row.names = TRUE, quote = FALSE, eol="\n")
    # file with data culled
    culled_data_matrix <- data_matrix[ ,(colnames(data_matrix) %in% id_list)]
    culled_data_file_name = gsub("\\.\\.", "\\.", paste(data_in, ".", culled_file_suffix, ".txt", sep="", collapse="" ))
    if(debug==TRUE){ test2 <<- culled_data_matrix; print(culled_data_file_name) }
    write.table(culled_data_matrix, file=culled_data_file_name, sep="\t", col.names = NA, row.names = TRUE, quote = FALSE, eol="\n")
  }
  
  # cull metadata file
  if( is.null(metadata_in)==FALSE ){
    # file with metadata retained
    metadata_matrix <- import_metadata(metadata_in)
    pass_metadata_matrix <- metadata_matrix[!(rownames(metadata_matrix ) %in% id_list),]
    pass_metadata_file_name = gsub("\\.\\.", "\\.", paste(metadata_in, ".", pass_file_suffix, ".txt", sep="", collapse="" ))
    write.table(pass_metadata_matrix, file=pass_metadata_file_name, sep="\t", col.names = NA, row.names = TRUE, quote = FALSE, eol="\n")
    # file with metadata culled
    culled_metadata_matrix <- metadata_matrix[(rownames(metadata_matrix ) %in% id_list),]
    culled_metadata_file_name = gsub("\\.\\.", "\\.", paste(metadata_in, ".", culled_file_suffix, ".txt", sep="", collapse="" ))
    write.table(culled_metadata_matrix, file=culled_metadata_file_name, sep="\t", col.names = NA, row.names = TRUE, quote = FALSE, eol="\n")
  }

}
######################################################################################################################

######################################################################################################################
process_batch <- function(batch_count, batch_start, batch_end, mgid_list, my_log, my_entry, my_annot, my_source, my_level, sleep_int, debug){

  if( debug==TRUE ){ print("made it to process_batch 1") }
  
  # create list of ids in the batch from the main list
  batch_list = mgid_list[batch_start:batch_end]

  # write batch informatin to log
  write( date(), file = my_log, append = TRUE)
  write( paste("BATCH:", batch_count," :: batch_start(", batch_start, ") batch_end(", batch_end, ")", sep="", collapse="" ), file = my_log, append = TRUE)
  write( paste("# batch ( ", batch_count, ") members:", file = my_log, append = TRUE) )
  for (i in 1:length(batch_list)){ write(batch_list[i], file = my_log, append = TRUE) }

  # make the call
  write("# GOING TO START THE CALL:\n" , file = my_log, append = TRUE)
  if(debug==TRUE){print("# GOING TO START THE CALL:\n")}
  current_batch <- collection(batch_list, count = c(entry=my_entry, annot=my_annot, source=my_source, level=my_level))
  write("# FINSISHED MAKING THE CALL:\n" , file = my_log, append = TRUE)
  if(debug==TRUE){print("# FINSISHED MAKING THE CALL:\n")}
  
  check_batch <- current_batch$count
  if(debug==TRUE){print("# checked bactch")}
                  
  collection_call <- msession$urls()[1]

  if(debug==TRUE){print("# checked url")}
  
  matrix_call <- msession$urls()[2]

  if(debug==TRUE){print("# tried matrix call")}
  
  write(paste("# API_CALL (matrix_call):\n", matrix_call ), file = my_log, append = TRUE)
  write(paste("# API_CALL (status_call):\n", collection_call ), file = my_log, append = TRUE)

  
  # check the status of the call -- proceed only when the data are available
  check_status(collection_call, sleep_int, my_log, debug, batch_count)
      
  return(current_batch)
}
######################################################################################################################

######################################################################################################################
check_status <- function (collection_call, sleep_int, my_log, debug, batch_count)  {

  if( debug==TRUE ){ print("made it to check_status function") }
  API_status_check<- fromJSON(getURL(collection_call))
  current_status <- API_status_check['status']      
  while ( grepl(current_status, "done")==FALSE ){
    Sys.sleep(sleep_int)
    sleep_int <- sleep_int+10
    print( paste("Sleeping for (", sleep_int, ") more seconds - waiting for call to complete; batch ( ", batch_count," )", sep="", collapse="") )
    write( paste("# API_CALL: (status check)\n", collection_call, sep="", collapse="" ), file = my_log, append = TRUE)
    write( paste("# Sleeping for (", sleep_int, ") more seconds - waiting for call to complete; batch ( ", batch_count," )", sep="", collapse=""), file = my_log, append = TRUE )
    API_status_check<- fromJSON(getURL(collection_call))
    current_status <- API_status_check['status']
  }
  
}
######################################################################################################################     

######################################################################################################################
print.tic <- function(x,...) {
    if (!exists("proc.time"))
        stop("cannot measure time")
    gc(FALSE)
    assign(".temp.tictime", proc.time(), envir = .GlobalEnv)
}
######################################################################################################################

######################################################################################################################
print.toc <- function(x,...) {
    if (!exists(".temp.tictime", envir = .GlobalEnv))
        stop("Did you tic?")
    time <- get(".temp.tictime", envir = .GlobalEnv)
    rm(".temp.tictime", envir = .GlobalEnv)
    print(res <- structure(proc.time() - time,
                           class = "proc_time"), ...)
    invisible(res)
}
######################################################################################################################

######################################################################################################################
source_https <- function(url, ...) {
  require(RCurl)
  sapply(c(url, ...), function(u) { eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv) } )
} # Sourced code is commented out below in case you need it
######################################################################################################################

######################################################################################################################
print_dl_usage <- function() {
  writeLines("  ------------------------------------------------------------------------------
  matR_batch_dl.r
  ------------------------------------------------------------------------------
  DESCRIPTION:
  This script perform a batch download using Dan's matR-apps collection-merge

  USAGE:
  (mgid_list, sleep_int = 10, my_log = \"my_log.txt\", batch_size = 50, my_entry=\"count\", my_annot=\"func\", my_source=\"Subsystem\", my_level=\"level3\", debug=FALSE, verbose=TRUE){

  ")
  stop("You are vieiwing the usage because you did not supply an mgid_list")
}
######################################################################################################################









