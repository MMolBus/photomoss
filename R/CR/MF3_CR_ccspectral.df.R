# TITLE: 

# OBJECTIVE: Two main objectives
# 1. Image segmentation between moss and background using 
# autothreshold, supervised autothreshold or manual criteria.
# 2. The measurement of moss physiological activity 


# To achieve this objectives the function calculates different 
# index of reflectance: 
# ndvi, sr, msavi, evi, ci, bsci, bi, 
# nor_r, nor_g, nor_b, ex_r, ex_g,  ex_b,  ex_gr, cive,  veg
# # library(sm)
#  i <- 1
#   obs.areas <- obs_areas
# # # # # # authothreshold.method <- method[1]
# photo <- 1
# area <- 1
# # # # pkg[!(pkg %in% installed.packages()[, "Package"])]
# # # #
# # # # if (!require("sm")) install.packages("sm")
#   wd.path <- wd
#   threshold.method <- "Mean"
# index. <-"SAT"
# R functions
#' ccspectral.df: 
#' 
#' @description Image segmentation to calculate areas of Biological Soil Covers dominated
#' by photosynthetic organisms.Calculates spectral indices, segments and 
#' classifies Biological Soil Covers (foreground) and soil (background) based on
#' global histogram threshold values. 
#' 
#' Automatic thresholding are  based on _autothresholdr_ package
#' (\href{https://onlinelibrary.wiley.com/doi/full/10.1111/jmi.12474}{Landini et al., 2017}), 
#' an adaptation to R of \href{https://imagej.net/Auto_Threshold}{Auto Threshold plugin from ImageJ}.
#' Also, thresholding can be set manually.
#' 
#' To assess segmentation performance, logical _manual.mask.test_ requires the 
#' user to provides a manually delimited outline moss contour, and then, create 
#' a binary mask that separates background (0) from foreground (1) in ImageJ. 
#' This binary mask is used as baseline (or ground true) image to compare with 
#' the automatically calculated area. The function generates a confusion matrix 
#' comparing pixel values of the baseline and the calculated values. This 
#' confusion matrix could be used to calculate two classification 
#' evaluation metrcs as True Skill Statistic (TSS) or Intersection over Union 
#' (IoU). 
#' 
#' @param wd.path string. 
#' Working directory path where you can find the mandatory directories.
#' @param chart. SpatialPolygons. 
#' Spatial polygons provided by chart.2 function that set position of color 
#' chart tiles in the pictures. 
#' @param pic.format character. 
#' Picture file format. It could be "jpg" for .jpg, .JPG and .jpeg; or "tif", 
#' for .tif format. Default = "tif"
#' @param obs.areas list of SpatialPolygons. 
#' Spatial polygons that set the areas where the samples are located in the 
#' pictures.   
#' @param pdf logical. 
#' If a pdf with results is crated. 
#' Default= F
#' @param calculate.thresh logical. 
#' Requires autothreshold calculation. See _threshold.method_ argument. 
#' Default = F 
#' @param descrip logical. 
#' Requires descriptor calculation of index values over segmented areas. 
#' See _descriptors._ argument. 
#' Default = F
#' @param manual.mask.test logical. 
#' Requires segmentation performance calculation. If TRUE, _mask_ folder with 
#' baseline masks must be in working directory. Default = F
#' @param index. character.  
#' Sets the possible spectral indices that must be calculated.
#' It must be an vector with some of the following values (representing spectral
#' index codes): "NDVI", "SR", "MSAVI", "EVI", "CI", "BSCI", "BI", "NORR", 
#' "NORG", "NORB", "EXR", "EXG", "EXB", "EXGR", "CIVE", "VEG", "HUE", "SAT", 
#' "VAL". 
#' To see the meaning of the spectral index codes that can be included, see 
#' \href{https://github.com/MMolBus/photomoss/blob/master/vignettes/vignette_Photomoss_workflow/Vignette_Photomoss.md}{this vignette}. 
#' 
#' @param threshold.method character. 
#' If _calculate.thresh_ = TRUE. 
#' Indicate which one of the possible autothresholding methods must be applied
#' over the calculated spectral index images based on _autothresholdr_ package
#' (\href{https://onlinelibrary.wiley.com/doi/full/10.1111/jmi.12474}{Landini et al., 2017}), 
#' an adaptation to R of \href{https://imagej.net/Auto_Threshold}{Auto Threshold plugin from ImageJ}.
#' It must have only *ONE* of the following values: 
#' "Huang", "IsoData", "IJDefault", "Li", "Mean", "MinErrorI", "Moments", 
#' "Otsu", "Percentile", "RenyiEntropy", "Shanbhag", "Triangle".
#' For more details, see \href{https://github.com/MMolBus/photomoss/blob/master/vignettes/vignette_Photomoss_workflow/Vignette_Photomoss.md}{this vignette}. 
#' @param threshold.vector numeric.
#' If _calculate.thresh_ = FALSE.
#' Indicate which threshold values must be applied must be applied over the 
#' calculated spectral index images.
#' It must have the same lenght as _index._ argumet. And desired threshold 
#' values for each index must be set in the same order than in _index._ 
#' argument. 
#' @param descriptors. character. 
#' Indicates what descriptor metrics must be calculated over the segmented 
#' surfaces.   
#' It can be some of the following values: "median", "mean", "sd", "min", "max",
#'  "diff.range".
#' @param chart.values dataframe. The color chart values from the spectral 
#' channels we are working with. Deffault values for Red, Green, Blue, and 
#' Near Infrared. Colour chart enable colour equalization between images using a
#' known colour reference.
#' In our case we provide the color reference for ColorChecker® Classic of 
#' X-Rite following the reference values provided in \href{https://elibrary.asabe.org/abstract.asp??JID=3&AID=25359&CID=aeaj2008&v=24&i=6&T=1}{Ritchie et al. 2008}.
#' The dataframe must be provided with the following colnames red.chart, 
#' green.chart, blue.chart, nir.chart. Each column have to contain the reference
#' color values of the color chart we are using for the red, green, blue and 
#' near infrared channels. 
#' 
#' @return 
#' A dataframe with the required results 
#'
#' @examples
#' df <- 
#' ccspectral.df(
#' wd.path ="./my_wd", 
#' pic.format = "tif", 
#' chart = chart_polys, 
#' obs.areas = obs_areas_poly_list, 
#' pdf = F, 
#' calculate.thresh = F,
#' descrip = F, 
#' manual.mask.test = F, 
#' index. = c("SR"), 
#' threshold.method = c("Li"), 
#' threshold.vector = c(0.6),
#' descriptors. = c("mean") )
#'
#' @author Manuel Molina-Bustamante
#' @export                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       

ccspectral.df <- function(wd.path,
                          chart,
                          pic.format = "tif",
                          obs.areas,
                          pdf = F,
                          calculate.thresh = F,
                          descrip = F,
                          manual.mask.test = F,
                          index. = index_list,
                          threshold.method,
                          threshold.vector,
                          descriptors. = c("median", "mean", "sd", "min", "max", "diff.range"),
                          chart.vals =
                                data.frame(red.chart = red_chart,
                                           green.chart = green_chart, 
                                           blue.chart = blue_chart, 
                                           nir.chart = nir_chart)
){  
      ### Subsection 1: Preparing data -----------------------------------------------
      ## Predefined values for indices and charts ####
      # Define a list of spectral indices and their corresponding chart values
      # These values represent standard measurements for image analysis
      index_list <- c("NDVI", "SR", "MSAVI", "EVI", "CI", "BSCI", "BI", "NORR", 
                      "NORG", "NORB", "EXR", "EXG", "EXB", "EXGR", "CIVE", "VEG", 
                      "HUE", "SAT", "VAL")
      # Red, Green, Blue, and Near Infrared (NIR) channel values for a reference color chart
      # These are essential for normalizing image data across samples
      red_chart <- c(0.17, 0.63, 0.15, 0.11, 0.31, 0.20, 0.63, 0.12, 0.57, 0.21, 
                     0.33, 0.67, 0.04, 0.10, 0.60, 0.79, 0.70, 0.07, 0.93, 0.59, 
                     0.36, 0.18, 0.08, 0.03)
      green_chart <- c(0.10, 0.32, 0.19, 0.14, 0.22, 0.47, 0.27, 0.11, 0.13, 0.06, 
                       0.48, 0.40, 0.06, 0.27, 0.07, 0.62, 0.13, 0.22, 0.95, 0.62, 
                       0.38, 0.20, 0.09, 0.03)
      blue_chart <- c(0.07, 0.24, 0.34, 0.06, 0.42, 0.42, 0.06, 0.36, 0.12, 0.14, 
                      0.10, 0.06, 0.24, 0.09, 0.04, 0.08, 0.31, 0.38, 0.93, 0.62, 
                      0.39, 0.20, 0.09, 0.02)
      nir_chart <- c(0.43, 0.87, 0.86, 0.18, 0.86, 0.43, 0.85, 0.54, 0.54, 0.79, 
                     0.49, 0.66, 0.52, 0.44, 0.72, 0.82, 0.88, 0.42, 0.91, 0.51, 
                     0.27, 0.13, 0.06, 0.02)
      
      # 1.1 Check workspace MANDATORY sub-directories ----
      # Ensure that mandatory subdirectories (e.g., nir, vis, mask) are present in the working directory.
      # These directories are needed for image processing.
      if(manual.mask.test == T){
            if(any(list.files(getwd()) %in% c("nir", "vis", "mask"))){
                  # Required subdirectories exist; proceed
            }else{
                  # Switch to the specified working directory if not already there
                  wd <- getwd()
                  setwd(wd.path)
                  on.exit(setwd(wd)) # Ensure the working directory is reset after the function ends
            }
      } else {
            if(any(list.files(getwd()) %in% c("nir", "vis"))){
                  # Required subdirectories exist; proceed
            } else {
                  # Switch to the specified working directory if not already there
                  wd <- getwd()
                  setwd(wd.path)
                  on.exit(setwd(wd)) # Ensure the working directory is reset after the function ends
            }
      }
      
      # 1.2 Order custom argument values and validate required arguments ----
      # Ensure proper ordering of surfaces, indices, and descriptors for processing
      # Validate that threshold methods and vectors are properly defined
      if(calculate.thresh == T){
            surface. = c("predict.backgr", "predict.moss")  
            if(!threshold.method %in% c("Huang", "IJDefault", "IsoData", "Li", "Mean", 
                                        "MinErrorI", "Moments", "Otsu", "Percentile", 
                                        "RenyiEntropy", "Shanbhag", "Triangle")){
                  stop("Define a valid threshold.method argument.") # Ensure valid thresholding methods
            }
      } else {
            if(!exists("threshold.vector")){
                  stop("Define a threshold.vector for the selected index.") # Threshold values are required
            } else if(length(index.) != length(threshold.vector)){
                  stop("threshold.vector must match the length of index.") # Ensure thresholds match indices
            }
            surface. = c("predict.backgr", "predict.moss")
      }
      
      # Adjust surfaces, indices, and descriptors based on their priority order
      surface_order <- c("baseline.backgr", "baseline.moss", "predict.backgr", "predict.moss")
      surface. <- surface.[order(match(surface., surface_order))]
      index_order <- index_list
      index. <- index.[order(match(index., index_order))]
      descriptors_order <- c("median", "mean", "sd", "min", "max", "diff.range")
      descriptors. <- descriptors.[order(match(descriptors., descriptors_order))]
      
      # 1.3 Create exportation folder ----
      # Generate a folder to store the output files
      out_dir <- gsub(":", ".", paste0("output ", Sys.time(), " ", threshold.method))
      dir.create(out_dir)
      
      # 1.4 Prepare a data frame for results and write an empty CSV ----
      # Define column names for the results data frame
      if(descrip == F){
            if(manual.mask.test == F){
                  # Create columns for the results data frame without descriptive statistics
                  df_names <- c("sample", "vis.file", "nir.file",
                                unlist(lapply(1:length(index.), function(i)
                                      c(apply(expand.grid(surface., index.[i]), 1, paste, collapse = "."))
                                )),
                                apply(expand.grid("threshold.value", index.), 1, paste, collapse, "."), 
                                "threshold.method")
            } else {
                  # If manual mask test is applied, add additional columns for TSS, IoU
                  df_names <- c("sample", "vis.file", "nir.file",
                                unlist(lapply(1:length(index.), function(i)
                                      c(apply(expand.grid(surface., index.[i]), 1, paste, collapse = "."))
                                )),
                                apply(expand.grid(c("TSS","IoU"), index.), 1, paste, collapse = "."),
                                apply(expand.grid("threshold.value", index.), 1, paste, collapse, "."),
                                "threshold.method")
            }
      } else {
            if(manual.mask.test == F){
                  # With descriptive statistics, add more columns
                  df_names <- c("sample", "vis.file", "nir.file",
                                unlist(lapply(1:length(index.), function(i) 
                                      lapply(1:length(surface.), function(j)
                                            c(apply(expand.grid(surface.[j], index.[i]), 1, paste, collapse = "."),
                                              apply(expand.grid(surface.[j], descriptors., index.[i]), 1, paste, collapse = ".")
                                            )
                                      )
                                )),
                                apply(expand.grid("threshold.value", index.), 1, paste, collapse = "."),
                                "threshold.method")
            } else {
                  # With descriptive statistics and manual mask test, add columns for both
                  df_names <- c("sample", "vis.file", "nir.file", 
                                unlist(lapply(1:length(index.), function(i)
                                      lapply(1:length(surface.), function(j)
                                            c(apply(expand.grid(surface.[j], index.[i]), 1, paste, collapse = "."),
                                              apply(expand.grid(surface.[j], descriptors., index.[i]), 1, paste, collapse = ".")
                                            )
                                      )
                                )),
                                apply(expand.grid(c("TSS", "IoU"), index.), 1, paste, collapse = "."),
                                apply(expand.grid("threshold.value", index.), 1, paste, collapse = "."),
                                "threshold.method")
            }
      }
      
      # Create an empty data frame with the defined structure
      df <- data.frame(matrix(ncol = length(df_names), nrow = 0))
      colnames(df) <- df_names  # Assign column names to the empty data frame
      col_class <- c(rep("character", 3), rep("numeric", length(df_names) - 4), "character")
      for (i in c(1:length(col_class))){class(df[, i]) <- col_class[i]}  # Set correct data types for each column
      rm(col_class)  # Remove temporary column class variable
      
      # Export an empty summary CSV file if it doesn't already exist
      if(calculate.thresh == TRUE){
            summary_file <- paste0(out_dir, "/", threshold.method, "_summary_data.csv")
            if(!file.exists(summary_file)){write.csv(df, summary_file, row.names = F)}  # Export summary if no file exists
      } else {
            summary_file <- paste0(out_dir, "/summary_data.csv")
            if(!file.exists(summary_file)){write.csv(df, summary_file, row.names = F)}  # Export summary if no file exists
      }
      
      # 1.5 Import images as list --------------------------------------------------
      vis_files <- list.files(path = "./vis")  # List all VIS files
      nir_files <- list.files(path = "./nir")  # List all NIR files
      if(manual.mask.test == T){mask_files <- list.files(path = "./mask", pattern = ".tif$")}  # If manual mask test, list mask files
      
      # 1.6 Check if a matching error exists between lists --------------------------
      if(length(vis_files) != length(nir_files)){stop("Different number of VIS and NIR photos")}  # Error check for mismatched file counts
      
      # Total number of samples per observation area
      total_samples <- length(obs.areas)
      
      # 1.7 Set sample names -------------------------------------------------------
      # Extract cell names from .roi files
      cell_names <- gsub(".*/", "", list.files(path = "./rois", pattern = ".roi$", full.names = F, recursive = T))
      # Count the samples per picture in the "rois" directory
      samples.per.pic <- unlist(lapply(1:(length(list.dirs("rois")) - 1), 
                                       function(i) length(list.files(list.dirs("rois")[i + 1]))))
      
      ## ERROR CHECK: Ensure `samples.per.pic` matches with number of VIS files
      if(sum(samples.per.pic) != length(cell_names)) {
            stop("Mismatch between ROI files and sample counts in `samples.per.pic`")
      }
      
      # Combine VIS file names with corresponding cell names in a data frame
      all_named <- data.frame(photo = unlist(lapply(1:length(vis_files),
                                                    function(i) rep(vis_files[i], each = samples.per.pic[i]))), 
                              cell = cell_names)
      
      # Optional: Use sample names from a CSV file if available
      if(file.exists("names.csv")){
            sample_names <- c(as.character(read.csv("names.csv")[, 1]))
            if(length(sample_names) != total_samples){
                  stop("File of sample names contains less/more names than samples")
            }
            all_named$moss <- sample_names  # Assign sample names from the CSV file
      } else {
            all_named$moss <- c(names = paste0("obs_", 1:total_samples))  # Generate default sample names if no file is found
      }
      print(all_named)
      
      
      ### Subsection 2: Perform calculations -----------------------------------------
      all <- data.frame(Var1 = 1:length(all_named[,1]), Var2 = 1:length(obs.areas))
      all <- dplyr::arrange(all, Var1)
      print(all)
      
      start_time <- Sys.time()  # Capture the start time for the calculations
      message(paste("Starting calculations at", start_time))  # Print the start time
      
      # Apply the calculations to each sample
      apply(all, 1, function(pair){
            calcs(pair[1],
                  pair[1],
                  obs.areas = obs.areas,
                  vis.files = all_named[,1],
                  nir.files = all_named[,1],
                  chart = chart,
                  pic.format = pic.format,
                  mask.files = mask_files,
                  manual.mask.test = manual.mask.test,
                  summary.file = summary_file,
                  total.samples = total_samples,
                  index. = index.,
                  descriptors. = descriptors.,
                  calculate.thresh = calculate.thresh,
                  descrip = descrip,
                  threshold.method = threshold.method,
                  threshold.vector = threshold.vector,
                  pdf = pdf,
                  start.time = start_time,
                  chart.vals
            )
      })
      
      # Inform the user about the location of the output files
      message("Processed files may be found at: ", paste0(wd.path, out_dir))
}