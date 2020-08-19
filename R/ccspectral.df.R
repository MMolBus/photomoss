# TITLE: 
# ccspectral.multiareas

# OBJECTIVE: Two main objectives
# The meassurement of moss physiological activity 
# Image segmentation between moss and background using 
# autothreshold, supervised autothreshold or manual criteria.

# To achive this objectives the function calculates diferent 
# index of reflectance: 
# ndvi, sr, msavi, evi, ci, bsci, bi, 
# nor_r, nor_g, nor_b, ex_r, ex_g,  ex_b,  ex_gr, cive,  veg
# # library(sm)
#  i <- 1
#   obs.areas <- obs_areas
# # # # # # authothreshold.method <- method[1]
  # photo <- 1
  # area <- 1
# # # # # pkg[!(pkg %in% installed.packages()[, "Package"])]
# # # # #
# # # # # if (!require("sm")) install.packages("sm")
#   tif.path <- md
#   threshold.method <- "Mean"
# index. <-"SAT"

ccspectral.df <- function(tif.path,
                          chart,
                          obs.areas,
                          pdf = F,
                          calculate.thresh = T,
                          descrip = F,
                          manual.mask.test=F,
                          index. = c("NDVI", "SR", "MSAVI", "EVI", "CI", "BSCI", "BI",
                                     "NORR", "NORG", "NORB", "EXR", "EXG", 
                                     "EXB", "EXGR", "CIVE", "VEG", 
                                     "HUE", "SAT", "VAL"),
                          threshold.method=c("Huang"),
                          threshold.vector,
                          descriptors. = 
                            c("median","mean","sd","min",
                              "max","diff.range")
                          )
{  

# Charge library ----------------------------------------------------------
# Subsection 1: Preparing data -----------------------------------------------------------------------------------------
  # Check workspace MANDATORY sub-directories =============================================================
    if(manual.mask.test==T){
      if(any(list.files(getwd()) %in% "nir") &
       any(list.files(getwd()) %in% "vis") &
       any(list.files(getwd()) %in% "mask")) {
    }else{
      wd <- getwd()
      setwd(tif.path)
      on.exit(setwd(wd))
    }
      }else{
      if(any(list.files(getwd()) %in% "nir") &
         any(list.files(getwd()) %in% "vis")) {
      }else{
        wd <- getwd()
        setwd(tif.path)
        on.exit(setwd(wd))}}
    
    # Order custom arguments values and test required arguments =============================================
    
    if(calculate.thresh==T){
      surface. = c("auto.thresh.moss", "auto.thresh.backgr")  
      if(any(threshold.method==c("Huang", "IJDefault", 
                                 "IsoData", "Li", 
                                 "Mean", "MinErrorI", 
                                 "Moments", "Otsu",
                                 "Percentile", "RenyiEntropy",
                                 "Shanbhag", "Triangle"))==F){
      stop("if you want to calculate auto threshold value 
             you need to define a valid threshold.method argument.")}
    }else{
      if(exists("threshold.vector")==F){
        stop("if you don't want to calculate autothreshold values you 
        need to define a threshold.vector values for the selected index.")
      }else{ 
        if(length(index.)!=length(threshold.vector)){
          stop("thershold.vector must have the same length as the 
              index. argument")}
      }
      surface. = c("predef.thresh.moss", "predef.thresh.backgr")
      }
    
    if(manual.mask.test==T){
     surface. <- c(surface.,"manual.mask.moss", "manual.mask.backgr",  "backgr.as.backgr","moss.as.backgr",
                   "backgr.as.moss", "moss.as.moss")
      }
    
    surface_order <- c("auto.thresh.moss",   "auto.thresh.backgr",
                       "predef.thresh.moss", "predef.thresh.backgr",
                       "manual.mask.moss",   "manual.mask.backgr")
    surface. <- surface.[order(match(surface., surface_order))]
   
    index_order <- c("NDVI", "SR", "MSAVI", "EVI", "CI", "BSCI", "BI",
                     "NORR", "NORG", "NORB", "EXR", "EXG", "EXB", "EXGR", 
                     "CIVE", "VEG", "HUE", "SAT", "VAL") 
    index. <- index.[order(match(index., index_order))]
   
    descriptors_order <- c("median", "mean", "sd", "min",
                           "max", "diff.range"
                           # "threshold",
                           # "n.cell"
                           )
    descriptors. <- descriptors.[order(match(descriptors., descriptors_order))]
  
    # Create exportation folder =============================================================================
  
    out_dir <- paste0("output ",Sys.time(), " ", threshold.method)
    out_dir <- gsub(":", ".", out_dir)
    dir.create(out_dir)
    # Prepare dataframe for exportation results and write empty csv =============================================
  
  # Create empty data.frame -----------------------------
    if(descrip==F){
      if(calculate.thresh==T){#if you want to calculate autothresholds
      if(manual.mask.test==F){
        df_names <-
          c("sample", "vis.file", "nir.file",
            do.call(c,
                    lapply(1:length(index.), function(i)
                      c(apply(expand.grid(surface.,
                                          index.[i]), 1, paste, collapse=".")
                      )
                    )
            ),
            apply(expand.grid("threshold.value",
                              index.), 1, paste, collapse="."),
            "threshold.method")  
      }else{
      df_names <-
        c("sample", "vis.file", "nir.file", 
          do.call(c,
                  lapply(1:length(index.), function(i)
                    c(apply(expand.grid(
                      surface.,
                      index.[i]), 1, paste, collapse = ".")
                    )
                  )
          ),
          apply(expand.grid("threshold.value",
                            index.), 1, paste, collapse="."),
                "threshold.method")}
      
    }else{#if you don't want to calculate autothresholds, use a threshold vector value,
      if(manual.mask.test==F){
        df_names <-
          c("sample", "vis.file", "nir.file",
            do.call(c,
                    lapply(1:length(index.), function(i)
                      c(apply(expand.grid(
                        surface.,
                        index.[i]), 1, paste, collapse=".")
                      )
                    )
            ),
            apply(expand.grid("threshold.value",
                              index.), 1, paste, collapse="."),
            "threshold.method")
        }else{
          df_names <-
            c("sample", "vis.file", "nir.file", 
              do.call(c,
                      lapply(1:length(index.), function(i)
                        c(apply(expand.grid(
                          surface.,
                          index.[i]), 1, paste, collapse = ".")
                        )
                      )
              ),
              apply(expand.grid("threshold.value",
                                index.), 1, paste, collapse="."),
              "threshold.method")}
    }
    }else{
      if(calculate.thresh==T){#if you want to calculate autothresholds
        if(manual.mask.test==F){
          df_names <-
            c("sample", "vis.file", "nir.file",
              do.call(c,
                      do.call(c,
                              lapply(1:length(index.), function(i)
                                lapply(1:length(surface.), function(j)
                                  c(apply(expand.grid(surface.[j],
                                                      index.[i]), 1, paste, collapse="."),
                                    apply(expand.grid(surface.[j],
                                                      descriptors.,
                                                      index.[i]), 1, paste, collapse=".")
                                  )
                                )
                              )
                      )
              ),
              apply(expand.grid("threshold.value",
                                index.), 1, paste, collapse="."),
              "threshold.method")  
        }else{
          df_names <-
            c("sample", "vis.file", "nir.file",
              do.call(c,
                      do.call(c,
                              lapply(1:length(index.), function(i)
                                lapply(1:length(surface.), function(j)
                                  c(apply(expand.grid(surface.[j],
                                                      index.[i]), 1, paste, collapse="."),
                                    apply(expand.grid(surface.[j],
                                                      descriptors.,
                                                      index.[i]), 1, paste, collapse=".")
                                  )
                                )
                              )
                      )
              ),
              apply(expand.grid("threshold.value",
                                index.), 1, paste, collapse="."),
              "threshold.method")}
        
      }else{#if you don't want to calculate autothresholds, use a threshold vector value,
        if(manual.mask.test==F){
          df_names <-
            c("sample", "vis.file", "nir.file",
              do.call(c,
                      do.call(c,
                              lapply(1:length(index.), function(i)
                                lapply(1:length(surface.), function(j)
                                  c(apply(expand.grid(surface.[j],
                                                      index.[i]), 1, paste, collapse="."),
                                    apply(expand.grid(surface.[j],
                                                      descriptors.,
                                                      index.[i]), 1, paste, collapse=".")
                                  )
                                )
                              )
                      )
              ),
              apply(expand.grid("threshold.value",
                                index.), 1, paste, collapse="."),
              "threshold.method")
        }else{
          df_names <-
            c("sample", "vis.file", "nir.file", "real.moss.cover",
              do.call(c,
                      do.call(c,
                              lapply(1:length(index.), function(i)
                                lapply(1:length(surface.), function(j)
                                  c(apply(expand.grid(surface.[j],
                                                      index.[i]), 1, paste, collapse="."),
                                    apply(expand.grid(surface.[j],
                                                      descriptors.,
                                                      index.[i]), 1, paste, collapse=".")
                                  )
                                )
                              )
                      )
              ),
              apply(expand.grid("threshold.value",
                                index.), 1, paste, collapse="."),
              "threshold.method")}
      }
    }
  
      df <- data.frame(matrix(ncol = length(df_names), nrow = 0))
      colnames(df) <- df_names
      
      # set df col class
      col_class <-
        c(rep("character", 3), rep("numeric", length(df_names) - 4),"character")
      for (i in c(1:length(col_class))) {
        class(df[, i]) <- col_class[i]
      }
   
    rm(col_class)
    
    # Create results csv-----------------------------------------------------
    if(calculate.thresh == TRUE){
    summary_file <- paste0(out_dir,  paste0("/",threshold.method,"_","summary_data.csv"))
    if(!file.exists(summary_file)){write.csv(df, summary_file, row.names = F)}
    } else{
      summary_file <- paste0(out_dir,  paste0("/summary_data.csv"))
      if(!file.exists(summary_file)){write.csv(df, summary_file, row.names = F)}}
    
    
  
  # Import images as list ---------------------------------------------------
    vis_files <- list.files(path = "./vis")
    nir_files <- list.files(path = "./nir")
    if(manual.mask.test==T){
    mask_files <- list.files((path = "./mask"))}
    
  
  # Check if a matching error exists between lists --------------------------
    if (length(vis_files) != length(nir_files)) {
      stop("Different number of VIS and NIR photos")
    }
    # Samples per picture 
    # total samples per picture (same number of samples in each picture) in this case one sample one picture
    
    # old code (recycling same obs.areas polygons for each picture)
    # total_samples <- length(vis_files)*length(obs.areas)
    # message(paste0(length(vis_files), " pictures with ", length(obs.areas), " areas each = ", total_samples, " total samples"))
    
    # new code (one obs area for picture)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
    # total_samples <- length(obs.areas)
    # message(paste(length(vis_files), "pictures with", 
    #               length(obs.areas) / length(vis_files), 
    #               "areas each =", total_samples, "total samples"
    #   )
    # )
    
    
    # new code (indetermined obs areas for picture)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
    total_samples <- length(obs.areas)
    # message(paste(length(vis_files), "pictures with",
    #               length(obs.areas) / length(vis_files),
    #               "areas each =", total_samples, "total samples"
    #   )
    # )
    
    # Set sample names #############################################################################
    
    # new code
   # we have the .roi files in picture named folders in the "rois" directory
    samples.per.pic <- 
      do.call(c,
              lapply(1:(length(list.dirs("rois"))-1), 
                     function(i) length(list.files(list.dirs("rois")[i+1]))))
    all_named       <- 
      data.frame(photo=do.call(c,lapply(1:length(vis_files),
                                        function(i) rep(vis_files[i], each=samples.per.pic[i]))),
                 alveolo=names(obs.areas))
        if (file.exists("names.csv")) {
      sample_names <- c(as.character(read.csv("names.csv")[, 1]))
      if (length(sample_names) != total_samples) {
        stop("File of sample names contains less/more names than samples")
      }
      all_named$moss <- sample_names
    } else{
      all_named$moss <- c(names = paste0("obs_", 1:(total_samples)))
    }
    print(all_named)
    
  # Subsection 2: set functions ------------------------------------------------------------------------------------------
    # Calcs function =========================================================================================
  
    # source("./ccspectral/calcs.autothreshold.R")
    
  # Subsection 3: make calculations  ----------------------------------------
    # all <- expand.grid(1:length(vis_files), 1:length(obs.areas))
    all <- data.frame(Var1 = 1:length(all_named[,1]), Var2 = 1:length(obs.areas))
  
    all <- dplyr::arrange(all, Var1)
    print(all)
    
    start_time <- Sys.time()
    message(paste("Starting calculations at", start_time))
  apply(all, 1, function(pair) {
    calcs(
      pair[1],
      pair[1],
      obs.areas = obs.areas,
      vis.files = all_named[,1],
      nir.files = all_named[,1],
      chart=chart,
      mask.files = mask_files,
      manual.mask.test = manual.mask.test,
      summary.file = summary_file,
      total.samples = total_samples,
      index.= index.,
      descriptors.= descriptors.,
      calculate.thresh = calculate.thresh,
      descrip = descrip,
      threshold.method = threshold.method,
      threshold.vector = threshold.vector,
      pdf = pdf,
      start.time=start_time
      )
  })
# for(i in nrow(all)){
#     calcs(
#       photo = i,
#       area = i,
#       obs.areas = obs.areas,
#       vis.files = all_named[,1],
#       nir.files = all_named[,1],
#       chart=chart,
#       mask.files = mask_files, 
#       manual.mask.test = manual.mask.test,
#       summary.file = summary_file,
#       total.samples = total_samples,
#       index.= index.,
#       descriptors.= descriptors.,
#       calculate.thresh = calculate.thresh,
#       descrip = descrip,
#       threshold.method = threshold.method,
#       pdf = pdf
#     )
#   }
  showwarnings
  message("Processed files may be found at: ", paste0(tif.path, out_dir))
}
