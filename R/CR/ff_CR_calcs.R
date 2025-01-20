#CALCS function for ccspectral
      # vis.files = all_named[,1]
      # nir.files = all_named[,1]
      # manual.mask.test = manual.mask.test
      # mask.files = mask_files
      # summary.file = summary_file
      # total.samples = total_samples
      # index.= index.
      # descriptors.= descriptors.
      # calculate.thresh = calculate.thresh
      # threshold.method = threshold.method
      # area <- 1
      # photo <- 1
      # chart.vals <- chart.vals
# Function to process image data, calculate indices, apply thresholds, 
# and compute surface descriptors, with the option to generate a summary 
# report and visualizations in a PDF.

calcs <- function(photo,
                  area, 
                  obs.areas, 
                  vis.files,
                  nir.files,
                  pic.format,
                  manual.mask.test,
                  mask.files, 
                  summary.file,
                  chart,
                  total.samples, 
                  index., 
                  descriptors., 
                  calculate.thresh, 
                  threshold.vector,
                  descrip,
                  threshold.method,
                  pdf,
                  start.time,
                  chart.vals
                  ){
# Step 1: Prepare data and check inputs-----------------------------------------

      # Extract the area associated with the current photo and check if 
      # the number of sample names match the total number of samples.
      # Also checks if manual masking is used and prepares the mask file.
      
      ## 1.1 Define input values------------------------------------------------
      obs_area <- obs.areas[[1]]
      vis_photo <- vis.files[1]
      nir_photo <- nir.files[1]
      if(manual.mask.test == T){mask_photo <- mask.files[photo]}
      
      ## 1.2 Select and set sample names ---------------------------------------
      done_samples <- nrow(data.table::fread(summary.file, select = 1L, header = T))
      # Check if file name exists, if not, set default sample names
      if (file.exists("names.csv")) {
            sample_names <- c(as.character(read.csv("names.csv")[, 1]))
            if (length(sample_names) != total.samples) {
                  stop("File of sample names contains less/more names than samples")
            }
      } else{
            sample_names <- c(names = paste0("obs_", 1:(total.samples)))
      }
      sample_name <- sample_names[photo]
      
      ## 1.3. Check all single elements that have been correctly set -----------
   print(paste("vis picture name: ", as.character(vis_photo)))
   print(paste("nir picture name: ", as.character(nir_photo)))
   if(manual.mask.test == T){print(paste("Baseline file", mask_photo))}
   
# Step 2: Cell extraction and colour calibration -------------------------------
       ## 2.1 Read and create raster from image
        if(pic.format == "tif"){
              ### 2.1.a Read and create raster from tif image-------------------
            if(manual.mask.test == T){
              all_bands <- raster.tiff.ccspectral(
                vis.photo = vis_photo,
                nir.photo = nir_photo,
                manual.mask.test = manual.mask.test,
                mask.photo = mask_photo)
            }else{
              all_bands <- raster.tiff.ccspectral(
                vis.photo = vis_photo,
                nir.photo = nir_photo,
                manual.mask.test = manual.mask.test)
            }      
        }else{
            if(manual.mask.test == T){
              all_bands <- raster.jpg.ccspectral(
              ### 2.1.b Read and create raster from jpg image-------------------
                vis.photo = vis_photo,
                nir.photo = nir_photo,
                manual.mask.test = manual.mask.test,
                mask.photo = mask_photo)
            }else{
              all_bands <- raster.jpg.ccspectral(
                vis.photo = vis_photo,
                nir.photo = nir_photo,
                manual.mask.test = manual.mask.test)
            }
      }
       ## 2.2 Calibrate colour  cell.extract.color.cal.fun function-------------
        calibration_results <- cell.extract.color.cal.fun(
                                    obs.area = obs_area,
                                    all.bands = all_bands,
                                    chart = chart,
                                    manual.mask.test = manual.mask.test,
                                    chart.vals = chart.vals,
                                    pdf = pdf)
        if(pdf == T && manual.mask.test == T){moss_poly <- calibration_results[7]}
# 3. Calculate index values, as raster and as dataframe ------------------------
   # Create a list with calibration results: matrix, raster and index calculation (result)
   list_raster_results <- index.calc.fun(
                                raster.mat  = calibration_results[[1]],
                                raster.band = calibration_results[[2]],
                                index. = index.)
  ## 3.1 Calculate threshold results -------------------------------------------
   list_threshold_results <- calculate.raster.thresh.fun(
                                list.raster.results = list_raster_results,
                                calculate.thresh = calculate.thresh,
                                threshold.method = threshold.method,
                                threshold.vector = threshold.vector)
  ## 3.2 Extract mask values ---------------------------------------------------
  if(manual.mask.test == T){
    # Set data frame list with cell coordinates(x,y) index values(z) 
    # threshold mask surface and manual mask (baseline) surface.
    # Additionally we compare manual mask surgface and threshold mask surface
    # We create new surface classes (as new columns in the data frame )
    # by crossing the two classification as follows:
    # True.Negative (TN) => baseline background classified as background.
    # False.Positive (FP) => baseline background classified as moss.
    # False.Negative (FN)=> baseline moss classified as background.
    # True.Positive (TP)=> baseline moss classified as moss.
  # extract mask pixel coordinates
    coor <- coordinates(calibration_results[[1]])
   
    failed_thresholds <- 
          lapply(1:2, 
                 function(i) 
                       list_threshold_results[[i]][is.na(list_threshold_results[[2]]) == T]
                 )
    failed_threshold_names <- 
          gsub("_thresh_mask", "", names(failed_thresholds[[1]]))
    
    succesfull_thresholds <- 
          lapply(1:2, 
                 function(i)
                       list_threshold_results[[i]][is.na(list_threshold_results[[2]]) != T]
                 )
    succesfull_threshold_names <- 
          gsub("_thresh_mask", "", names(succesfull_thresholds[[1]]))
    
    # create class surface prediction code: 00 = TN, 01 = FP, 10 = FN, 11 = TP 
    surface_class <- lapply(grep(paste(succesfull_threshold_names, collapse = "|"), index.),
                              function(i)
                                 paste0(as.integer(values(calibration_results[[2]][[4]])),
                                        as.integer(values(list_threshold_results[[1]][[i]]))
                                        )
                             )
    class_label <- c("00", "01", "10", "11")

    binary_surfaces <- lapply(1:length(surface_class),
                       function(i)
                       varhandle::to.dummy(surface_class[[i]], "surface")
                       )
    names(binary_surfaces) <- succesfull_threshold_names
    
    failed_binary_surfaces <- matrix(as.numeric(rep(NA, 4*ncell(list_raster_results[[1]]))),
                                                nrow = ncell(list_raster_results[[1]]),
                                                ncol = 4)
    failed_binary_surfaces <- lapply(1:length(failed_threshold_names), 
                                       function(i) failed_binary_surfaces)
      
   names(failed_binary_surfaces) <- failed_threshold_names
      
   binary_surfaces <- c(binary_surfaces, failed_binary_surfaces)
   binary_surfaces <- binary_surfaces[match(index., names(binary_surfaces))]
   # Correct if are less than 4 classes when we cross baseline mask and calculated mask
   for(i in c(1:length(binary_surfaces))[index. %in% succesfull_threshold_names]){
         if(ncol(binary_surfaces[[i]])!= 4){
                      cols <- c("surface.00", "surface.01", "surface.10", "surface.11")
                      missing_colnames <- cols[is.element(cols, colnames(binary_surfaces[[i]])) != T]
                      missing_cols <- matrix(0, ncell(list_threshold_results[[1]][[1]]), ncol = length(missing_colnames))
                      colnames(missing_cols) <- missing_colnames 
                      binary_surfaces[[i]] <- cbind(binary_surfaces[[i]], missing_cols)
                      binary_surfaces[[i]] <- binary_surfaces[[i]][, match(cols, colnames(binary_surfaces[[i]]))]
        }else{
          binary_surfaces[[i]] <- binary_surfaces[[i]]
        }
   }
   list_df_results <- lapply(1:length(binary_surfaces), function(i)
                              cbind(
                                coor,
                                terra::values(list_raster_results[[i]]),
                                terra::values(calibration_results[[2]][[4]]),
                                as.integer(values(list_threshold_results[[1]][[i]])),
                                binary_surfaces[[i]][,1],
                                binary_surfaces[[i]][,2],
                                binary_surfaces[[i]][,3],
                                binary_surfaces[[i]][,4]
                                )
                      )
    # transform in dataframe
    list_df_results <- lapply(c(1:length(list_raster_results)), function(i)
                              as.data.frame(list_df_results[[i]]))
    # Set colnames
    colnames <- c("x", "y", "index_value", "baseline.surface.class", "predict.surface.class", "TN","FP", "FN", "TP")
    list_df_results <- lapply(list_df_results, setNames, colnames)
    rm(colnames, surface_class, binary_surfaces)
    }else{ 
    coor <- coordinates(calibration_results[[1]])
    # Set df list with cell coordinates(x,y) indexvalues(z) and mask threshold values (surface)
    list_df_results <- lapply(c(1:length(list_raster_results)), function(i)
                                 cbind(
                                   coor,
                                   terra::values(list_raster_results[[i]]),
                                   terra::values(list_threshold_results[[1]][[i]])
                                   ))
    # transform in data frame
    list_df_results <- lapply(c(1:length(list_raster_results)), function(i)
                              as.data.frame(list_df_results[[i]]))
    # Set colnames
    colnames <- c("x", "y", "index_value", "surface_threshold")
    list_df_results <- lapply(list_df_results, setNames, colnames)
    names(list_df_results) <- names(list_raster_results)
    rm(colnames)
    }
  if(pdf == FALSE){
                rm(list_raster_results)
                list.results <- list(list_df_results)
                names(list.results) <- c("data.frames")
  }else{
    # List raster results an df results
    list.results <- list(list_df_results, list_raster_results)
    names(list.results) <- c("data.frames", "rasters")}
  # 8. Descriptors calculation ----
  if(descrip == F){
        if(manual.mask.test == F){
              int_surf_cover <- unname(do.call(c, lapply(seq_along(index.), 
                                                         function(i) 
                                                               c(unlist(lapply(0:1, function(k)
                                                                     c(table(list.results[[1]][[i]][, 4][list.results[[1]][[i]][, 4] == k]), 0)[-2])))))
                                       )
              int_surf_cover[is.nan(int_surf_cover)] <- NA
        }else{
              int_surf_cover <- unname(do.call(c, lapply(seq_along(index.), function(i)
                                c(unlist(lapply(0:1, function(k)
                                      c(table(list.results[[1]][[i]][, 4][list.results[[1]][[i]][, 4] == k]), 0)[-2])),
                                  unlist(lapply(0:1, function(k)
                                        c(table(list.results[[1]][[i]][, 5][list.results[[1]][[i]][, 5] == k]), 0)[-2])),
                                  c(table(list.results[[1]][[i]][, 6][list.results[[1]][[i]][, 6] == 1]), 0)[-2],
                                  c(table(list.results[[1]][[i]][, 7][list.results[[1]][[i]][, 7] == 1]), 0)[-2],
                                  c(table(list.results[[1]][[i]][, 8][list.results[[1]][[i]][, 8] == 1]), 0)[-2],
                                  c(table(list.results[[1]][[i]][, 9][list.results[[1]][[i]][, 9] == 1]), 0)[-2])))
                                )
              TSS.IoU <- do.call(c,lapply(seq_along(index.), function(i)
                                          TSS.IoU.calc(list.results[[1]][[i]])))
              int_surf_cover <- c(int_surf_cover, TSS.IoU)
              int_surf_cover[is.nan(int_surf_cover)] <- NA
        }
  }else{
        if(manual.mask.test == F){
              int_surf_cover <- unlist(lapply(seq_along(index.), function(i)
                    do.call(c, lapply(0:1 , function(k)
                          descriptor.fun(list.results[[1]][[i]][, 3][list.results[[1]][[i]][, 4] == k],
                                         descriptors.)))
                    )
                    )
              int_surf_cover[is.nan(int_surf_cover)] <- NA
        }else{
              int_surf_cover <- lapply(seq_along(index.), function(i)
                                  unlist(lapply(4:5, function(j)
                                  unlist(lapply(0:1, function(k)
                                  descriptor.fun(list.results[[1]][[i]][, 3][list.results[[1]][[i]][, j] == k],
                                  descriptors.))))))
              test_mask_surfaces <- lapply(seq_along(index.), function(i)
                                    unlist(lapply(6:9, function(j)
                                    descriptor.fun(list.results[[1]][[i]][, 3][list.results[[1]][[i]][, j] == 1],
                                    descriptors.))))
              int_surf_cover <- unlist(lapply(seq_along(index.), function(i)
                                c(int_surf_cover[[i]], test_mask_surfaces[[i]])))
              TSS.IoU <- do.call(c, lapply(seq_along(index.), function(i)
                              TSS.IoU.calc(list.results[[1]][[i]])))
              
              int_surf_cover <- c(int_surf_cover, TSS.IoU)
              int_surf_cover[is.nan(int_surf_cover)] <- NA
              rm(test_mask_surfaces)
        }
  }
  # 9. START dataframe for index values presentation ----
  dat <- read.csv(summary.file)
  if(calculate.thresh == T){
        theresholds.results <- unlist(list_threshold_results[[2]])
        new_dat <- as.data.frame(as.list(unname(
                                                c(sample_name,
                                                   vis_photo,
                                                   nir_photo,
                                                   int_surf_cover,
                                                   theresholds.results,
                                                   threshold.method)
                                                  )))
  }else{
        new_dat <- as.data.frame(as.list(unname(
                                                c(sample_name,
                                                 vis_photo,
                                                 nir_photo,
                                                 int_surf_cover,
                                                 threshold.vector,
                                                 "Predefined")
                                                )))
  }
  colnames(new_dat) <- colnames(dat)
  dat_bind <- rbind(dat, new_dat)
  write.csv(dat_bind, summary.file, row.names = F)
  # 10. Create pdf to plot results ----
  if(pdf == T){
    # plot pdf with results (operated by lists)
    pdf_name <- paste0(out_dir, "/", sample_name, ".pdf")
    # run plotpdf 
    plotpdf(lhist = lhist,
            lind = index.,
            lman =  moss_manual_int_list,
            lover = overlap_index_list,
            i.names = index_names,
            asp = asp,
            pdf.name = paste0(sample_name, ".pdf"))
    }
# print timer check point  
       loop_time <- strsplit(as.character((as.numeric(Sys.time()) - as.numeric(start.time))/60), "\\.")[[1]]
       loop_time[2] <- round(60 * as.numeric(paste0("0.", as.character(loop_time[2]))))
        
       message(paste0(sample_name, " processed."))
       message(paste0("Made", done_samples + 1, "of", total.samples, "total samples", "in", loop_time[1], "mins", loop_time[2], "secs."))
}     
