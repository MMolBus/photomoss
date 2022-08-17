# # CALCS function, for ccspectral
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

calcs <- function(photo,
                  area, 
                  obs.areas, 
                  vis.files,
                  nir.files,
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
                  start.time
                  ){
  # Prepare data
  obs_area   <- obs.areas[[area]]
  vis_photo  <- vis.files[photo]
  nir_photo  <- nir.files[photo]
  if(manual.mask.test==T){
    mask_photo <- mask.files[photo]
    }

  # Select and set sample name 
  
  done_samples <-
    nrow(data.table::fread(summary.file, select = 1L, header = T))
  if (file.exists("names.csv")) { sample_names <- c(as.character(read.csv("names.csv")[, 1]))
  if (length(sample_names) != total.samples) 
    {stop ("File of sample names contains less/more names than samples")}
  } else{
    sample_names <- c(names = paste0("obs_", 1:(total.samples)))
  }
  
  sample_name <- sample_names[photo]
  
  # Check all single elements have been correctly set ===========================

  print(paste("vis picture name: ", as.character(vis_photo)))
  print(paste("nir picture name: ", as.character(nir_photo)))
  # print(paste("Roi to sample correspondance:", paste0(cell_names[area], " = ", sample_name)))
  if(manual.mask.test==T){
  print(paste("Baseline file", mask_photo))
  }
  # Cell extraction and color calibration -----------------------------------------------------
  # Read and create raster from tiff =====================================
  
  if(manual.mask.test==T){
    all_bands <-  raster.tiff.ccspectral(vis.photo = vis_photo, nir.photo = nir_photo, 
                                         manual.mask.test = manual.mask.test, 
                                         mask.photo = mask_photo)
  }else{
    all_bands <-  raster.tiff.ccspectral(vis.photo = vis_photo, nir.photo = nir_photo, 
                                         manual.mask.test = manual.mask.test)
  }

  # Calibrate color with color checker
  
  calibration_results <-
    cell.extract.color.cal.fun(
      obs.area = obs_area,
      all.bands = all_bands,
      chart = chart,
      manual.mask.test = manual.mask.test,
      pdf = pdf
    )
  
  if(pdf==T && manual.mask.test==T){
    moss_poly <- calibration_results[7]
  }
  
  ###########################################################################  
  # Calculate index values, as raster and as dataframe ----------------------
  ############################################################################  

  
  list_raster_results <- index.calc.fun(raster.mat  = calibration_results[[1]], 
                                       raster.band = calibration_results[[2]] , 
                                       index. = index.
                                       # calculate.thresh=calculate.thresh, 
                                       # threshold.vector,
                                       # calculate.thresh = calculate.thresh,
                                       # manual.mask.test = manual.mask.test,
                                       # threshold.method = threshold.method,
                                       # pdf = pdf
                                      )
  
  # Calculate thershold results
    
    list_threshold_results <-
      calculate.raster.thresh.fun(
        list.raster.results = list_raster_results,
        calculate.thresh    = calculate.thresh,
        threshold.method    = threshold.method,
        threshold.vector    = threshold.vector
        )
    
    
 # save list threshold results
    print("saving threshold rasters")
    lapply(seq_along(list_threshold_results[[1]]), function(i)
      writeRaster(list_threshold_results[[1]][[i]], 
                  paste(index.[i],threshold.method, 
                        paste0(sample_name,".tif"), 
                        sep="_"), 
                        overwrite=T))
    
  # Extract mask values -----------------------------------------------------
  #extract mask pixel coordinates
  if(manual.mask.test==T){
    # Set data frame list with cell coordinates(x,y) index values(z) 
    # mask threshold (surface) and mask manual(surface)
    # Additionnaly we compare manual segmentation and threshold segmentation
     # We create new surface classes (as new cols in the data frame )
    # by crossing the two classification as follows:
    # True.Negative => baseline background classified as background.
    # False.Positive => baseline background classified as moss.
    # False.Negative => baseline moss classified as background.
    # True.Positive => baseline moss classified as moss.
    coor <- 
      coordinates(calibration_results[[1]])
    failed_thresholds <-
      lapply(1:2, function(i)
        list_threshold_results[[i]][is.na(list_threshold_results[[2]])==T])
    failed_threshold_names <-
      gsub("_thresh_mask","",names(failed_thresholds[[1]]))
    succesfull_thresholds <-
      lapply(1:2, function(i)
        list_threshold_results[[i]][is.na(list_threshold_results[[2]])!=T])
    succesfull_threshold_names <-
      gsub("_thresh_mask","",names(succesfull_thresholds[[1]]))
    
    surface_class <-
      lapply(grep(paste(succesfull_threshold_names, collapse = "|"), index.),
             function(i)
               paste0(as.integer(values(calibration_results[[2]][[4]])==0),
                      as.integer(values(list_threshold_results[[1]][[i]]))
                      )
             )
    
    class_label <- c( "00",     "10",     "01",     "11" )
      if(require(varhandle)!=T){
        install.packages("varhandle")
        require(varhandle)}
      binary_surfaces <-
        lapply(1:length(surface_class),
               function(i)
                 varhandle::to.dummy(surface_class[[i]], "surface")
               )
      names(binary_surfaces) <- 
        succesfull_threshold_names
      
      failed_binary_surfaces <-
        matrix(as.numeric(rep(NA,4*ncell(list_raster_results[[1]]))),
               nrow =ncell(list_raster_results[[1]]),
               ncol = 4)
      
      failed_binary_surfaces <-
        lapply(1:length(failed_threshold_names), function(i) failed_binary_surfaces)
      
      names(failed_binary_surfaces) <- 
        failed_threshold_names
      
      binary_surfaces <-
        c(binary_surfaces, failed_binary_surfaces)
      
      binary_surfaces <- binary_surfaces[match(index., names(binary_surfaces))]
     # Correct if are less than 4 classes when we cross baseline mask and calculated mask
       
       for(i in c(1:length(binary_surfaces))[index. %in% succesfull_threshold_names]){
        if(ncol(binary_surfaces[[i]])!=4){
          cols <- c("surface.00", "surface.10", "surface.01", "surface.11")
          missing_colnames <- cols[is.element(cols, colnames(binary_surfaces[[i]]))!=T]
          missing_cols <-
            matrix(0, ncell(list_threshold_results[[1]][[1]]),
                   ncol = length(missing_colnames))
          colnames(missing_cols) <- missing_colnames 
          binary_surfaces[[i]] <- 
            cbind(binary_surfaces[[i]],missing_cols)
          binary_surfaces[[i]] <- 
            binary_surfaces[[i]][,match(cols, colnames(binary_surfaces[[i]]))]
        }else{
          binary_surfaces[[i]] <- binary_surfaces[[i]]
        }
      }
      
      list_df_results <-
        lapply(
          # grep(paste(succesfull_threshold_names, collapse = "|"), index.),
          1:length(binary_surfaces),
          function(i)
            cbind(
              coor,
              getValues(list_raster_results[[i]]),
              as.integer(values(list_threshold_results[[1]][[i]])),
              getValues(calibration_results[[2]][[4]]),
              binary_surfaces[[i]][,1],
              binary_surfaces[[i]][,2],
              binary_surfaces[[i]][,3],
              binary_surfaces[[i]][,4]
              )
             )
    # transform in data frame
    list_df_results <-
      lapply(c(1:length(list_raster_results)), function(i)
        as.data.frame(list_df_results[[i]]))
    
    # Set colnames
    colnames <- c("x", "y", "index_value", "predict.surface.class", "baseline.surface.class", 
                  "True.Negative","False.Positive", "False.Negative",
                  "True.Positive")
    
    list_df_results <- lapply(list_df_results, setNames, colnames)
    rm(colnames, surface_class, binary_surfaces)
    }else{ 
    coor <- coordinates(calibration_results[[1]])
    # Set df list with cell coordinates(x,y) indexvalues(z)  and 
    # mask threshold values (surface)
    list_df_results <-
      lapply(c(1:length(list_raster_results)),
             function(i)
               cbind(
                 coor,
                 getValues(list_raster_results[[i]]),
                 getValues(list_threshold_results[[1]][[i]])
                 )
             )
    # transform in data frame
    list_df_results <-
      lapply(c(1:length(list_raster_results)), function(i)
        as.data.frame(list_df_results[[i]]))
    # Set colnames
    colnames <- c("x", "y", "index_value", "surface_threshold")
    list_df_results <- lapply(list_df_results, setNames, colnames)
    names(list_df_results) <- names(list_raster_results)
    rm(colnames)
    }
  
  if(pdf == FALSE){
    rm(list_raster_results)
    list.results<- list(list_df_results)
    names(list.results) <- c("data.frames")
  }else{
    # List raster results an df results
    list.results <- list(list_df_results, list_raster_results)
    names(list.results) <- c("data.frames", "rasters")}
  # Return
  
  # return(list.results)
  
  
  # rm(calibration_results)
  ############################################################################  
  # Descriptors calculation -------------------------------------------------
  ############################################################################
  if(descrip==F){
    if(manual.mask.test==F){
      if(length(index.)>1){
      int_surf_cover <-
        unname(do.call(c,
                lapply(c(1:length(index.)),
                       function(i)
                         unname(
                           c(
                             table(list.results[[1]][[i]][,4])[2], table(list.results[[1]][[i]][,4])[1]
                           )
                         )
                )
        ))
      }else{
        int_surf_cover <-
          unlist(
                  lapply(c(1:length(index.)),
                         function(i)
                           unname(
                             c(
                               table(list.results[[1]][[i]][,4])[2], table(list.results[[1]][[i]][,4])[1]
                             )
                           )
                  )
          )    
      }
    
  }else{
    if(length(index.)>1){
    int_surf_cover <-
     unname(do.call(c,
              lapply(c(1:length(index.)),
                     function(i)
                       unname(
                         c(
                           table(list.results[[1]][[i]][,4])[2], table(list.results[[1]][[i]][,4])[1],
                           # table(list.results[[1]][[i]][,5])[1], table(list.results[[1]][[i]][,5])[2],
                           length(c(list.results[[1]][[i]][,5])[c(list.results[[1]][[i]][,5])==0]),
                           length(c(list.results[[1]][[i]][,5])[c(list.results[[1]][[i]][,5])==1]),
                           # table(list.results[[1]][[i]][,4])[2], table(list.results[[1]][[i]][,4])[1],
                           # table(list.results[[1]][[i]][,5])[2], table(list.results[[1]][[i]][,5])[1],
                           table(list.results[[1]][[i]][,6])[2],
                           # table(list.results[[1]][[i]][,6])[1],
                           table(list.results[[1]][[i]][,7])[2], 
                           # table(list.results[[1]][[i]][,7])[1],
                           table(list.results[[1]][[i]][,8])[2],
                           # table(list.results[[1]][[i]][,8])[1],
                           table(list.results[[1]][[i]][,9])[2]
                           # table(list.results[[1]][[i]][,9])[1]
                         )
                       )
              )
      ))
    }else{
      int_surf_cover <-
        unlist(
                lapply(c(1:length(index.)),
                       function(i)
                         unname(
                           c(
                             table(list.results[[1]][[i]][,4])[2], table(list.results[[1]][[i]][,4])[1],
                             # table(list.results[[1]][[i]][,5])[2], table(list.results[[1]][[i]][,5])[1],
                             length(c(list.results[[1]][[i]][,5])[c(list.results[[1]][[i]][,5])==0]),
                             length(c(list.results[[1]][[i]][,5])[c(list.results[[1]][[i]][,5])==1]),
                             # table(list.results[[1]][[i]][,4])[2], table(list.results[[1]][[i]][,4])[1],
                             # table(list.results[[1]][[i]][,5])[2], table(list.results[[1]][[i]][,5])[1],
                             table(list.results[[1]][[i]][,6])[2],
                             # table(list.results[[1]][[i]][,6])[1],
                             table(list.results[[1]][[i]][,7])[2], 
                             # table(list.results[[1]][[i]][,7])[1],
                             table(list.results[[1]][[i]][,8])[2],
                             # table(list.results[[1]][[i]][,8])[1],
                             table(list.results[[1]][[i]][,9])[2]
                             # table(list.results[[1]][[i]][,9])[1]
                           )
                         )
                )
        )
    }
   
      }
    
  }else{#descrip==T
    # source("./ccspectral/Descriptor.calculation.fun.R")
    if(manual.mask.test==F){
      if(length(index.)>1){
        int_surf_cover <-
          unname(do.call(c,
                  lapply(c(1:length(index.)),
                         function(i)
                           do.call(c,
                                   lapply( 0:1 , function(j)
                                     descriptor.fun(
                                       list.results[[1]][[i]][,3][list.results[[1]][[i]][,4] == j],
                                       descriptors.)
                                   )
                           )
                  )
          ))
      }else{
        int_surf_cover <-
         unlist(
                  lapply(c(1:length(index.)),
                         function(i)
                           do.call(c,
                                   lapply( 0:1 , function(j)
                                     descriptor.fun(
                                       list.results[[1]][[i]][,3][list.results[[1]][[i]][,4] == j],
                                       descriptors.)
                                   )
                           )
                  )
          )
      }
      
      }else{#manual.mask.test==T
        
        int_surf_cover <-
                  lapply(c(1:length(index.)),
                         function(i)
                           unlist(
                                   lapply(4:5 , function(j)
                                     unlist(
                                             lapply(0:1, function(k)
                                               descriptor.fun(
                                                 list.results[[1]][[i]][,3][list.results[[1]][[i]][,j] == k],
                                                 descriptors.)
                                               )
                                             )
                                     )
                                   )
                         )
        test_mask_surfaces <-   
          lapply(c(1:length(index.)),
                 function(i)
                   unlist(
                           lapply(6:9 , function(j)
                             descriptor.fun(
                               list.results[[1]][[i]][,3][list.results[[1]][[i]][,j] == 1],
                               descriptors.)
                             )
                           )
                 )
        int_surf_cover <-
          unlist(
                  lapply(c(1:length(index.)),
                         function(i)
                           c(int_surf_cover[[i]], test_mask_surfaces[[i]]
                             )
                         )
                  )
        rm(test_mask_surfaces)      
                         
      }
    }
   
int_surf_cover[is.na(int_surf_cover)] <- 0
  # START dataframe for index index vaulues presentation --------------------
  
  dat <- read.csv(summary.file)
  
  # names(descriptor_value) <- colnames(dat)[-c(1:7)]
  # 
  if(calculate.thresh==T){
    if(length(index.)>1){
      theresholds.results <-  unname(do.call(c,list_threshold_results[[2]]))
    }else{
      theresholds.results <-  unlist(list_threshold_results[[2]])
    }
    
    new_dat <-
      as.data.frame(
        as.list(
          c(
            sample_name,
            vis_photo,
            nir_photo,
            int_surf_cover,
            theresholds.results,
            threshold.method
            )
          )
        )
  }else{
    new_dat <-
      as.data.frame(
        as.list(
          c(
            sample_name,
            vis_photo,
            nir_photo,
            int_surf_cover,
            threshold.vector,
            "Predefined"
          )
        )
      )
    }

  colnames(new_dat) <- colnames(dat)
  dat_bind <- rbind(dat, new_dat)
  write.csv(dat_bind, summary.file, row.names = F)
  

  # Create pdf to plot results ---------------------------------------------
  if(pdf == T){
    # Set plotpdf function to plot results (operated by lists) ---------------------------------
    pdf_name <- paste0(out_dir, "/", sample_name, ".pdf")
    
    plotpdf <-  function(lhist, lind, lman, lover, i.names, asp, pdf.name){
      # set pdf structure -------------------------------------------------------
     
      pdf(file = pdf.name, w = 14, h = 3.571429 * length(index.))
      par(mfrow = c(length(index.), 4))
      
      # set function for pdf graphic content ------------------------------------
      # hist:raster dataframe with x y coordinates index value (z) and binary mask value (surface)
      # ind: index raster
      
      pdfprint <-   function(hist, ind, man, over, name, asp){
        # set surface binary image as factor ------------------------------------
      
        surface.f <- factor(hist[,4], levels= c(1,0),
                            labels = c("no_moss","moss"))
        # surface.overlap <- factor(hist[,5], levels= c(1,2,3),
        #                     labels = c("substrate","overlap","moss"))
        #
        # PLOT densities ----------------------------------------------------------
        if(require(sm)!=T){
          install.packages("sm")
        }
        sm::sm.density.compare(hist[,3], surface.f, xlab= name)
        
        title(main = paste(names), "values by surface")
        # add legend
        colfill <- c(2:(2+length(levels(surface.f))))
        legend("topright", levels(surface.f), fill=colfill)
        
        # PLOT index values and real moss contour --------------------------------
        plot(ind,
             # main =  paste(toupper(names)),"values",
             axes = FALSE, box = FALSE,
             asp  = asp)
        plot(moss_poly, add=T, border="red")
        
        # PLOT index values from real moss area and real moss contour  ------------
        plot(man,
             main =  paste(toupper(name)),"moss values over whole scene",
             axes = FALSE, box = FALSE,
             asp  = asp)
        plot(moss_poly, add=T, border="red")
        
        # PLOT overlap index values between real moss area and background  ------------
        plot(over,
             main =  paste(toupper(names)),"index overlap regions",
             axes = FALSE, box = FALSE,
             asp  = asp)
        plot(moss_poly, add=T, border="red")
        
      }
      # run pdf.print over our list of indexes ----------------------------------
      lapply(c(1:length(lind)), function(i)
        pdfprint(hist  = list_df_results[[i]][,3],
                 ind   = lind[[i]],
                 man   = lman[[i]],
                 over  = lover[[i]],
                 names = i.names[[i]],
                 asp   = asp))
      # close pdf ---------------------------------------------------------------
      dev.off()
    }
    
    # run plotpdf ------------------------------------------------------------------------------
    plotpdf(lhist   = lhist,
            lind    = index.,
            lman    =  moss_manual_int_list,
            lover   = overlap_index_list,
            i.names = index_names,
            asp     = asp,
            pdf.name = paste0(sample_name, ".pdf"))
  }
  

  loop_time <-
    strsplit(as.character((as.numeric(Sys.time())- as.numeric(start.time))/60),"\\.")[[1]]
  loop_time[2] <-  
    round(60*as.numeric(paste0("0.",as.character(loop_time[2]))))
  
  message(paste0(sample_name, " processed."))
  message("Made ", done_samples+1, " of ", total.samples, " total samples", 
                 " in ", loop_time[1], " mins ", loop_time[2], " secs.")
}     
