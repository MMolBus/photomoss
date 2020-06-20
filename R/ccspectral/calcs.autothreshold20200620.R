# # # CALCS function, for ccspectral
# vis.files = vis_files
# nir.files = nir_files
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
                  thereshold.vector,
                  descrip, threshold.method, pdf) {
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
  # set sample name
  # if (done_samples > 0) { 
  #   sample_name <- sample_names[done_samples + 1]
  # }else{
  #   sample_name <- sample_names[1]}
  
  sample_name <- sample_names[photo]
  
  # Check all single elements have been correctly set ===========================

  print(vis_photo)
  print(nir_photo)
  print(paste0(names(obs.areas)[area], ": ", sample_name))
  if(manual.mask.test==T){
  print(mask_photo)
  }
  # Cell extraction and color calibration -----------------------------------------------------
  # Read and create raster from tiff =====================================
  source("./ccspectral/raster.tif.ccspectral.R")
  
  if(manual.mask.test==T){
    all_bands <-  raster.tiff.ccspectral(vis.photo = vis_photo, nir.photo = nir_photo, 
                                         manual.mask.test = manual.mask.test, 
                                         mask.photo = mask_photo)
  }else{
    all_bands <-  raster.tiff.ccspectral(vis.photo = vis_photo, nir.photo = nir_photo, 
                                         manual.mask.test = manual.mask.test)
  }
  
  
  
   
  # ######IF ML
    source("./ccspectral/cell.extract.color.cal.fun.R")
  
  calibration_results <-
    cell.extract.color.cal.fun(
      obs.area = obs_area,
      all.bands = all_bands,
      chart = chart,
      manual.mask.test = manual.mask.test,
      pdf = pdf
    )
  
 if(descrip==T){
  red_rsq <- calibration_results[3]
  green_rsq <- calibration_results[4]
  blue_rsq <- calibration_results[5]
  nir_rsq <- calibration_results[6]
  if(manual.mask.test==T){
    real_cover_moss <- sum(getValues(calibration_results[[2]][[4]]))
  }
  }else{
    if(manual.mask.test==T){
      real_cover_moss <- sum(getValues(calibration_results[[2]][[4]]))
      }
    }
  if(pdf==TRUE){
    moss_poly <- calibration_results[7]
    }
  ###########################################################################  
  # Calculate index values, as raster and as dataframe ----------------------
  ############################################################################  

    source("./ccspectral/20190628(2)_indexcalculation.fun.R")
  
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
  
  if(calculate.thresh == TRUE) {
    source("./ccspectral/calculate.thresh.fun.R")
    
    source("./ccspectral/autothreshold.value.func.R")
    
    list_threshold_results <-
      calculate.thresh.fun(list.raster.results = list_raster_results,
                           threshold.method = threshold.method)
  }
  
  # Extract mask values -----------------------------------------------------
  #extract mask pixel coordinates
  if(manual.mask.test==T){
    coor <- coordinates(raster.band[[4]])
    # Set df list with cell coordinates(x,y) indexvalues(z)  and mask surface values (surface)
    if(calculate.thresh == TRUE){
      list_df_results <-
        lapply(c(1:length(list_raster_results)), 
               function(i)
                 cbind(
                   coor,
                   getValues(list_raster_results[[i]]),
                   getValues(list_threshold_results[[1]][[i]]),
                   getValues(raster.band[[4]]),
                 )
        )
      
      # transform in data frame
      list_df_results <-
        lapply(c(1:length(list_raster_results)), function(i)
          as.data.frame(list_df_results[[i]]))
      # Set colnames
      colnames <- c("x", "y", "z", "surface_threshold", "surface_manual")
      list_df_results <- lapply(list_df_results, setNames, colnames)
    }else{
      list_df_results <-
        lapply(c(1:length(list_raster_results)), 
               function(i)
                 cbind(
                   coor,
                   extract(list_raster_results[[i]],
                           1:ncell(list_raster_results[[i]])),
                   extract(raster.band[[4]],
                           1:ncell(raster.band[[4]]))
                 )
        )
      # transform in data frame
      list_df_results <-
        lapply(c(1:length(list_raster_results)), function(i)
          as.data.frame(list_df_results[[i]]))
      # Set colnames
      colnames <- c("x", "y", "z", "surface_manual")
      list_df_results <- lapply(list_df_results, setNames, colnames)}
    
  }else{ 
    coor <- coordinates(raster.band[[1]])
    # Set df list with cell coordinates(x,y) indexvalues(z)  and mask surface values (surface)
    if(calculate.thresh == TRUE){
      list_df_results <-
        lapply(c(1:length(list_raster_results)), 
               function(i)
                 cbind(
                   coor,
                   getValues(list_raster_results[[i]]),
                   getValues(list_threshold_results[[1]][[i]]
                   )
                 )
        )
      
      # transform in data frame
      list_df_results <-
        lapply(c(1:length(list_raster_results)), function(i)
          as.data.frame(list_df_results[[i]]))
      # Set colnames
      colnames <- c("x", "y", "z", "surface_threshold")
      list_df_results <- lapply(list_df_results, setNames, colnames)
    }
    
    names(list_df_results) <- names(list_raster_results)}
  
  if(pdf == FALSE){
    rm(list_raster_results)
    list.results<- list(list_df_results)
    names(list.results) <- c("data.frames")
  }else{
    # List raster results an df results
    list.results <- list(list_df_results, list_raster_results)
    names(list.results) <- c("data.frames", "rasters")}
  # Return
  
  return(list.results)
  
  
  rm(calibration_results)
  ############################################################################  
  # Descriptors calculation -------------------------------------------------
  ############################################################################
if(manual.mask.test==F){
  
}else{
  #if manual.mask.tes==T we need to create new surface categories 
  # resulting on the surface crossing between on the treshold clasification 
  # and the  manua mask.
  if(descrip == F){
    source("./ccspectral/cell.count.sf.class.fun.R")
    int_surf_cover <-
      lapply(c(1:length(index.)),
             function(i)
               cell.count.sf.class(index_results_list[[1]][[i]][, 4],
                                   index_results_list[[1]][[i]][, 5]))
    int_surf_cover <- 
      do.call(c, int_surf_cover)
    }else{
      if(calculate.thresh == TRUE){
        
        source("./ccspectral/20190628(2)_Descriptor.calculation.fun.R")
        
        surface_manual <- 
          c("1|0", "1", "0")
        descriptor_value_manual <- 
          lapply(c(1:length(surface_manual)), 
                 function(j)
                   lapply(c(1:length(index.)), 
                          function(i)
                            descriptor.fun(
                              index_results_list[[1]][[i]][, 3]
                              [grep(surface_manual[j],
                                    index_results_list[[1]][[i]][, 4])],
                              des = descriptors.)))
        descriptor_value_manual <-
          lapply(c(1:length(surface_manual)),
                 function(j)
                   do.call(c, descriptor_value_manual[[j]]))
        
        descriptor_value_manual <-
          do.call(c, descriptor_value_manual)
        
        surface_thresh <- c("1", "0")
        
        descriptor_value_thresh <-
          lapply(c(1:length(surface_thresh)),
                 function(j)
                   lapply(c(1:length(index.)),
                          function(i)
                            descriptor.fun(
                              index_results_list[[1]][[i]][, 3]
                              [grep(surface_thresh[j],
                                    index_results_list[[1]][[i]][, 5])],
                              des = descriptors.)))
        
        descriptor_value_thresh <- 
          lapply(c(1:length(surface_thresh)), function(j)
            do.call(c, descriptor_value_thresh[[j]]))
        descriptor_value_thresh <- 
          do.call(c, descriptor_value_thresh)
        descriptor_value <- 
          c(descriptor_value_manual, descriptor_value_thresh)
        
        rm(descriptor_value_manual, descriptor_value_thresh)
    
        source("./ccspectral/cell.count.sf.class.fun.R")
        
        int_surf_cover <- 
          lapply(c(1:length(index.)),
                 function(i)
                   cell.count.sf.class(index_results_list[[1]][[i]][, 4],
                                       index_results_list[[1]][[i]][, 5]))
        int_surf_cover <- 
          do.call(c, int_surf_cover)
    
  }else{
    surface_manual <- c("1|0", "1", "0")  
    
    descriptor_value_manual <- 
      lapply(c(1:length(surface_manual)), function(j)
        lapply(c(1:length(index.)), function(i)
          descriptor.fun(
            index_results_list[[1]][[i]][, 3]
            [grep(surface_manual[j], index_results_list[[1]][[i]][, 4])],
                       des = descriptors.)))
    
    descriptor_value_manual <- lapply(c(1:length(surface_manual)), function(j)
      do.call(c, descriptor_value_manual[[j]]))
    
    descriptor_value <- do.call(c, descriptor_value_manual)
  }
}
  }
    
  
  # START dataframe for index index vaulues presentation --------------------
  
  dat <- read.csv(summary.file)
  
  # names(descriptor_value) <- colnames(dat)[-c(1:7)]
  # 
  if(descrip==F){new_dat <- 
    as.data.frame(
      c(
        list(
          sample_name,
          vis_photo,
          nir_photo,
          real_cover_moss
        ),
        as.list(int_surf_cover),
        threshold.method
      )
    )
  }else{
    if(calculate.thresh == F){
    new_dat <- 
      as.data.frame(
        c(
          list(
            sample_name,
            vis_photo,
            nir_photo,
            red_rsq,
            green_rsq,
            blue_rsq,
            nir_rsq, 
            real_cover_moss
          ),
          as.list(descriptor_value)
        )
      )
  }else{
    new_dat <-
      as.data.frame(
        c(
          list(
            sample_name,
            vis_photo,
            nir_photo,
            red_rsq,
            green_rsq,
            blue_rsq,
            nir_rsq,
            real_cover_moss
          ),
          as.list(descriptor_value),
          as.list(int_surf_cover),
          threshold.method
        )
      )
  }
    }
    
  
  
  colnames(new_dat) <- colnames(dat)
  dat_bind <- rbind(dat, new_dat)
  write.csv(dat_bind, summary.file, row.names = F)
  
  # Create pdf to plot results ---------------------------------------------
  if(pdf == T){
    # Set plotpdf function to plot results (operated by lists) ---------------------------------
    pdf_name<- paste0(out_dir, "/", sample_name, ".pdf")
    
    plotpdf<-  function(lhist, lind, lman, lover, i.names, asp, pdf.name){
      # set pdf structure -------------------------------------------------------
      pdf(file = pdf.name, w = 14, h = 3.571429 * length(lind))
      par(mfrow = c(length(index.), 4))
      
      # set function for pdf graphic content ------------------------------------
      # hist:raster dataframe with x y coordinates index value (z) and binary mask value (surface)
      # ind: index raster
      pdfprint <-   function(hist, ind, man, over, names, asp)
      {
        # set surface binary image as factor ------------------------------------
        surface.f <- factor(hist[,4], levels= c(1,0),
                            labels = c("no_moss","moss"))
        # surface.overlap <- factor(hist[,5], levels= c(1,2,3),
        #                     labels = c("substrate","overlap","moss"))
        #
        # PLOT densities ----------------------------------------------------------
        sm.density.compare(hist[,3], surface.f, xlab= names)
        title(main = paste(names), "values by surface")
        # add legend
        colfill <- c(2:(2+length(levels(surface.f))))
        legend("topright", levels(surface.f), fill=colfill)
        
        # PLOT index values and real moss contour --------------------------------
        plot(ind,
             main =  paste(toupper(names)),"values",
             axes = FALSE, box = FALSE,
             asp  = asp)
        plot(moss_poly, add=T, border="red")
        
        # PLOT index values from real moss area and real moss contour  ------------
        plot(man,
             main =  paste(toupper(names)),"moss values over whole scene",
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
        pdfprint(hist  = lhist[[i]],
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
            lind    = index,
            lman    =  moss_manual_int_list,
            lover   = overlap_index_list,
            i.names = index_names,
            asp     = asp,
            pdf.name = paste0(sample_name, ".pdf"))
  }
  
  
  message(paste0(sample_name, " processed... (",
                 100* round((done_samples+1)/total.samples, 2), " %)"))
  # print = paste(sample_name, "processed")
}     
