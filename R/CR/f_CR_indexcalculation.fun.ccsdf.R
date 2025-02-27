#' Calculate Various Remote Sensing Indices
#'
#' This function computes various spectral indices from raster data.
#' @param raster.mat A matrix containing raster data.
#' @param raster.band A raster stack containing the spectral bands.
#' @param index. A vector of indices to be calculated. 
#' It have to be one of this values "NDVI", "SR", "MSAVI", "EVI", "CI", "BI", "NORR", "NORG", "NORB", 
# "EXR", "EXG", "EXB", "EXGR", "CIVE", "VEG", "HUE", "SAT", "VAL"
#' @return A list of raster layers corresponding to the requested indices.

#' @keywords internal  # Mark the function as internal

index.calc.fun <- function(raster.mat, # calibration_results[[1]]
                           raster.band, # calibration_results[[2]]
                           index. # index. <- index_order
                           ){
    ## 1. SET INTERNAL FUNCTIONS -------------------------------
    # 1.1 Function rgb.spectral.normalization
    if(length(grep(paste(c("NORR", "NORG", "NORB", "EXR", "EXG", "EXB", 
                           "EXGR", "CIVE","VEG"), collapse = "|"), unique(index.))) > 0){
      # charge rgb.normalisation function
      rgb.spectral.normalization <- function(raster.mat){
        
        # Normalize red, green, and blue bands by their max values
        red_norm_coord   <- terra::values(raster.mat[[1]]) / max(terra::values(raster.mat[[1]]))
        green_norm_coord <- terra::values(raster.mat[[2]]) / max(terra::values(raster.mat[[2]]))
        blue_norm_coord  <- terra::values(raster.mat[[3]]) / max(terra::values(raster.mat[[3]]))
        
        # Create a new raster object
        rgb_norm <- terra::rast(raster.mat)
        sum_rgb_nc <- red_norm_coord + green_norm_coord + blue_norm_coord
        
        # Normalize the RGB channels
        red_norm <- terra::setValues(rgb_norm, (red_norm_coord / sum_rgb_nc))
        green_norm <- terra::setValues(rgb_norm, (green_norm_coord / sum_rgb_nc))
        blue_norm <- terra::setValues(rgb_norm, (blue_norm_coord / sum_rgb_nc))
        rgb_norm <- terra::rast(red_norm, green_norm, blue_norm)
       
         return(rgb_norm)
      }
      # Apply the normalization function
      rgb_norm <- rgb.spectral.normalization(raster.mat) # calculate rgb_norm raster
      rst <- terra::rast(rgb_norm[[3]]) # set reference raster
    }
      
      # 1.2 Convert RGB to HSV if needed
    if(length(grep(paste(c("HUE", "SAT", "VAL"), collapse = "|"), unique(index.))) > 0){
            hsv_brick <- raster.band
            hsv <- t(rgb2hsv(values(raster.band[[1]]), 
                             values(raster.band[[2]]), 
                             values(raster.band[[3]]), 
                             maxColorValue = 1))
            
            # Convert to HSV raster
            hsv_brick <- terra::rast(lapply(c(1:3), function(i)
                            rast(matrix(hsv[,i],
                            nrow = nrow(raster.band[[1]]),
                            ncol = ncol(raster.band[[1]]),
                            byrow = T))))
            names(hsv_brick) <- c("h", "s", "v")
    }
    ## 2. INDEX CALCULATION  -------------------------------
    # Compute indices if included in index. vector
    # else (an index is not included) index raster is set as NULL
    
    # NDVI 
    if(any(unique(grepl("NDVI", index.))) == TRUE){
      NDVI <- (raster.mat[[4]] - raster.mat[[1]]) / (raster.mat[[4]] + raster.mat[[1]])
      }else{
          NDVI <- NULL
    }
    # Simple Ratio (SR) 
    if(any(unique(grepl("SR", index.))) == TRUE){
      SR <- raster.mat[[4]] / raster.mat[[1]]
    }else{
      SR <- NULL
    }
    # Second Modified Soil Adjusted VEGetation Index (MSAVI2) 
    if(any(unique(grepl("MSAVI", index.))) == TRUE){
      MSAVI <- (2 * raster.mat[[4]] + 1 - sqrt((2 * raster.mat[[4]] + 1) ^ 2 - 8 * (raster.mat[[4]] - raster.mat[[1]]))) / 2
    }else{
      MSAVI <- NULL
    }
    # Enhanced VEGetation Index (EVI) 
    if(any(unique(grepl("EVI", index.))) == TRUE){
      EVI <- 2.5 * (raster.mat[[4]] - raster.mat[[1]]) / (raster.mat[[4]] + 6 *
                                                          raster.mat[[1]] - 7.5 *
                                                          raster.mat[[3]] + 1)
    }else{
      EVI <- NULL
    }
    # Crust Index (CI)
    if(any(unique(grepl("^CI$", index.))) == TRUE){
      CI <- 1 - ((raster.mat[[1]] - raster.mat[[3]]) / (raster.mat[[1]] + raster.mat[[3]]))
    }else{
      CI <- NULL
    }
    # Biological Soil Crust Index (BSCI)
    if(any(unique(grepl("^BSCI$", index.))) == TRUE){
      BSCI <- (1 - 2 * abs(raster.mat[[1]] - raster.mat[[2]])) /
                        # raster::mean(stack(raster.mat[[2]], raster.mat[[1]], raster.mat[[4]]))
                       terra::mean(c(raster.mat[[2]], raster.mat[[1]], raster.mat[[4]]))
    }else{
      BSCI <- NULL
    }
    # Brightness Index (BI)
    if(any(unique(grepl("BI", index.))) == TRUE){
      BI <- sqrt(raster.mat[[2]] ^ 2 + raster.mat[[1]] ^ 2 + raster.mat[[4]] ^ 2)
    }else{
      BI <- NULL
    }
    # Normalized red (NorR)
    if(any(unique(grepl("NORR", index.))) == TRUE){
      NORR <- rgb_norm[[1]]
    }else{
      NORR <- NULL
    }
    # Normalized green (NorG)
    if(any(unique(grepl("NORG", index.))) == TRUE){
      NORG <- rgb_norm[[2]]
    }else{
      NORG <- NULL
    }
    # Normalized blue (NorB)
    if(any(unique(grepl("NORB", index.))) == TRUE){
      NORB <- rgb_norm[[3]]
    }else{
      NORB <- NULL
    }
    # Excess red (ExR)
    if(any(unique(grepl("EXR", index.))) == TRUE){
      EXR <- terra::setValues(rst, 1.4 * terra::values(rgb_norm[[1]]) - terra::values(rgb_norm[[2]]))
      }else{
      EXR <- NULL
      }
    # Excess green (ExG)
    if(any(unique(grepl("^EXG$", index.))) == TRUE){
      EXG <- terra::setValues(rst,2 * terra::values(rgb_norm[[2]]) -
                               terra::values(rgb_norm[[1]]) -
                               terra::values(rgb_norm[[3]]))
      }else{
      EXG <- NULL
    }
    # Excess blue (ExB)
    if(any(unique(grepl("EXB", index.))) == TRUE){
      EXB <- terra::setValues(rst, 1.4 * terra::values(rgb_norm[[3]]) - terra::values(rgb_norm[[2]]))
    }else{
      EXB <- NULL
    }
    # Excess green minus excess red (EXGR)
    if(any(unique(grepl("^EXGR$", index.))) == TRUE){
      if(is.null(EXG) == TRUE){
        EXG <- terra::setValues(rst, 2 * terra::values(rgb_norm[[2]]) -
                                  terra::values(rgb_norm[[1]]) -
                                  terra::values(rgb_norm[[3]]))
      }
      if(is.null(EXR) == TRUE){
        EXR <- terra::setValues(rst, 1.4 * terra::values(rgb_norm[[1]]) - terra::values(rgb_norm[[2]]))
      }
      EXGR <- EXG - EXR
    }else{
      EXGR <- NULL
    }
    # Color index of VEGetation (CIVE)
    if(any(unique(grepl("^CIVE$", index.))) == TRUE){
      CIVE <- terra::setValues(rst, 0.441 * terra::values(rgb_norm[[1]]) -
                             0.811 * terra::values(rgb_norm[[2]]) +
                             0.385 * terra::values(rgb_norm[[3]]) +
                             18.78745)
    }else{
      CIVE <- NULL
    }
    # VEGetation index (VEG)
    if(any(unique(grepl("VEG", index.))) == TRUE){
      VEG <- terra::setValues(rst, terra::values(rgb_norm[[2]]) /
                              ((terra::values(rgb_norm[[1]]) ^ 0.667) *
                               (terra::values(rgb_norm[[3]]) ^ (1 - 0.667))))
    }else{
      VEG <- NULL
    }
    # HUE (HUE)
    if(any(unique(grepl("HUE", index.))) == TRUE){
      HUE <- hsv_brick[[1]]
    }else{
      HUE <- NULL
    }
    # SATuration (SAT)
    if(any(unique(grepl("SAT", index.))) == TRUE){
      SAT <- hsv_brick[[2]]
    }else{
      SAT <- NULL
    }
    # Value (VAL)
    if(any(unique(grepl("VAL", index.))) == TRUE){
      VAL <- hsv_brick[[3]]
    }else{
      VAL <- NULL
    }
    
    ## 3. RESULTS: list, name and return results -------------------------------
    # List all rasters, and NULL vectors respectively.
    # Return raster results
    list_raster_results <- list(NDVI, SR, MSAVI, EVI, CI, BSCI ,BI, NORR, NORG, 
                                NORB, EXR, EXG, EXB, EXGR, CIVE, VEG, HUE, SAT, VAL)
    # Define the index order
    index_order <- c("NDVI", "SR", "MSAVI", "EVI", "CI", "BSCI", "BI", "NORR", 
                     "NORG", "NORB", "EXR", "EXG", "EXB", "EXGR", "CIVE", "VEG", 
                     "HUE", "SAT", "VAL")
    # Assign names to the result list
    names(list_raster_results) <- index_order
    # Remove NULL values from the list
    list_raster_results <- plyr::compact(list_raster_results)
    # Return the list of calculated indices
    return(list_raster_results)
  }
