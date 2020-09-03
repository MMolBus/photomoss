# Index calculations -------------------------------------------------------------------------
# set index calculation function ----------------------------------------------
# index calculation function returns a list of rasters for required index
# ARGUMENTS:
# r: raster stack with RGB+NIR channels
# index: character vector with the required index 
#  possible index
# "NDVI", "SR", "MSAVI", "EVI", "CI", "BI", "NORR", "NORG", "NORB", 
# "EXR", "EXG", "EXB", "EXGR", "CIVE", "VEG", "HUE", "SAT", "VAL"
# index_order <- c("NDVI", "SR", "MSAVI", "EVI", "CI", "BI", "NORR", "NORG", "NORB",
# "EXR", "EXG", "EXB", "EXGR", "CIVE", "VEG", "HUE", "SAT", "VAL")
# index. <- index_order 
# 
# raster.mat  = calibration_results[[1]]
# raster.band = calibration_results[[2]]
# index. = index.
# calculate.thresh = calculate.thresh
# threshold.method
# pdf = pdf


index.calc.fun <- 
  function(raster.mat, 
           raster.band, 
           index.
           # calculate.thresh, 
           # threshold.vector,
           # threshold.method, 
           # manual.mask.test, 
           # 
  ){
    # SET INTERNAL FUNCTIONS-------------------------------------------------------
    ##############################################################################.
    # Functionrgb.spectral.normalization -----------------------------------------
    if (length(grep(paste(
      c("NORR", "NORG", "NORB", "EXR", "EXG", "EXB", "EXGR", "CIVE","VEG"),
      collapse = "|"),
      unique(index.))) > 0) {
      # charge set.rgb.normaliSATion function -----------------------------------
      rgb.spectral.normalization <- function(raster.mat) {
        red_norm_coord   <- raster::getValues(raster.mat[[1]]) /
          max(raster::getValues(raster.mat[[1]]))
        green_norm_coord <- raster::getValues(raster.mat[[2]]) /
          max(raster::getValues(raster.mat[[2]]))
        blue_norm_coord  <- raster::getValues(raster.mat[[3]]) /
          max(raster::getValues(raster.mat[[3]]))
        rgb_norm   <- raster::raster(raster.mat)
        sum_rgb_nc <-
          red_norm_coord + green_norm_coord + blue_norm_coord
        red_norm   <-  raster::setValues (rgb_norm, (red_norm_coord /
                                                       sum_rgb_nc))
        green_norm <- raster::setValues(rgb_norm, (green_norm_coord /
                                                     sum_rgb_nc))
        blue_norm  <- raster::setValues(rgb_norm,  (blue_norm_coord /
                                                      sum_rgb_nc))
        rgb_norm   <- raster::brick(red_norm, green_norm, blue_norm)
        return(rgb_norm)
      }
      
      # calculate rgb_norm raster -----------------------------------------------
      rgb_norm <- rgb.spectral.normalization(raster.mat)
      
      # set reference raster ----------------------------------------------------
      rst <- raster(rgb_norm[[3]])
      
    }
    # HSV image transformation there is no need of color calibration -------------
    if (length(grep(paste(c(
      "HUE", "SAT", "VAL"
    ), collapse = "|"),
    unique(index.))) > 0) {
      hsv_brick <- raster.band
      hsv <- t(rgb2hsv(values(raster.band[[1]]), 
                       values(raster.band[[2]]), 
                       values(raster.band[[3]]), 
                       maxColorValue = 1))
      hsv_brick <- brick(lapply(c(1:3), function(i) 
        raster(matrix(hsv[,i], 
                      nrow = nrow(raster.band[[1]]), 
                      ncol = ncol(raster.band[[1]]),  
                      byrow = T))))
      names(hsv_brick) <- c("h", "s", "v")
      
    }
    #==============================================================================.
    # INDEX CALCULATION ------------------------------------------------------------
    # if there are included in the function argument "index." vector,
    # a raster index is calculated
    # else (an index is not included) index raster is set as NULL
    ##############################################################################.
    # NDVI -----------------------------------------------------------------------
    if (any(unique(grepl("NDVI", index.))) == TRUE) {
      NDVI <- (raster.mat[[4]] - raster.mat[[1]]) /
        (raster.mat[[4]] + raster.mat[[1]])
      
    } else{
      NDVI <- NULL
    }
    
    # Simple Ratio (SR) ----------------------------------------------------------
    if (any(unique(grepl("SR", index.))) == TRUE) {
      SR <- raster.mat[[4]] / raster.mat[[1]]
    } else{
      SR <- NULL
    }
    
    # Second Modified Soil Adjusted VEGetation Index (MSAVI2) --------------------
    if (any(unique(grepl("MSAVI", index.))) == TRUE) {
      MSAVI <-
        (2 * raster.mat[[4]] + 1 - sqrt((2 * raster.mat[[4]] + 1) ^ 2 -
                                          8 * (raster.mat[[4]] - raster.mat[[1]]))) / 2
    } else{
      MSAVI <- NULL
    }
    
    # Enhanced VEGetation Index (EVI) --------------------------------------------
    if (any(unique(grepl("EVI", index.))) == TRUE) {
      EVI <-
        2.5 * (raster.mat[[4]] - raster.mat[[1]]) / (raster.mat[[4]] + 6 *
                                                       raster.mat[[1]] - 7.5 *
                                                       raster.mat[[3]] + 1)
    } else{
      EVI <- NULL
    }
    
    # Crust Index (CI) -----------------------------------------------------------
    if (any(unique(grepl("^CI$", index.))) == TRUE) {
      CI <- 1 - ((raster.mat[[1]] - raster.mat[[3]]) /
                   (raster.mat[[1]] + raster.mat[[3]]))
    } else{
      CI <- NULL
    }
    
    # Biological Soil Crust Index (BSCI) -----------------------------------------
    if (any(unique(grepl("^BSCI$", index.))) == TRUE) {
      BSCI <- (1 - 2 * abs(raster.mat[[1]] - raster.mat[[2]])) /
        raster::mean(stack(raster.mat[[2]], raster.mat[[1]], raster.mat[[4]]))
    } else{
      BSCI <- NULL
    }
    
    # Brightness Index (BI) ------------------------------------------------------
    if (any(unique(grepl("BI", index.))) == TRUE) {
      BI <-
        sqrt(raster.mat[[2]] ^ 2 + raster.mat[[1]] ^ 2 + raster.mat[[4]] ^ 2)
    } else{
      BI <- NULL
    }
    
    # Normalized red (NorR) ------------------------------------------------------
    if (any(unique(grepl("NORR", index.))) == TRUE) {
      NORR <- rgb_norm[[1]]
    } else{
      NORR <- NULL
    }
    
    # Normalized green (NorG) ----------------------------------------------------
    if (any(unique(grepl("NORG", index.))) == TRUE) {
      NORG <- rgb_norm[[2]]
    } else{
      NORG <- NULL
    }
    
    # Normalized blue (NorB) -----------------------------------------------------
    if (any(unique(grepl("NORB", index.))) == TRUE) {
      NORB <- rgb_norm[[3]]
    } else{
      NORB <- NULL
    }
    
    # Excess red (ExR) -----------------------------------------------------------
    if (any(unique(grepl("EXR", index.))) == TRUE) {
      EXR   <- setValues(rst,
                         1.4 * getValues(rgb_norm[[1]]) -
                           getValues(rgb_norm[[2]]))
    } else{
      EXR <- NULL
    }
    
    # Excess green (ExG) ---------------------------------------------------------
    if (any(unique(grepl("^EXG$", index.))) == TRUE) {
      EXG <- setValues(rst,
                       2 * getValues(rgb_norm[[2]]) -
                         getValues(rgb_norm[[1]]) -
                         getValues(rgb_norm[[3]]))
    } else{
      EXG <- NULL
    }
    
    # Excess blue (ExB) ----------------------------------------------------------
    if (any(unique(grepl("EXB", index.))) == TRUE) {
      EXB <- setValues(rst,
                       1.4 * getValues(rgb_norm[[3]]) -
                         getValues(rgb_norm[[2]]))
    } else{
      EXB <- NULL
    }
    
    # Excess green minus excess red (EXGR) ---------------------------------------
    if (any(unique(grepl("^EXGR$", index.))) == TRUE) {
      if (is.null(EXG) == TRUE) {
        EXG <- setValues(rst,
                         2 * getValues(rgb_norm[[2]]) -
                           getValues(rgb_norm[[1]]) -
                           getValues(rgb_norm[[3]]))
      }
      if (is.null(EXR) == TRUE) {
        EXR <- setValues(rst,
                         1.4 * getValues(rgb_norm[[1]]) -
                           getValues(rgb_norm[[2]]))
      }
      EXGR <- EXG - EXR
    } else{
      EXGR <- NULL
    }
    
    # Color index of VEGetation (CIVE) -------------------------------------------
    if (any(unique(grepl("^CIVE$", index.))) == TRUE) {
      CIVE   <- setValues(
        rst,
        0.441 * getValues(rgb_norm[[1]]) -
          0.811 * getValues(rgb_norm[[2]]) +
          0.385 * getValues(rgb_norm[[3]]) +
          18.78745
      )
    } else{
      CIVE <- NULL
    }
    
    # VEGetation index (VEG) -----------------------------------------------------
    if (any(unique(grepl("VEG", index.))) == TRUE) {
      VEG <- setValues(rst,
                       getValues(rgb_norm[[2]]) /
                         ((getValues(rgb_norm[[1]]) ^ 0.667) *
                            (getValues(rgb_norm[[3]]) ^ (1 - 0.667))))
    } else{
      VEG <- NULL
    }
    
    # HUE (HUE) ------------------------------------------------------------------
    if (any(unique(grepl("HUE", index.))) == TRUE) {
      HUE <- hsv_brick[[1]]
    } else{
      HUE <- NULL
    }
    
    # SATuration  (SAT) ----------------------------------------------------------
    if (any(unique(grepl("SAT", index.))) == TRUE) {
      SAT <- hsv_brick[[2]]
    } else{
      SAT <- NULL
    }
    
    # Value (VAL) ----------------------------------------------------------------
    if (any(unique(grepl("VAL", index.))) == TRUE) {
      VAL <- hsv_brick[[3]]
    } else{
      VAL <- NULL
    }
    
    
    #=============================================================================.
    # RESULTS: list, name and return results --------------------------------------
    # List all rasters, and NULL vectors respectively.
    # Name it
    # Filter elements thath have NULL value.
    # Return raster results.
    ##############################################################################.
    # list results
    list_raster_results <- list(NDVI, SR, MSAVI, EVI, CI, BSCI ,BI, NORR, NORG, 
                                NORB, EXR, EXG, EXB, EXGR, CIVE, VEG, HUE,SAT,VAL)
    
    # set index order
    index_order <- c("NDVI", "SR", "MSAVI", "EVI", "CI", "BSCI", "BI", "NORR", 
                     "NORG", "NORB", "EXR", "EXG", "EXB", "EXGR","CIVE", "VEG", 
                     "HUE", "SAT" , "VAL")
    
    # Name list raster results
    names(list_raster_results) <- index_order
    # remove NULL index from the list
    list_raster_results <- plyr::compact(list_raster_results)
    return(list_raster_results)
  }
