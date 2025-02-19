#' Cell Extraction and Color Calibration Function
#'
#' This function extracts cell values from a raster dataset and performs color calibration
#' using a nonlinear regression model based on a standard or custom color checker.
#'
#' @param obs.area A SpatVector representing the observed area.
#' @param all.bands A SpatRaster containing spectral bands.
#' @param chart A list of spatial polygons defining the color chart patches.
#' @param manual.mask.test A logical indicating whether to use a manual mask (TRUE or FALSE).
#' @param chart.vals A data frame containing reference color chart values.
#'
#'
#' @return A list containing:
#'   \item{raster_mat}{SpatRaster of the corrected spectral bands.}
#'   \item{raster_band}{SpatRaster of the original spectral bands.}
#'   \item{red_rsq}{Adjusted R-squared for red band calibration.}
#'   \item{green_rsq}{Adjusted R-squared for green band calibration.}
#'   \item{blue_rsq}{Adjusted R-squared for blue band calibration.}
#'   \item{nir_rsq}{Adjusted R-squared for near-infrared band calibration.}
#'   \item{moss_poly}{(Optional) A polygon representing the manually masked area, if applicable.}
#'
#' @details The function performs the following steps:
#'   \enumerate{
#'     \item Extracts spectral band values from the observed area.
#'     \item Applies manual masking if enabled.
#'     \item Crops spectral bands to match the observed area extent.
#'     \item Extracts color chart values and samples data for model training.
#'     \item Fits nonlinear models to calibrate the spectral bands.
#'     \item Predicts corrected values and generates a calibrated raster.
#'   }
#'
#' @importFrom terra ext extract crop rast as.polygons
#' @importFrom stats nls predict var
#'
#' @keywords internal

cell.extract.color.cal.fun <- 
      function(obs.area, all.bands, chart, manual.mask.test, chart.vals){
  
            # Get the extent of the observed area
  obs_ext <- terra::ext(obs.area) # raster::extent
  # Create an empty raster matrix to store predictions
  temp_mat <- terra::rast(matrix(data = NA,
                                    nrow = nrow(all.bands),
                                    ncol = ncol(all.bands),
                                    byrow = T))
  # Extract spectral band values from the observed area
  bands_df <- data.frame(terra::extract(all.bands, obs.area))
  
  if(manual.mask.test == T){
        colnames(bands_df) <- c("vis.red", "vis.green", "vis.blue", "nir.blue", "mask")
  }else{colnames(bands_df) <- c("vis.red", "vis.green", "vis.blue", "nir.blue")
  }
  
  # Crop individual spectral bands to match the observed area extent
  red_band <- terra::crop(all.bands[[1]], terra::ext(obs_ext))
  terra::ext(red_band) <- terra::ext(c(0, 1, 0, 1))
 
  green_band <- terra::crop(all.bands[[2]], terra::ext(obs_ext))
  terra::ext(green_band) <- terra::ext(c(0, 1, 0, 1))
  
  blue_band <- terra::crop(all.bands[[3]], terra::ext(obs_ext))
  terra::ext(blue_band) <- terra::ext(c(0, 1, 0, 1))
  
  # If manual mask is used, process the mask band and generate polygons
  if(manual.mask.test == T){
    mask_band <- terra::crop(all.bands[[5]], terra::ext(obs_ext))
    terra::ext(mask_band) <- terra::ext(c(0, 1, 0, 1))
    # moss_poly <- rasterToPolygons(mask_band, fun = function(x){x == 1}, dissolve = T)
    moss_poly <- terra::as.polygons(mask_band, fun = function(x){x == 1}, dissolve = T)
    # raster_band <- brick(red_band, green_band, blue_band, mask_band)
    raster_band <- terra::rast(red_band, green_band, blue_band, mask_band)
  }else{
      # raster_band <- brick(red_band, green_band, blue_band)
      raster_band <- terra::rast(red_band, green_band, blue_band)
  }
  
  # Initialize an empty dataframe for training data
  train_df <- data.frame()
  
  # Standard color checker reference values
  chart_values_list <- data.frame(
                          red.chart =
                                c(0.17, 0.63, 0.15, 0.11, 0.31, 0.20,
                                  0.63, 0.12, 0.57, 0.21, 0.33, 0.67,
                                  0.04, 0.10, 0.60, 0.79, 0.70, 0.07,
                                  0.93, 0.59, 0.36, 0.18, 0.08, 0.03),
                          green.chart =
                                c(0.10, 0.32, 0.19, 0.14, 0.22, 0.47,
                                  0.27, 0.11, 0.13, 0.06, 0.48, 0.40,
                                  0.06, 0.27, 0.07, 0.62, 0.13, 0.22,
                                  0.95, 0.62, 0.38, 0.20, 0.09, 0.03),
                          blue.chart = 
                                c(0.07, 0.24, 0.34, 0.06, 0.42, 0.42,
                                  0.06, 0.36, 0.12, 0.14, 0.10, 0.06,
                                  0.24, 0.09, 0.04, 0.08, 0.31, 0.38,
                                  0.93, 0.62, 0.39, 0.20, 0.09, 0.02),
                          nir.chart = 
                                c(0.43, 0.87, 0.86, 0.18, 0.86, 0.43,
                                  0.85, 0.54, 0.54, 0.79, 0.49, 0.66,
                                  0.52, 0.44, 0.72, 0.82, 0.88, 0.42,
                                  0.91, 0.51, 0.27, 0.13, 0.06, 0.02))
  # Determine if using standard color checker or a custom one
  if(identical(chart.vals, chart_values_list)){
          chart_vals <- chart.vals
          print("You are using Xrite classic ColorCheker")
          for (i in c(1:24)){
                poly <- chart[i]
                options(warn = -1)
                # df_samp <- data.frame(chart_vals[i, ], raster::extract(all.bands, poly))
                df_samp <- data.frame(chart_vals[i, ], terra::extract(all.bands, poly))
                options(warn = 0)
                if(nrow(df_samp) >= 50){
                        df_samp <- df_samp[sample(x = 1:nrow(df_samp), size = 50, replace = F), ]
                        }
                train_df <- rbind(train_df, df_samp)
                }
         }else{
              chart_vals <- chart.vals
              print("You are NOT using Xrite classic ColorCheker")
              for(i in seq_along(chart)){
                      poly <- chart[i]
                      options(warn = -1)
                      # df_samp <- data.frame(chart_vals[i, ], raster::extract(all.bands, poly))
                      df_samp <- data.frame(chart_vals[i, ], terra::extract(all.bands, poly))
                      options(warn = 0)
                      if(nrow(df_samp) >= 50){df_samp <- df_samp[sample(x = 1:nrow(df_samp), size = 50, replace = F), ]
                      }
                      train_df <- rbind(train_df, df_samp)
                      }
         }
   
  # Fit nonlinear models for color correction
  red_nls <- nls(red.chart ~ (a * exp(b * vis.red)), trace = F, 
                 data = train_df, start = c(a = 0.1, b = 0.1))
  green_nls <- nls(green.chart ~ (a * exp(b * vis.green)), trace = F, 
                   data = train_df, start = c(a = 0.1, b = 0.1))
  blue_nls <- nls(blue.chart ~ (a * exp(b * vis.blue)), trace = F, 
                  data = train_df, start = c(a = 0.1, b = 0.1))
  nir_nls <- nls(nir.chart ~ (a * exp(b * nir.blue)), trace = F, 
                 data = train_df, start = c(a = 0.1, b = 0.1))
  
  # Predict corrected values for each band
  red_preds <- predict(red_nls, bands_df)
  green_preds <- predict(green_nls, bands_df)
  blue_preds <- predict(blue_nls, bands_df)
  nir_preds <- predict(nir_nls, bands_df)
  
  # Calculate adjusted R-squared (coefficient of determination
  red_rsq <- 1 - sum((train_df$red.chart - predict(red_nls, 
                                                   train_df))^2)/(length(train_df$red.chart) * var(train_df$red.chart))
  green_rsq <- 1 - sum((train_df$green.chart - predict(green_nls, 
                                                       train_df))^2)/(length(train_df$green.chart) * var(train_df$green.chart))
  blue_rsq <- 1 - sum((train_df$blue.chart - predict(blue_nls, 
                                                     train_df))^2)/(length(train_df$blue.chart) * var(train_df$blue.chart))
  nir_rsq <- 1 - sum((train_df$nir.chart - predict(nir_nls, 
                                                   train_df))^2)/(length(train_df$nir.chart) * var(train_df$nir.chart))
  
  
  # Create corrected raster matrices
  red_mat <- temp_mat
  green_mat <- temp_mat
  blue_mat <- temp_mat
  nir_mat <- temp_mat
  
  red_mat <- terra::crop(red_mat, terra::ext(obs_ext))
  green_mat <- terra::crop(green_mat, terra::ext(obs_ext))
  blue_mat <- terra::crop(blue_mat, terra::ext(obs_ext))
  nir_mat <- terra::crop(nir_mat, terra::ext(obs_ext))
  
  values(red_mat) <- red_preds
  values(green_mat) <- green_preds
  values(blue_mat) <- blue_preds
  values(nir_mat) <- nir_preds
  
  terra::ext(red_mat) <- terra::ext(c(0, 1, 0, 1))
  terra::ext(green_mat) <- terra::ext(c(0, 1, 0, 1))
  terra::ext(blue_mat) <- terra::ext(c(0, 1, 0, 1))
  terra::ext(nir_mat) <- terra::ext(c(0, 1, 0, 1))
  
  # Combine corrected bands into a single raster object
  raster_mat <- terra::rast(red_mat, green_mat, blue_mat, nir_mat)
  
  # Prepare output
  if(manual.mask.test == T){
    out <- list(raster_mat, raster_band, red_rsq, green_rsq, blue_rsq, nir_rsq, moss_poly)
    names(out) <- c("raster_mat", "raster_band", "red_rsq", "green_rsq", "blue_rsq", "nir_rsq", "moss_poly")
  }else{
    out <- list(raster_mat, raster_band, red_rsq, green_rsq, blue_rsq, nir_rsq)
    names(out) <- c("raster_mat", "raster_band", "red_rsq", "green_rsq", "blue_rsq", "nir_rsq")
  }
  return(out)
}

