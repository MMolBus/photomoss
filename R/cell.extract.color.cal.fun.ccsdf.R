# cell extraction and color calibration function
# obs.area <- obs_area
# all.bands <- all_bands
# chart
# manual.mask.test
# pdf

cell.extract.color.cal.fun <- 
  function(obs.area, all.bands, chart, manual.mask.test, chart.vals, pdf){
        
        obs_ext <- 
              extent(obs.area)
        
        temp_mat <-
              raster(matrix(data = NA, 
                            nrow = nrow(all.bands), 
                            ncol = ncol(all.bands), 
                            byrow = T))
        
        bands_df <-
              data.frame(extract(all.bands, obs.area))
        
        if(manual.mask.test==T){
              colnames(bands_df) <-
                    c("vis.red", 
                      "vis.green",
                      "vis.blue", 
                      "nir.blue",
                      "mask")
              }else{
                    colnames(bands_df) <-
                          c("vis.red", 
                            "vis.green",
                            "vis.blue", 
                            "nir.blue")
                    }
  
  red_band <- 
    crop(all.bands[[1]], extent(obs_ext))
  extent(red_band) <- 
    extent(c(0, 1, 0, 1))
 
  green_band <- 
        crop(all.bands[[2]], extent(obs_ext))
  extent(green_band) <- 
        extent(c(0, 1, 0, 1))
  
  blue_band <- 
        crop(all.bands[[3]], extent(obs_ext))
  extent(blue_band) <- 
        extent(c(0, 1, 0, 1))
  
  if (manual.mask.test == T) {
    mask_band <- crop(all.bands[[5]], extent(obs_ext))
    extent(mask_band) <- extent(c(0, 1, 0, 1))
    moss_poly <- rasterToPolygons(mask_band, fun = function(x) {
      x == 1
    }, dissolve = T)
    raster_band <- brick(red_band, green_band, blue_band, 
                         mask_band)
  }else{
    raster_band <- brick(red_band, green_band, blue_band)
  }
  train_df <- data.frame()
  
  if(identical(
        chart.vals,
        data.frame(
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
             0.91, 0.51, 0.27, 0.13, 0.06, 0.02))) 
     ){
        chart_vals <- chart.vals
        print("You are using Xrite classic ColorCheker")
  # chart_vals <- data.frame(red.chart = 
  #                                c(0.17, 0.63, 0.15, 0.11, 0.31, 0.20,
  #                                  0.63, 0.12, 0.57, 0.21, 0.33, 0.67,
  #                                  0.04, 0.10, 0.60, 0.79, 0.70, 0.07,
  #                                  0.93, 0.59, 0.36, 0.18, 0.08, 0.03), 
  #                          green.chart = 
  #                                c(0.10, 0.32, 0.19, 0.14, 0.22, 0.47, 
  #                                  0.27, 0.11, 0.13, 0.06, 0.48, 0.40, 
  #                                  0.06, 0.27, 0.07, 0.62, 0.13, 0.22, 
  #                                  0.95, 0.62, 0.38, 0.20, 0.09, 0.03), 
  #                          blue.chart = 
  #                                c(0.07, 0.24, 0.34, 0.06, 0.42, 0.42,
  #                                  0.06, 0.36, 0.12, 0.14, 0.10, 0.06, 
  #                                  0.24, 0.09, 0.04, 0.08, 0.31, 0.38, 
  #                                  0.93, 0.62, 0.39, 0.20, 0.09, 0.02), 
  #                          nir.chart = 
  #                                c(0.43, 0.87, 0.86, 0.18, 0.86, 0.43, 
  #                                  0.85, 0.54, 0.54, 0.79, 0.49, 0.66, 
  #                                  0.52, 0.44, 0.72, 0.82, 0.88, 0.42, 
  #                                  0.91, 0.51, 0.27, 0.13, 0.06, 0.02))
  for (i in c(1:24)[-3]) {
    poly <- chart[i]
    options(warn = -1)
    df_samp <- data.frame(chart_vals[i, ], extract(all.bands, 
                                                   poly))
    options(warn = 0)
    if (nrow(df_samp) >= 50) {
      df_samp <- df_samp[sample(x = 1:nrow(df_samp), size = 50, 
                                replace = F), ]
      }
    train_df <- rbind(train_df, df_samp)
    }
  }else{
        chart_vals <- chart.vals
        print("You are NOT using Xrite classic ColorCheker")
          for (i in seq_along(chart)) {
    poly <- chart[i]
    options(warn = -1)
    df_samp <- data.frame(chart_vals[i, ], extract(all.bands, 
                                                   poly))
    options(warn = 0)
    if (nrow(df_samp) >= 50) {
      df_samp <- df_samp[sample(x = 1:nrow(df_samp), size = 50, 
                                replace = F), ]
      }
    train_df <- rbind(train_df, df_samp)
    }
  }
  
  red_nls <- nls(red.chart ~ (a * exp(b * vis.red)), trace = F, 
                 data = train_df, start = c(a = 0.1, b = 0.1))
  red_preds <- predict(red_nls, bands_df)
  red_rsq <- 1 - sum((train_df$red.chart - predict(red_nls, 
                                                   train_df))^2)/(length(train_df$red.chart) * var(train_df$red.chart))
  red_mat <- temp_mat
  red_mat <- crop(red_mat, extent(obs_ext))
  extent(red_mat) <- extent(c(0, 1, 0, 1))
  values(red_mat) <- red_preds
  green_nls <- nls(green.chart ~ (a * exp(b * vis.green)), 
                   trace = F, data = train_df, start = c(a = 0.1, b = 0.1))
  green_preds <- predict(green_nls, bands_df)
  green_rsq <- 1 - sum((train_df$green.chart - predict(green_nls, 
                                                       train_df))^2)/(length(train_df$green.chart) * var(train_df$green.chart))
  green_mat <- temp_mat
  green_mat <- crop(green_mat, extent(obs_ext))
  extent(green_mat) <- extent(c(0, 1, 0, 1))
  values(green_mat) <- green_preds
  blue_nls <- nls(blue.chart ~ (a * exp(b * vis.blue)), trace = F, 
                  data = train_df, start = c(a = 0.1, b = 0.1))
  blue_preds <- predict(blue_nls, bands_df)
  blue_rsq <- 1 - sum((train_df$blue.chart - predict(blue_nls, 
                                                     train_df))^2)/(length(train_df$blue.chart) * var(train_df$blue.chart))
  blue_mat <- temp_mat
  blue_mat <-crop(blue_mat, extent(obs_ext))
  extent(blue_mat) <- extent(c(0, 1, 0, 1))
  values(blue_mat) <- blue_preds
  nir_nls <- nls(nir.chart ~ (a * exp(b * nir.blue)), trace = F, 
                 data = train_df, start = c(a = 0.1, b = 0.1))
  nir_preds <- predict(nir_nls, bands_df)
  nir_rsq <- 1 - sum((train_df$nir.chart - predict(nir_nls, 
                                                   train_df))^2)/(length(train_df$nir.chart) * var(train_df$nir.chart))
  nir_mat <- temp_mat
  nir_mat <- crop(nir_mat, extent(obs_ext))
  extent(nir_mat) <- extent(c(0, 1, 0, 1))
  values(nir_mat) <- nir_preds
  raster_mat <- brick(red_mat, green_mat, blue_mat, nir_mat)
  if (manual.mask.test == T) {
    out <- list(raster_mat, raster_band, red_rsq, green_rsq, 
                blue_rsq, nir_rsq, moss_poly)
    names(out) <- c("raster_mat", "raster_band", 
                    "red_rsq", "green_rsq", "blue_rsq", 
                    "nir_rsq", "moss_poly")
  }else {
    out <- list(raster_mat, raster_band, red_rsq, green_rsq, 
                blue_rsq, nir_rsq)
    names(out) <- c("raster_mat", "raster_band", 
                    "red_rsq", "green_rsq", "blue_rsq", 
                    "nir_rsq")
  }
  return(out)}

