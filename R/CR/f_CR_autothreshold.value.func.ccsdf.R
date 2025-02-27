#' Perform automatic thresholding on raster values.
#' 
#' This function applies automatic thresholding to a raster object by first
#' binning its values into 256 levels based on the specified range, 
#' then applying the selected thresholding method to identify an appropriate 
#' threshold value. It is used for identifying a threshold value in
#' raster-based index data.
#' 
#' @param raster A `Raster` object to which thresholding will be applied.
#' @param max.index A numeric value specifying the maximum index for the 
#' thresholding.
#' @param min.index A numeric value specifying the minimum index for the 
#' thresholding.
#' @param method A string specifying the thresholding method to apply. 
#' See the documentation of the `autothresholdr::auto_thresh_apply_mask` 
#' function for supported methods.
#' 
#' @return A numeric value representing the threshold identified for the raster 
#' data.
#' @keywords internal

autothreshold.value <- 
      function(raster, max.index, min.index, method){
  # Extract the name of the raster (index) and select the first layer of the raster
  index <- names(raster)
  raster <- raster[[1]]
  
  # Define a helper function for binning raster values into 256 levels
  col.0.255.bin.range <- 
        function(raster, max.index, min.index){
  # Calculate the range (difference) between max and min index values
  a <- max.index - min.index
  
  # Bin the raster values based on the calculated range, divided into 256 bins
  bin <- .bincode(x = terra::values(raster),
                  breaks = seq(min.index, max.index, (a/256)),
                  include.lowest = T)
  
  # Return the binned values  
  return(bin)
        }
  
  # Call the binning function to create color code index based on raster values
  col_code_index <- col.0.255.bin.range(raster, max.index, min.index)
  
  # Extract the actual raster values
  index_values <- terra::values(raster)
  
  # Combine the color code index and index values into a data frame
  df_colcode_indexvalues <- cbind.data.frame(
                                      "colcode" = col_code_index,
                                      "indexvalues" = index_values)
  
  # Set the raster values to the color code index
  terra::values(raster) <- df_colcode_indexvalues$colcode
  
  # Apply the auto-thresholding method from the `autothresholdr` package, 
  # with options to ignore white, black, and NA values
  atm <- autothresholdr::auto_thresh_apply_mask(
                terra::values(raster),
                method,
                ignore_white = T,
                ignore_black = T,
                ignore_na = TRUE
   )
  
  # Retrieve the threshold value from the auto-threshold result
  pt  <- attr(atm, "thresh")[1]
  
  # Sort the unique color code values and select the threshold point
  p   <- sort(unique(df_colcode_indexvalues$colcode), decreasing = F)[pt]
  
  # Find the corresponding original index value for the chosen color code
  out <- min((df_colcode_indexvalues[df_colcode_indexvalues$colcode == p, ])$indexvalues)
  
  # Print the threshold value along with the index name and method used
  print(paste(index, " ", method," threshold value = ", out))
  
  # Return the threshold value
  return(out)
}
