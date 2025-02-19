#' Process Spectral Raster Images in TIFF format (Internal)
#'
#' This function reads visible and near-infrared (NIR) spectrum images in  TIFF 
#' format, applies an optional binary mask, and returns a combined raster object 
#' with spectral bands. This function is intended for internal use only.
#'
#' @param vis.photo A character string specifying the filename of the visible 
#' spectrum (RGB) image.
#' @param nir.photo A character string specifying the filename of the NIR image.
#' @param manual.mask.test A logical value. If TRUE, the function applies a 
#' binary mask (from a TIFF file).
#' @param mask.photo A character string specifying the filename of the binary 
#' mask (if manual.mask.test is TRUE).
#' 
#' @return A raster object containing the combined bands: visible red, green, 
#' blue, NIR blue, and optionally a binary mask.
#' 
#' @examples
#' # Example 1: Without mask
#' result <- .internal_raster_tiff_ccspectral("visible.tiff", "nir.tiff", FALSE, NULL)
#'
#' # Example 2: With mask
#' result <- .internal_raster_tiff_ccspectral("visible.tiff", "nir.tiff", TRUE, "mask.tiff")
#'
#' @keywords internal  # Mark the function as internal

raster.tiff.ccspectral <-
      function(vis.photo,
               nir.photo,
               manual.mask.test,
               mask.photo) {
            # Read the visible spectrum image (TIFF) into a variable
            vis_tiff <-
                  tiff::readTIFF(paste("./vis/", vis.photo, sep = ""))
            
            # Extract the individual red, green, and blue bands for the visible spectrum image
            vis_red <-
                  terra::rast(vis_tiff[, , 1])  # Red band
            vis_green <-
                  terra::rast(vis_tiff[, , 2])  # Green band
            vis_blue <-
                  terra::rast(vis_tiff[, , 3])  # Blue band
            
            # Read the near-infrared (NIR) image (TIFF) into a variable
            nir_tiff <-
                  tiff::readTIFF(paste("./nir/", nir.photo, sep = ""))
            
            # Extract the blue band from the NIR image and add a small value for correction
            nir_blue <-
                  terra::rast(nir_tiff[, , 3]) + 10 / 256
            
            # If manual mask testing is enabled
            if (manual.mask.test == T) {
                  # Read the mask image (TIFF) into a variable
                  mask_tiff <-
                        suppressWarnings(tiff::readTIFF(paste("./mask/", mask.photo, sep = "")))
                  
                  # Convert the mask image from binary (0 and 1 values) to a raster where 0 becomes 1
                  binar_mask <-
                        terra::rast(mask_tiff) == 0
            }
            
            # Calculate the aspect ratio (rows / columns) of the red band
            asp <-
                  nrow(vis_red) / ncol(vis_red)
            
            # Stack the bands together, including the mask if manual masking is enabled
            if (manual.mask.test == T) {
                  # Combine all bands
                  all_bands <-
                        c(vis_red, vis_green, vis_blue, nir_blue, binar_mask)  # Current way of combining the bands
                  
                  # Name the individual layers (bands) of the combined object
                  names(all_bands) <-
                        c("vis.red",
                          "vis.green",
                          "vis.blue",
                          "nir.blue",
                          "binar.mask")
            } else{
                  # If no mask is used, only combine the visible spectrum and NIR blue bands
                  all_bands <-
                        c(vis_red, vis_green, vis_blue, nir_blue)  # Current way of combining the bands
                  
                  # Name the individual layers (bands) of the combined object
                  names(all_bands) <- c("vis.red", "vis.green", "vis.blue", "nir.blue")
            }
            
            # Return the final raster stack
            return(all_bands)
      }