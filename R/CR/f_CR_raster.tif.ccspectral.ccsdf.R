# Read TIF Images and set aspect ratio
raster.tiff.ccspectral <- function(vis.photo, nir.photo, manual.mask.test, mask.photo){
  vis_tiff <- tiff::readTIFF(paste("./vis/", vis.photo, sep = ""))
  vis_red <- terra::rast(vis_tiff[, , 1])
  vis_green <- terra::rast(vis_tiff[, , 2])
  vis_blue <- terra::rast(vis_tiff[, , 3])
  nir_tiff <-  tiff::readTIFF(paste("./nir/", nir.photo, sep = ""))
  nir_blue <- terra::rast(nir_tiff[, , 3]) + 10 / 256
  
  if(manual.mask.test == T){
    mask_tiff <- suppressWarnings(tiff::readTIFF(paste("./mask/", mask.photo, sep = "")))
    # transform 0 to 1 from binary tif obtained from image J
    binar_mask <- terra::rast(mask_tiff) == 0
  }
  asp <- nrow(vis_red) / ncol(vis_red)
  if(manual.mask.test == T){
    # all_bands <- stack(vis_red, vis_green, vis_blue, nir_blue, binar_mask)
    all_bands <- c(vis_red, vis_green, vis_blue, nir_blue, binar_mask)
    names(all_bands) <- c("vis.red", "vis.green", "vis.blue", "nir.blue", "binar.mask")
  }else{
    # all_bands <- stack(vis_red, vis_green, vis_blue, nir_blue)
    all_bands <- c(vis_red, vis_green, vis_blue, nir_blue)
    names(all_bands) <-c("vis.red", "vis.green", "vis.blue", "nir.blue")
  }
  return(all_bands)
}
