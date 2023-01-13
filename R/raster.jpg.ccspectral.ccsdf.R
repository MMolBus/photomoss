# Read JPG Images and set aspect ratio
# vis.photo <- vis_photo
# nir.photo <- nir_photo
# mask.photo <- mask_photo
# img.photo <-  img_photo

raster.jpg.ccspectral <- function(vis.photo, nir.photo, manual.mask.test, mask.photo){
  vis_jpg  <-
    jpeg::readJPEG(paste("./vis/", vis.photo, sep = ""))
  vis_red   <- raster(vis_jpg[, , 1])
  vis_green <- raster(vis_jpg[, , 2])
  vis_blue  <- raster(vis_jpg[, , 3])

  nir_jpg  <-
    jpeg::readJPEG(paste("./nir/", nir.photo, sep = ""))
  nir_blue  <- raster(nir_jpg[, , 3]) + 10 / 256
  if(manual.mask.test==T){
    mask_tiff  <-
      suppressWarnings(tiff::readTIFF(paste("./mask/", mask.photo, sep = "")))
    # transform 0 to 1 from binary tiff obtained from ImageJ
    binar_mask   <- raster(mask_tiff)==0
  }

  asp <- nrow(vis_red) / ncol(vis_red)
  if(manual.mask.test==T){
    all_bands <-
      stack(vis_red, vis_green, vis_blue, nir_blue, binar_mask)
    names(all_bands) <-
      c("vis.red", "vis.green", "vis.blue", "nir.blue", "binar.mask")
  }else{
    all_bands <-
      stack(vis_red, vis_green, vis_blue, nir_blue)
    names(all_bands) <-
      c("vis.red", "vis.green", "vis.blue", "nir.blue")
  }
  return(all_bands)
}
