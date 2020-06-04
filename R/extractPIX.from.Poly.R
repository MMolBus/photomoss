# TITLE: extractPIX.from.Poly
# Function: extract pixel positions enclosed by a polygon.
extractPIX.from.Poly<- function(poly){
  # function(poly){
  # file <- Sys.glob(paths = paste0(tif.path, "vis/*.tif"))[1]
  # file <-list.files(path = "./vis",full.names = T)
  # file <- tif.path
  # vis.tiff <- tiff::readTIFF(file)
  vis.tiff <- tiff::readTIFF(list.files("./vis", full.names = T)[1])
  vis.red <- raster::raster(vis.tiff[, , 1])
  
  message("Extracting cells. Please wait.")
  cells <- data.frame(raster::extract(vis.red, poly, cellnumbers = T))[,1]
  out <- data.frame(cells, raster::rowColFromCell(vis.red, cells), 
                    raster::xyFromCell(vis.red, cells))
  return(out)
}

