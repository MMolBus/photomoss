change.labels.order <- function(input.path, input.file) {
  
  if(any(list.files(getwd())%in%"nir") & any(list.files(getwd())%in%"vis")){}else{
    wd <- getwd()
    setwd(input.path)
    on.exit(setwd(wd))
  }
  
  
  all.names <- read.csv(input.file)
  
  pots <- nrow(all.names)
  
  if((pots/14)%%1==0){pots.per.block <- 14}else{
    if((pots/10)%%1==0){pots.per.block <- 10}else{message("Names do not correspond either to blocks of 14 (monosp communities), neither blocks of 10 (mixed communities)")}
  }
  
  blocks <- pots/pots.per.block
  
  
  if(pots.per.block==14){
    block.order <- c(1, NA, 8, NA, 2, 3, 9, 10, 4, 5, 11, 12, 6, 7, 13, 14)
  }else{block.order <- c(1, NA, 6, NA, 2, 3, 7, 8, 4, 5, 9, 10)}
  
  ordered.pots <- unlist(lapply(seq(0, pots.per.block*(blocks-1), by=pots.per.block), "+", block.order))
  
  names <- all.names[ordered.pots,, drop=F]
  
  names$names <- as.character(names$names)
  names[is.na(names$names), "names"] <- "mossless"
  print(names)
  
  write.table(names, "names.csv", row.names=F, quote=F)
}
##################################################################################################################################################
chart.from.tif <- function(tif.path, samp.width = 0.01){
  # file <- Sys.glob(path = paste0(tif.path, "vis/*.tif"))[1]
  file <- list.files(path = "./vis",pattern=".tif$",full.names = T)[1]
  vis.tiff <- tiff::readTIFF(file)
  vis.red <- raster::raster(vis.tiff[, , 1])
  vis.green <- raster::raster(vis.tiff[, , 2])
  vis.blue <- raster::raster(vis.tiff[, , 3])
  rgb <- raster::stack(vis.red, vis.green, vis.blue)
  options(warn = -1)
  
  op <- par(mfrow=c(1,1), mar=c(0,0,0,0), oma=c(0,0,0,0))
  on.exit(par(op))
  # X11()
  raster::plotRGB(rgb, scale = 1, asp = nrow(vis.red)/ncol(vis.red))
  options(warn = 0)
  
  chart.coords <- data.frame(x = numeric(), y = numeric())
  message("Click on all 24 color chart cells in sequence. The sequence follows left to right and starts at cell 1 (brown, top left) and finishes on cell 24 (black, bottom right).")
  for (i in 1:24) {
    options(warn = -1)
    chart.coords[i, 1:2] <- click(xy = T)[1:2]
    options(warn = 0)
  }
  sp.chart <- sp::SpatialPoints(chart.coords)
  chart.buff <- rgeos::gBuffer(sp.chart, width = samp.width, byid = T)
  plot(chart.buff, add = T, col = "green")
  
  return(chart.buff)
}

####################################################################################################################
####################################################################################################################

roi2polygon <- function(roi.path, tif.path){
  # Para importar los archivos punto .roi instalamos el paquete "RImageJROI"
  # library(RImageJROI)
  
  # Importamos el área de interes que creamos con Image J
  # usamos como EJEMPLO la número 5 (la numeración de las celdillas las sabemos gracias  a la referencia
  # y orden conocido de los alveolos en la fotografía, algo que fuimos
  # apuntando según creabamos las celdillas en el Image J)
  # roi.path <- roi.paths[[1]]
  x_roi5 <- RImageJROI::read.ijroi(roi.path, verbose = FALSE)
  
  # una vez importada el área de interés a nuestro Global Environment vamos a necesitar hacer una serie de cambios de formato
  # hasta llegar a tener objetos SpatialPolygons legibles por crusCover
  
  # para ello, instalamos el paquete "spatstat"
  # library(spatstat)
  
  # primero lo transformamos a formato owin (ijroi ->  owin)
  x_owin5 <- RImageJROI::ij2spatstat(x_roi5)
  
  # Ahora hacemos dos operaciones que son necesarias para que las ventanas coincidan sobre la imagen que proyecta crustCover:
  # 1) es necesario invertir las coordenadas del eje Y de la ventana, pues imageJ proyecta el eje Y
  # invertido (y=0 -> borde superior de la imagen) con respecto a cómo lo proyecta crustCover (y=0 -> borde inferior de la imagen)
  # 2) es necesario reescalar las coordenadas de la ventana tal manera que
  # correspondan al rango de X e Y que oscila entre 0 y 1 en la imagen proyectada por crustCover
  
  # Establecemos la imagen de referencia para hacer los siguietes cálculos
  # tif.path <- "tif/"
  # first.tif.filename <- Sys.glob(paste0(tif.path, "vis/*.tif"))[[1]]
  first.tif.filename <- list.files(path = "./vis",full.names = T)[1]
  # library(raster)
  # RGB_stack_DEM <- raster::stack(first.tif.filename)
  bandred <- raster(first.tif.filename, band=1)
  
  # # En el vector de coordenadas y de la ventana hcemos la operación 1 y 2
  # w5_y_corr <- (nrow(raster::as.matrix(bandred)) - (as.data.frame(x_owin5))$y) / nrow(RGB_stack_DEM)
  # 
  # # En el vector de coordenadas x de la ventana hacemos solo la operación 2
  # w5_x <- (as.data.frame(x_owin5))$x / raster::ncol(RGB_stack_DEM)
  
  # En el vector de coordenadas y de la ventana hcemos la operación 1 y 2
  w5_y_corr <- (nrow(raster::as.matrix(bandred)) - (as.data.frame(x_owin5))$y) / nrow(bandred)
  
  # En el vector de coordenadas x de la ventana hacemos solo la operación 2
  w5_x <- (as.data.frame(x_owin5))$x / raster::ncol(bandred)
  
  #Unimos los vectores
  xym5 <-  cbind(x = w5_x, y = w5_y_corr)
  #creamos el polígono
  p5 <-  sp::Polygon(xym5)
  # Creamos la lista de polígonos con un solo polígono, en este ejemplo el pocillo 5
  ps5 <- sp::Polygons(list(p5), "pocillo 5")
  # creamos el objeto SpatialPolygons
  sps <- sp::SpatialPolygons(list(ps5))
  # plot(sps, add=T, col="red")
  # plot(sps, col="red")
  return(sps)
}

################################################################################################################################################
extractPIX.from.Poly <-
  function(tif.path, poly){
    # file <- Sys.glob(path = paste0(tif.path, "vis/*.tif"))[1]
    file <-list.files(path = "./vis",full.names = T)[1]
    vis.tiff <- tiff::readTIFF(file)
    vis.red <- raster::raster(vis.tiff[, , 1])
    
    message("Extracting cells. Please wait.")
    cells <- data.frame(raster::extract(vis.red, poly, cellnumbers = T))[, 
                                                                         1]
    out <- data.frame(cells, raster::rowColFromCell(vis.red, cells), 
                      raster::xyFromCell(vis.red, cells))
    return(out)
  }

