# TITLE: roi2polygon.2 
# Function: "Transform a '.roi' file created with imageJ to a spatial polygon, Improved and customized read.ijroi internal function, as read.ijroi.2, with respect to the previous version roi2polygon" 
roi2polygon.2 <- function(roi.path, tif.path){
  # Para importar los archivos punto .roi instalamos el paquete "RImageJROI"
  # library(RImageJROI)
  # Importamos el área de interes que creamos con Image J
  # usamos como EJEMPLO la número 5 (la numeración de las celdillas las sabemos gracias  a la referencia
  # y orden conocido de los alveolos en la fotografía, algo que fuimos
  # apuntando según creabamos las celdillas en el Image J)
  # roi.path <- roi.paths[[1]]
  
  read.ijroi.2 <- function (file, verbose = FALSE) {
    {
      getByte <- function(con) {
        pos <- seek(con)
        n <- readBin(con, raw(0), 1, size = 1)
        if (verbose) 
          message(paste("Pos ", pos, ": Byte ", n, sep = ""))
        return(as.integer(n))
      }
      getShort <- function(con) {
        pos <- seek(con)
        n <- readBin(con, integer(0), 1, size = 2, signed = TRUE, 
                     endian = "big")
        if (n < -5000) {
          seek(con, -2, origin = "current")
          n <- readBin(con, integer(0), 1, size = 2, signed = FALSE, 
                       endian = "big")
        }
        if (verbose) 
          message(paste("Pos ", pos, ": Short ", n, sep = ""))
        return(n)
      }
      getInt <- function(con) {
        pos <- seek(con)
        n <- readBin(con, integer(0), 1, size = 4, signed = TRUE, 
                     endian = "little")
        if (verbose) 
          message(paste("Pos ", pos, ": Integer ", n, sep = ""))
        return(n)
      }
      getFloat <- function(con) {
        pos <- seek(con)
        n <- readBin(con, double(0), 1, size = 4, signed = TRUE, 
                     endian = "big")
        if (verbose) 
          message(paste("Pos ", pos, ": Float ", n, sep = ""))
        return(n)
      }
      subtypes <- list(TEXT = 1, ARROW = 2, ELLIPSE = 3, IMAGE = 4)
      opts <- list(SPLINE_FIT = 1, DOUBLE_HEADED = 2, OUTLINE = 4)
      types <- list(polygon = 0, rect = 1, oval = 2, line = 3, 
                    freeline = 4, polyline = 5, noRoi = 6, freehand = 7, 
                    traced = 8, angle = 9, point = 10)
      name <- NULL
      if (!is.null(file)) {
        size <- file.info(file)$size
        if (!grepl(".roi$", file) && size > 5242880) 
          stop("This is not an ROI or file size>5MB)")
        name <- basename(file)
      }
      con <- file(file, "rb")
      if (getByte(con) != 73 || getByte(con) != 111) {
        stop("This is not an ImageJ ROI")
      }
      if (verbose) 
        message("Reading format data")
      r <- list()
      getShort(con)
      r$version <- getShort(con)
      r$type <- getByte(con)
      getByte(con)
      r$top <- getShort(con)
      r$left <- getShort(con)
      r$bottom <- getShort(con)
      r$right <- getShort(con)
      r$width <- with(r, right - left)
      r$height <- with(r, bottom - top)
      r$n <- getShort(con)
      r$x1 <- getFloat(con)
      r$y1 <- getFloat(con)
      r$x2 <- getFloat(con)
      r$y2 <- getFloat(con)
      r$strokeWidth <- getShort(con)
      r$shapeRoiSize <- getInt(con)
      r$strokeColor <- getInt(con)
      r$fillColor <- getInt(con)
      r$subtype <- getShort(con)
      r$options <- getShort(con)
      if ((r$type == types["freehand"]) && (r$subtype == subtypes["ELLIPSE"])) {
        r$aspectRatio <- getFloat(con)
      }
      else {
        r$style <- getByte(con)
        r$headSize <- getByte(con)
        r$arcSize <- getShort(con)
      }
      r$position <- getInt(con)
      getShort(con)
      getShort(con)
      if (verbose) 
        message("Reading coordinate data")
      # if (!is.null(name) && (grepl(".roi$", name))) 
      #   r$name <- substring(name, 1, nchar(name) - 4)
      # isComposite <- (r$shapeRoiSize > 0)
      # if (isComposite) {
      #   stop("Composite ROIs not supported")
    }
    if (r$type %in% types["line"]) {
      if (r$subtype %in% subtypes["ARROW"]) {
        r$doubleHeaded <- (r$options & opts$DOUBLE_HEADED)
        r$outline <- (r$options & opts$OUTLINE)
      }
    }
    if (r$type %in% types[c("polygon", "freehand", "traced", 
                            "polyline", "freeline", "angle", "point")]) {
      r$coords <- matrix(NA, r$n, 2)
      if (r$n > 0) {
        for (i in 1:r$n) {
          r$coords[i, 1] <- getShort(con)
        }
        for (i in 1:r$n) {
          r$coords[i, 2] <- getShort(con)
        }
        r$coords[r$coords < 0] <- 0
        r$coords[, 1] <- r$coords[, 1] + r$left
        r$coords[, 2] <- r$coords[, 2] + r$top
      }
    }
    close(con)
    if (r$type %in% types["line"]) {
      r$coords <- matrix(NA, 2, 2)
      r$coords[1, 1] <- r$x1
      r$coords[2, 1] <- r$x2
      r$coords[1, 2] <- r$y1
      r$coords[2, 2] <- r$y2
    }
    if (is.null(r$coords)) {
      Xcoords <- unlist(c(r[names(r) %in% c("left", "x1")], 
                          r[names(r) %in% c("right", "x2")]))
      Ycoords <- unlist(c(r[names(r) %in% c("top", "y1")], 
                          r[names(r) %in% c("bottom", "y2")]))
      r$coords <- data.frame(x = Xcoords, y = Ycoords)
    }
    colnames(r$coords) <- c("x", "y")
    r$types <- types
    r$strType <- names(types)[which(types == r$type)]
    if (r$subtype != 0) {
      r$subtypes <- subtypes
      r$strSubtype <- names(subtypes)[which(subtypes == r$subtype)]
    }
    if (r$type == r$types[["oval"]] | r$type == r$types[["rect"]]) {
      r$xrange <- range(c(r$left, r$right))
    }
    else {
      r$xrange <- range(r$coords[, 1])
    }
    if (r$type == r$types[["oval"]] | r$type == r$types[["rect"]]) {
      r$yrange <- range(c(r$top, r$bottom))
    }
    else {
      r$yrange <- range(r$coords[, 2])
    }
    class(r) <- "ijroi"
    return(r)
  }
  x_roi5 <- read.ijroi.2(roi.path, verbose = FALSE)
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