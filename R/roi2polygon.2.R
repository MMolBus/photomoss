# TITLE: roi2polygon.2 
# Author: Manuel Molina-Bustamnte
# Date: 2018/04
# ---------------------------------------------------------------------------------------
# Function: "Transform a '.roi' file created with imageJ to a spatial polygon.
# Improved and customized read.ijroi internal function, as read.ijroi.2, with respect 
# to the previous version roi2polygon"
# ---------------------------------------------------------------------------------------

roi2polygon.2 <-
      function(roi.paths, tif.path){
            
 # install required packages----

            # "RImageJROI" to import .roi files.
            if(require(RImageJROI)==F){
                  install.packages("RImageJROI")
                  library(RImageJROI)
                  }else{
                        library(RImageJROI)}
            
            # "spatstat" to edit roi file format make it readable by photomoss
            if(require(RImageJROI)==F){
                  install.packages("spatstat")
                  library(spatstat)
                  }else{
                        library(spatstat)
                        }
            # "stringr" to set roi names
            if(require(RImageJROI)==F){
                  install.packages("stringr")
                  library(stringr)
                  }else{
                        library(stringr)
                        }
            
 # Create  read.ijroi.2 function to read roi files----
  read.ijroi.2 <- 
        function (file, verbose = FALSE){
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
  
 # we will put all the polygons in a list-----
  # Create empty list
  obs_areas <-  list()
  
  for(i in seq_along(roi.paths)){
        
        # Import ROIs made with imageJ
        
        roi <- 
              read.ijroi.2(roi.paths[i], verbose = FALSE)
        
        # Some format transformations
        # transform ijroi to  owin format
        owin <- 
              RImageJROI::ij2spatstat(roi)
        
        # Two opeerations to match window oposition over the raster:
        # 1) Turn upside down Y coordinates of the window
        # 2) Re-escalate window coordinates to 0 - 1, to match raster image reference.
        
        # we use te first image of the series as reference
        first.tif.filename <- 
              list.files(path = "./vis",full.names = T)[1]
        
        bandred <- 
              raster(first.tif.filename, band=1)
        
        # Make operations 1 and 2 in Y coordinate vector
        owin_y_corr <- 
              (nrow(raster::as.matrix(bandred)) - (as.data.frame(owin))$y) / nrow(bandred)
        
        # Make operation 2 in X coordinate vector
        owin_x_corr <- 
              (as.data.frame(owin))$x / raster::ncol(bandred)
        
        # Joint vectors as columns of a data frame
        owin_xy_corr <-cbind(x = owin_x_corr, y = owin_y_corr)
        
        # Create polygon
        poly <-  sp::Polygon(owin_xy_corr)
        
        # create polygon list with one polygon
        poly_list <- sp::Polygons(list(poly),  
                                  tail(strsplit(roi.paths[i], "/")[[1]],n=1))
        
        # create SpatialPolygons object
        sps <- sp::SpatialPolygons(list(poly_list))
        
        # include in roi list
        obs_areas[[i]] <- sps
        
  }
  
  return(obs_areas)
      }
