#' Transform ImageJ '.roi' files to spatial polygons.
#' 
#' @description 
#' Read '.roi' files located in a folder and subfolders and transform to a list 
#' of spatial polygons.
#' 
#' @param roi.folder path to the folder that contains the '.roi' files. roi 
#' files could be located in subfolders. 
#' @param pic.folder path to the folder that contains the image files. 
#' The function will take the first image of the folder as reference. 
#' All images in the pic.folder must have the same dimensions.
#'
#' @return 
#' A list of spatial polygons
#' 
#' @examples#' 
#' roi2polygon.2 <- function(roi.folder = "./myrois", pic.folder = "./mypics")
#'
#' @author Manuel Molina-Bustamante
#'
#' @export

roi2polygon <- function(roi.folder, pic.folder) {
      
      # 1. Install required packages ----
      # Packages used in this function:
      # - "RImageJROI": to import .roi files
      # - "spatstat": to edit .roi file format to make it readable by photomoss
      # - "stringr": to handle ROI file names
      
      # 2. Create read.ijroi.2 function to read ROI files ----
      read.ijroi.2 <- function(file, verbose = FALSE) {
            # Define helper functions to read binary data from ROI files:
            
            # Function to read a single byte
            getByte <- function(con) { 
                  pos <- seek(con)  # Current position in file
                  n <- readBin(con, raw(0), 1, size = 1)  # Read a single byte
                  if (verbose) 
                        message(paste("Pos ", pos, ": Byte ", n, sep = ""))
                  return(as.integer(n))
            }
            
            # Function to read a short integer (2 bytes)
            getShort <- function(con) { 
                  pos <- seek(con)
                  n <- readBin(con, integer(0), 1, size = 2, signed = TRUE, endian = "big")
                  if (n < -5000) {  # Special case for certain ROI files
                        seek(con, -2, origin = "current")
                        n <- readBin(con, integer(0), 1, size = 2, signed = FALSE, endian = "big")
                  }
                  if (verbose) 
                        message(paste("Pos ", pos, ": Short ", n, sep = ""))
                  return(n)
            }
            
            # Function to read a 4-byte integer
            getInt <- function(con) { 
                  pos <- seek(con)
                  n <- readBin(con, integer(0), 1, size = 4, signed = TRUE, endian = "little")
                  if (verbose) 
                        message(paste("Pos ", pos, ": Integer ", n, sep = ""))
                  return(n)
            }
            
            # Function to read a 4-byte float
            getFloat <- function(con) { 
                  pos <- seek(con)
                  n <- readBin(con, double(0), 1, size = 4, signed = TRUE, endian = "big")
                  if (verbose) 
                        message(paste("Pos ", pos, ": Float ", n, sep = ""))
                  return(n)
            }
            
            # Subtype, option, and type definitions for ROIs
            subtypes <- list(TEXT = 1, ARROW = 2, ELLIPSE = 3, IMAGE = 4)
            opts <- list(SPLINE_FIT = 1, DOUBLE_HEADED = 2, OUTLINE = 4)
            types <- list(polygon = 0, rect = 1, oval = 2, line = 3, 
                          freeline = 4, polyline = 5, noRoi = 6, freehand = 7, 
                          traced = 8, angle = 9, point = 10)
            
            # Open the ROI file for reading
            con <- file(file, "rb")
            if (getByte(con) != 73 || getByte(con) != 111) {  # Check for valid ROI file
                  stop("This is not an ImageJ ROI")
            }
            if (verbose) 
                  message("Reading format data")
            
            # Parse the ROI file and extract data
            r <- list()  # Initialize an empty list for ROI data
            getShort(con)  # Skip reserved bytes
            r$version <- getShort(con)  # ROI version
            r$type <- getByte(con)  # ROI type
            getByte(con)  # Skip a byte
            r$top <- getShort(con)  # Top coordinate
            r$left <- getShort(con)  # Left coordinate
            r$bottom <- getShort(con)  # Bottom coordinate
            r$right <- getShort(con)  # Right coordinate
            r$width <- with(r, right - left)  # ROI width
            r$height <- with(r, bottom - top)  # ROI height
            r$n <- getShort(con)  # Number of points (for polygon types)
            
            # Additional ROI properties
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
            
            # Close the file connection
            close(con)
            
            # Return the parsed ROI as a list
            return(r)
      }
      
      # 3. Extract and process all ROI files into polygons ----
      
      # Create an empty list to store polygons
      obs_areas <- list()
      
      # List all .roi files in the provided folder
      roi_paths <- list.files(path = roi.folder, pattern = ".roi$", full.names = TRUE, recursive = TRUE)
      message(paste(length(roi_paths), "ROI files located"))
      
      # Use the first image in the folder as a reference for spatial alignment
      raster_ref <- list.files(path = pic.folder, full.names = TRUE)[1] %>% 
            terra::rast(.)
      
      # Loop through each ROI file
      for (i in seq_along(roi_paths)) {
            # Import ROI using the helper function
            roi <- read.ijroi.2(roi_paths[i], verbose = FALSE)
            
            # Convert ROI to spatial polygon format
            owin <- RImageJROI::ij2spatstat(roi)
            
            # Correct the Y coordinates (invert and normalize)
            owin_y_corr <- (nrow(terra::as.matrix(raster_ref)) - (as.data.frame(owin))$y) / terra::nrow(raster_ref)
            
            # Normalize the X coordinates
            owin_x_corr <- (as.data.frame(owin))$x / terra::ncol(raster_ref)
            
            # Combine corrected X and Y coordinates into a data frame
            owin_xy_corr <- cbind(owin_x_corr, owin_y_corr)
            
            # Create a polygon from the corrected coordinates
            poly <- terra::rast(owin_xy_corr)
            sps <- terra::as.polygons(poly)
            
            # Add the polygon to the list
            obs_areas[[i]] <- sps
            
            # Print progress
            print(paste(i, "of", length(roi_paths), "ROI files processed"))
      }
      
      # Return the list of polygons
      return(obs_areas)
}
