# test-chart2

# Load necessary libraries for testing
library(testthat)
library(devtools)

# Load the package for testing
devtools::load_all()

# Define the test for the chart2 function
test_that("chart2 function", {
      # Load a TIFF picture for testing
      picture <- tiff::readTIFF(list.files(
            path = pic.path,
            pattern = ".tif$",
            full.names = T
      )[1])
      
      # Get raster picture dimensions for creating a doomy tile.coords dataframe
      pic_raster <- terra::rast(picture)
      
      # Set parameters for chart2 function
      pic_path <- "data/tiff/vis"
      samp_width <- 40
      interactive <- FALSE
      tile_coords <-
            data.frame(
                  x = round(runif(24), 2) * dim(pic_raster)[2],
                  y = round(runif(24), 2) * dim(pic_raster)[1]
            )
      xriteclassic_chart <- TRUE
      n_color_tiles <- NULL
      
      # If tif images-----------------------------------------------------------
      pic_format <- "tif"
      
      # Call the chart2 function
      result <- chart2(
            pic.path = pic_path,
            samp.width = samp_width,
            pic.format = pic_format,
            interactive = FALSE ,
            xriteclassic.chart = xriteclassic_chart,
            n.color.tiles = n_color_tiles,
            tile.coords = tile_coords
      )
      
      # Test the result
      expect_s4_class(result, "SpatVector")
      expect_equal(length(result),
                   ifelse(xriteclassic_chart, nrow(tile_coords), n_color_tiles))
      
      # If jpg images-----------------------------------------------------------
      pic_format <- "jpeg"
      pic_path <- "data/tiff/jpeg"
      
      # Call the chart2 function
      result <- chart2(
            pic.path = pic_path,
            samp.width = samp_width,
            pic.format = pic_format,
            interactive = FALSE ,
            xriteclassic.chart = xriteclassic_chart,
            n.color.tiles = n_color_tiles,
            tile.coords = tile_coords
      )
      
      # Test the result
      expect_s4_class(result, "SpatVector")
      expect_equal(length(result),
                   ifelse(xriteclassic_chart, nrow(tile_coords), n_color_tiles))
})
