# test-chart2 



# data <- data.frame(Names = c("Pippin", "Merry"))
system.file("data", "tiff", package = "PhotomossR") |> list.files()

getwd()

work_dir <- "./data/tiff" 

setwd(work_dir)
getwd()

chart <-

      chart2(pic.path = "./vis", pic.format = "tif")


path <- tempdir()

library(testthat)
library(mockr)
# Micro test

devtools::load_all()

setwd("..")
getwd()


# Define the test
test_that("chart2 function", {
      # Override the locator function for testing
      original_locator <- graphics::locator
      locator <- function(n = 1, type = "p") {
            coords <- list(x = runif(n), y = runif(n))
            return(coords)
      }
      
      # Set parameters for chart2 function
      pic.path <- "data/tiff/vis"
      samp.width <- 0.01
      pic.format <- "tif"
      xriteclassic.chart <- TRUE
      n.color.tiles <- NULL
      chart.coords <- list(x = runif(24), y = runif(24))

      # Call the chart2 function
      result <- chart2(pic.path, samp.width, pic.format, xriteclassic.chart, n.color.tiles)
      
      # Test the result
      expect_s4_class(result, "SpatVector")
      expect_equal(length(result), ifelse(xriteclassic.chart, 24, n.color.tiles))
      
      # Restore the original locator function
      locator <- original_locator
})




pic.path <- "data/tiff/vis"
samp.width <- 0.01
pic.format <- "tif"
xriteclassic.chart <- TRUE
n.color.tiles <- NULL

local({
      # Override the locator function to return predefined coordinates
      local_mock(
            original_locator <- locator,
            locator <- function(n = 1, type = "p") {
                  coords <- list(x = runif(n), y = runif(n))
                  return(coords)}
            # locator = function(n = n.color.tiles , type = "p")
            #       list(x = round(runif(
            #             length(n.color.tiles), 0, 1
            #       ), 2),
            #       y = round(runif(
            #             length(n.color.tiles), 0, 1
            #       ), 2))
      )
      #
      # test_that("test chart function", {
      #       # Your test code here
      # })
      #
      pic.path <- "data/tiff/vis"
      samp.width <- 0.01
      pic.format <- "tif"
      xriteclassic.chart <- TRUE
      n.color.tiles <- NULL
      
      result <- 
            chart2(pic.path, samp.width, pic.format, xriteclassic.chart, n.color.tiles)
      
      expect_s4_class(result, "SpatialPolygons")
      expect_equal(length(result),
                   ifelse(xriteclassic.chart, 24, n.color.tiles))
      # Restore the original locator function
      locator <- original_locator
})

result <- chart2(pic.path, samp.width, pic.format, xriteclassic.chart, n.color.tiles)
class(result)
# expect_is(result, "SpatialPolygonsDataFrame")
expect_s4_class(result, "SpatialPolygons")
expect_equal(length(result), ifelse(xriteclassic.chart, 24, n.color.tiles))


      # Define your test
      test_that("chart2 function works correctly", {
            # Set up any necessary variables or data
# test_that("PDF certificates are created", {
      
      skip_on_ci()
      skip_on_cran()
      
      # Load the necessary library

      
            pic.path <- "path/to/your/test/image"
            samp.width <- 0.01
            pic.format <- "jpg"
            xriteclassic.chart <- TRUE
            n.color.tiles <- NULL
            
            # Call your function with the test data
            result <- chart2(pic.path, samp.width, pic.format, xriteclassic.chart, n.color.tiles)
            
            # Check that the result is as expected
            expect_is(result, "SpatialPolygonsDataFrame")
            expect_equal(length(result), ifelse(xriteclassic.chart, 24, n.color.tiles))
      })
      
      
      
       
      # ## Spanish
      # create_certificate_attendance(data = data, path = path, type = "Adventure",
      #                               title = "Going to Mordor",
      #                               hours = "1000", name.column = "Names",
      #                               language = "Spanish", signer = "Gandalf",
      #                               date = "10/07/3064"
      # )
      # 
      # expect_true(file.exists(file.path(path, "Asistencia_Pippin.pdf")))
      # expect_true(file.exists(file.path(path, "Asistencia_Merry.pdf")))
      # 
      # 
      # ## English
      # create_certificate_attendance(data = data, path = path, type = "Adventure",
      #                               title = "Going to Mordor",
      #                               hours = "1000", name.column = "Names",
      #                               language = "English", signer = "Gandalf",
      #                               date = "10/07/3064"
      # )
      # 
      # expect_true(file.exists(file.path(path, "Attendance_Pippin.pdf")))
      # expect_true(file.exists(file.path(path, "Attendance_Merry.pdf")))
      
      
# })


# test_that("Rmd file is present when keep.files = TRUE", {
#       
#       skip_on_ci()
#       skip_on_cran()
#       
#       create_certificate_attendance(data = data, path = path, type = "Adventure",
#                                     title = "Going to Mordor",
#                                     hours = "1000", name.column = "Names",
#                                     language = "English", signer = "Gandalf",
#                                     date = "10/07/3064",
#                                     keep.files = TRUE
#       )
#       
#       expect_true(file.exists(file.path(path, "attendance.Rmd")))
#       
# })
