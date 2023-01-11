# Title: chart.from.tif
# Function:
# R functions
#' Manual selection of the image regions where we can find the color checker tiles.
#' @param x string. File path where you can find the image files.
#' @param samp.width numeric. Distance from original click point to establish the perimeter of the new geometry. Of length 1 replicated to the number of input click points, or of length equal to the number of click points.
#' @param pic.format character. Picture file format. It could be "jpg" for .jpg,.JPG and .jpeg; or "tif", for .tif format.
#'
#' @return A raster with 24 features one by each color tile.
#'
#' @author Manuel Molina-Bustamante
#' @examples
#'
#' chart2(pic.path="./JPG", samp.width = 0.01, pic.format = "jpg")
#'
#' @export

chart2 <- function(pic.path,
                   samp.width = 0.01,
                   pic.format){


  chartf <- function(pic.path,
                     samp.width,
                     pic.format) {
    if (require(jpeg) == F) {
      install.packages("jpeg")

      library(jpeg)
    }

    if (require(raster) == F) {
      install.packages("raster")

      library(raster)
    }

    if (pic.format == "jpg") {
      file <-
        list.files(path = pic.path ,
                   pattern = ".jpg$|.JPG$|.jpeg$",
                   full.names = T)[1]

      pic <- jpeg::readJPEG(file)

    }

    if (pic.format == "tif") {
      file <- list.files(path = "./vis",
                         pattern = ".tif$",
                         full.names = T)[1]

      pic <- tiff::readTIFF(file)
    }

    pic.1 <- raster::raster(pic[, , 1])
    pic.2 <- raster::raster(pic[, , 2])
    pic.3 <- raster::raster(pic[, , 3])

    pic.raster <- raster::stack(pic.1,
                                pic.2,
                                pic.3)

    options(warn = -1)

    op <-
      par(
        mfrow = c(1, 1),
        mar = c(0, 0, 0, 0),
        oma = c(0, 0, 0, 0)
      )

    on.exit(par(op))

    X11()
    raster::plotRGB(pic.raster,
                    scale = 1,
                    asp = nrow(pic.1) / ncol(pic.1))

    options(warn = 0)

    # chart.coords <- data.frame(x = numeric(), y = numeric())

    message(
      "Color chart has 6 columns and 4 rows. Bottom row correspond to grayscale tiles. Click on all 24 color chart cells in sequence. The sequence follows left to right as follows: starts at cell 1 (brown, top left) and finishes on cell 24 (black, bottom right)."
    )

    # for (i in 1:24) {
    #   options(warn = -1)
    #   chart.coords[i, 1:2] <- click(xy = T)[1:2]
    #   options(warn = 0)
    # }
    chart.coords <- locator(n = 24, type = "p")
    chart.coords <- cbind(chart.coords[[1]], chart.coords[[2]])
    colnames(chart.coords) <- c("x", "y")

    sp.chart <- sp::SpatialPoints(chart.coords)

    chart_buff <-
      rgeos::gBuffer(sp.chart, width = samp.width, byid = T)

    # plot(chart_buff, add = T, col = "green")

    return(chart_buff)
  }

  chart <- chartf(pic.path = pic.path,
                  samp.width = samp.width,
                  pic.format = pic.format)

  plot(chart, add = T, col = "green")

  return(chart)

}
