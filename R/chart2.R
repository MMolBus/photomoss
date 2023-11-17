#' chart2:  Manual selection of points where color checker tiles are located in
#' the image.
#' 
#' @description 
#' Manual selection of points where color checker tiles are located in the image.
#' You need to click over color tiles in the opened window where you can find 
#' the picture with the color chart. 
#' To click over the tiles you need to follow the order shown in
#' \href{https://raw.githubusercontent.com/MMolBus/photomoss/master/vignettes/vignette_Photomoss_workflow/chart.png}{this image}.
#' Preferably, click on the color tile centroids.
#' 
#' @param pic.path string. File path where you can find the image files.
#' @param samp.width numeric. Distance to establish 
#' the buffer arround click point. This argument feeds the argument with from terra::buffer function operated internally by chart2. Default value with = 40. 
#' input click points, or of length equal to the number of click points.
#' @param pic.format character. Picture file format. It could be "jpg" for .jpg,
#' .JPG and .jpeg; or "tif", for .tif format.
#' @param xriteclassic.chart logical. Indicates if we are using 24 color tiles 
#' from Xrite classic ColorCheker to make the color calibration. Default = TRUE. 
#' If FALSE (you are not using Xrite classic ColorCheker, or you want to use 
#' another quantity of color tiles) you need to provide the number of tiles 
#' you would use in the n.color.tiles parameter  
#' @param n.color.tiles numeric. Only required if you would not use 24 color tiles 
#' from Xrite Classic ColorCheker to make the color calibration. In that case 
#' you need to provide the number of tiles (n) you would use in your color chart to 
#' make the color calibration.

#'
#' @return 
#' A spatial.polygon with 24 features one by each color tile in case 
#' xriteclassic.chart = T, or n features if xriteclassic.chart = F and n.color.tiles = n.
#'  
#'
#' @examples#'
#' chart2(pic.path="./JPG", samp.width = 0.01, pic.format = "jpg", 
#' xriteclassic.chart = T, n.color.tiles)
#' chart2(pic.path="./JPG", samp.width = 0.01, pic.format = "jpg", 
#' xriteclassic.chart = F, n.color.tiles = 4) # if you only want to calibrate with a color chart with 4 tiles
#'
#' @author Manuel Molina-Bustamante
#' @export

chart2 <- function(pic.path,
                   samp.width = 40,
                   pic.format, 
                   xriteclassic.chart = T,
                   n.color.tiles){



      if (require(jpeg) == F) {
            install.packages("jpeg")
            
            library(jpeg)
      }
      
      if (require(terra) == F) {
            install.packages("terra")
            
            library(terra)
      }
      
      if (pic.format == "jpg") {
            file <-
                  list.files(path = pic.path ,
                             pattern = ".jpg$|.JPG$|.jpeg$",
                             full.names = T)[1]
            
            pic <- jpeg::readJPEG(file)
            
      }

      if (pic.format == "tif") {
            file <- list.files(path = pic.path,
                               pattern = ".tif$",
                               full.names = T)[1]
            
            pic <- tiff::readTIFF(file)
      }
      
      pic.raster <-
            terra::rast(pic)
      
    options(warn = -1)

    op <-
      par(
        mfrow = c(1, 1),
        mar = c(0, 0, 0, 0),
        oma = c(0, 0, 0, 0)
      )

    on.exit(par(op))

    X11()
    terra::plotRGB(pic.raster, r = 1, g = 2, b = 3, scale=1)
    options(warn = 0)

    
if(xriteclassic.chart == T){
      message(
            "You are using Xrite classic ColorCheker"
            )
      message(
            "Color chart has 6 columns and 4 rows. Bottom row correspond to grayscale tiles. Click on all 24 color chart cells in sequence. The sequence follows left to right as follows: starts at cell 1 (brown, top left) and finishes on cell 24 (black, bottom right)."
            )

      n.color.tiles <- 24
      chart.coords <- locator(n = n.color.tiles, type = "p")
    }else{
          if(exists("n.color.tiles")==T){
                
          chart.coords <- locator(n = n.color.tiles, type = "p")
          }else{
                message(
                      "You are not using Xrite classic ColorCheker but you has not provided the number of tiles you want to check in the new color chart")
          }
          
}
    
    chart.coords <- cbind(chart.coords[[1]], chart.coords[[2]])
    colnames(chart.coords) <- c("x", "y")

    sp.chart <- terra::vect(chart.coords[, c("x", "y")])
    
    chart_buff <-
      terra::buffer(sp.chart, width = samp.width)
    terra::plotRGB(pic.raster, r = 1, g = 2, b = 3, scale=1)
    terra::points(chart_buff, col = "green", cex= 0.25)

    return(chart_buff)

}
