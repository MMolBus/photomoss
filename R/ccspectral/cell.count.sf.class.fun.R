# count number of cells by surface class to compare
#   manual segmentation and thresholded segmentation
# b_as_b => real (manual) background classified as background
# m_as_b => real (manual) background classified as moss
# b_as_m => real (manual) moss classifiedas background
# m_as_m => real (manual) moss classifiedas moss

cell.count.sf.class <- function(threshold.class, manual.class){

surface_class <- paste0(threshold.class, manual.class)

b_as_b <- length(surface_class[grep("00", surface_class)])
m_as_b <- length(surface_class[grep("01", surface_class)])
b_as_m <- length(surface_class[grep("10", surface_class)])
m_as_m <- length(surface_class[grep("11", surface_class)])

ncell_sf_class <- c(b_as_b,  
                    m_as_b, 
                    b_as_m, 
                    m_as_m)
return(ncell_sf_class)
}
