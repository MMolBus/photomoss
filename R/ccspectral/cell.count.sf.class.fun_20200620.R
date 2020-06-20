# count number of cells by surface class to compare
# manual segnmentation and autothresholded segmentation
# > background as background
# > background as moss
# > moss as background
# > moss as moss

cell.count.sf.class <- function(manual.class, auto.class){

surface_class <- paste0(manual.class, auto.class)

b_as_b <- length(surface_class[grep("00", surface_class)])
m_as_b <- length(surface_class[grep("01", surface_class)])
b_as_m <- length(surface_class[grep("10", surface_class)])
m_as_m <- length(surface_class[grep("11", surface_class)])

ncell_sf_class <- c(background_as_background,  
                    moss_as_background, 
                    background_as_moss, 
                    moss_as_moss)
return(ncell_sf_class)
}
