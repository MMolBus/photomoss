# Count number of cells by surface class to compare
# baseline segmentation mask and thresholded segmentation
# TN => True Negative: baseline background classified as background
# FP => False Positive: baseline background classified as moss
# FN => False Negative: baseline moss classified background
# TP => True Positive: baseline moss classified as moss

cell.count.sf.class <- function(threshold.class, manual.class){

surface_class <- paste0(threshold.class, manual.class)

TN <- length(surface_class[grep("00", surface_class)])
FP <- length(surface_class[grep("01", surface_class)])
FN <- length(surface_class[grep("10", surface_class)])
TP <- length(surface_class[grep("11", surface_class)])

ncell_sf_class <- c(TN, FP, FN, TP)
return(ncell_sf_class)
}
