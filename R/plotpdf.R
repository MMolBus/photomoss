# Set plotpdf function to plot results (operated by lists) ---------------------
plotpdf <-  function(lhist, lind, lman, lover, i.names, asp, pdf.name){
  # set pdf structure ----------------------------------------------------------
  
  pdf(file = pdf.name, w = 14, h = 3.571429 * length(index.))
  par(mfrow = c(length(index.), 4))
  
  # set function for pdf graphic content ---------------------------------------
  # hist:raster dataframe with x y coordinates index value (z) and binary mask value (surface)
  # ind: index raster
  
  pdfprint <-   function(hist, ind, man, over, name, asp){
    # set surface binary image as factor ---------------------------------------
    
    surface.f <- factor(hist[,4], levels= c(1,0),
                        labels = c("no_moss","moss"))
    # surface.overlap <- factor(hist[,5], levels= c(1,2,3),
    #                     labels = c("substrate","overlap","moss"))
    #
    # PLOT densities -----------------------------------------------------------
    if(require(sm)!=T){
      install.packages("sm")
    }
    sm::sm.density.compare(hist[,3], surface.f, xlab= name)
    
    title(main = paste(names), "values by surface")
    # add legend
    colfill <- c(2:(2+length(levels(surface.f))))
    legend("topright", levels(surface.f), fill=colfill)
    
    # PLOT index values and real moss contour ----------------------------------
    plot(ind,
         # main =  paste(toupper(names)),"values",
         axes = FALSE, box = FALSE,
         asp  = asp)
    plot(moss_poly, add=T, border="red")
    
    # PLOT index values from real moss area and real moss contour  -------------
    plot(man,
         main =  paste(toupper(name)),"moss values over whole scene",
         axes = FALSE, box = FALSE,
         asp  = asp)
    plot(moss_poly, add=T, border="red")
    
    # PLOT overlap index values between real moss area and background  ---------
    plot(over,
         main =  paste(toupper(names)),"index overlap regions",
         axes = FALSE, box = FALSE,
         asp  = asp)
    plot(moss_poly, add=T, border="red")
    
  }
  # run pdf.print over our list of indexes -------------------------------------
  lapply(c(1:length(lind)), function(i)
    pdfprint(hist  = list_df_results[[i]][,3],
             ind   = lind[[i]],
             man   = lman[[i]],
             over  = lover[[i]],
             names = i.names[[i]],
             asp   = asp))
  # close pdf ------------------------------------------------------------------
  dev.off()
}