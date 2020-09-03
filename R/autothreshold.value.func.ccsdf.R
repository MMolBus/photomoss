# Autothreshold.value function 

autothreshold.value <- function(raster, max.index, min.index, method) {
  index <- names(raster)
  raster <- raster[[1]]
  col.0.255.bin.range <- function (raster, max.index, min.index) {
    a   <- max.index - min.index
    bin <- .bincode(#as.vector(raster),
      x              = getValues(raster),
      breaks         =  seq(min.index, max.index, (a / 256)),
      include.lowest = T)
    return(bin)
  }
  
  col_code_index <- col.0.255.bin.range(raster, max.index, min.index)
  index_values   <- values(raster)
  
  df_colcode_indexvalues <- cbind.data.frame(
    "colcode" = col_code_index,
    "indexvalues" = index_values)
  
  values(raster) <- df_colcode_indexvalues$colcode
  
  atm <- autothresholdr::auto_thresh_apply_mask(
    values(raster),
    method,
    ignore_white = T,
    ignore_black = T,
    ignore_na = TRUE
  )
  pt  <- attr(atm, "thresh")[1]
  p   <- sort(unique(df_colcode_indexvalues$colcode), decreasing = F)[pt]
  out <- min((df_colcode_indexvalues[df_colcode_indexvalues$colcode == p, ])$indexvalues)
  
  print(paste0(index," ",method," threshold value = ",out))
  return(out)
}
