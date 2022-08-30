
# list.raster.results = list_raster_results
# threshold.method = threshold.method


calculate.raster.thresh.fun <- 
  function(
  list.raster.results,
  calculate.thresh,
  threshold.vector,
  threshold.method){
    
    index.      <- names(list.raster.results)
    index_order <- c("NDVI", "SR", "MSAVI", "EVI", "CI",  "BSCI", "BI", "NORR", "NORG", 
                   "NORB", "EXR", "EXG", "EXB", "EXGR","CIVE", "VEG", "HUE", 
                   "SAT" , "VAL")
    if(calculate.thresh==T){
    t_values <- as.list(rep(NA, length(index_order)))
    
    }else{
      t_values <-  rep(NA, length(index_order))
      t_values[index_order%in%index.] <- threshold.vector
      }
    raster_index_cut <- as.list(rep(NA, length(index_order)))
    # NDVI autothres ------------------------------------------------------------
    if (any(index.==index_order[1])== TRUE) {
      ri <- list.raster.results[index_order[1]]
      if(calculate.thresh==T){
      t_values[[1]] <-
        autothreshold.value(
          raster = ri,
          max.index = 1,
          min.index = -1,
          method    = threshold.method
        )
      }
      if (is.na(t_values[[1]])==T){
        # raster_index_cut[[1]] <- NA
        raster_index_cut[[1]] <- setValues(ri[[1]],  NA)
        message(paste("Can't calculate",index.[1], "authothreshold by", threshold.method, "method." ))
        }else{
          raster_index_cut[[1]]  <- ri[[1]] >= t_values[[1]]
          }
      }
    
  # SR autothres -----------------------------------------------------------
  if (any(index.==index_order[2]) == TRUE) {
    ri <- list.raster.results[index_order[2]]
    if(calculate.thresh==T){
      t_values[[2]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = max(getValues(ri[[1]]), na.rm = T),
          min.index = 0,
          method    = threshold.method
        )
    }
    if (is.na(t_values[[2]])==T){
      raster_index_cut[[2]] <- setValues(ri[[1]],  NA)
      message(paste("Can't calculate",index.[2], "authothreshold by", threshold.method, "method." ))
    }else{
    raster_index_cut[[2]]  <- ri[[1]] >= t_values[[2]]
    }
  }
  
  # MSAVI autothreshold -----------------------------------------------------
  if (any(index.==index_order[3]) == TRUE) {
    ri <- list.raster.results[index_order[3]]
    if(calculate.thresh==T){
      t_values[[3]]  <- 
        autothreshold.value(
          raster    = ri,
          max.index = 1,
          min.index = -1,
          method    = threshold.method
        )
    }
    if (is.na(t_values[[3]])==T){
      raster_index_cut[[3]] <- setValues(ri[[1]],  NA)
      message(paste("Can't calculate",index.[3], "authothreshold by", threshold.method, "method." ))
    }else{
    raster_index_cut[[3]]  <- ri[[1]] >= t_values[[3]]
    }
  }
  
  # EVI autothreshold -------------------------------------------------------
  if (any(index.==index_order[4]) == TRUE) {
    ri <- list.raster.results[index_order[4]]
    if(calculate.thresh==T){
      t_values[[4]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = 1,
          min.index = -1,
          method    = threshold.method
        )
    }
    if (is.na(t_values[[4]])==T){
      # raster_index_cut[[4]] <- NA
      raster_index_cut[[4]] <- setValues(ri[[1]],  NA)
      message(paste("Can't calculate",index.[4], "authothreshold by", threshold.method, "method." ))
    }else{
    raster_index_cut[[4]]  <- ri[[1]] >= t_values[[4]]
    }
  }
  
  # CI autothreshold -------------------------------------------------------
  if (any(index.==index_order[5]) == TRUE) {
    ri <- list.raster.results[index_order[5]]
    
    if(calculate.thresh==T){
      t_values[[5]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = 2,
          min.index = 0,
          method    = threshold.method
        )
    }
    if (is.na(t_values[[5]])==T){
      raster_index_cut[[5]] <- setValues(ri[[1]],  NA)
      message(paste("Can't calculate",index.[5], "authothreshold by", threshold.method, "method." ))
    }else{
    raster_index_cut[[5]]  <- ri[[1]] <= t_values[[5]]
    }
  }
  
  # BSCI autothreshold ----------------------------------------------------
  if (any(index.==index_order[6]) == TRUE) {
    ri <- list.raster.results[index_order[6]]
    if(calculate.thresh==T){
      t_values[[6]] <- 
        autothreshold.value(
          raster  = ri,
          max.index = max(getValues(ri[[1]]), na.rm = T),
          min.index = 0,
          method    = threshold.method
        )
    }
    if (is.na(t_values[[6]])==T){
      raster_index_cut[[6]] <- setValues(ri[[1]],  NA)
      message(paste("Can't calculate",index.[6], "authothreshold by", threshold.method, "method." ))
    }else{
    raster_index_cut[[6]]  <- ri[[1]] >= t_values[[6]]}
  }
  
  # BI autothreshold ---------------------------------------------------
  if (any(index.==index_order[7]) == TRUE) {
    ri <- list.raster.results[index_order[7]]
    if(calculate.thresh==T){
      t_values[[7]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = 2,
          min.index = 0,
          method    = threshold.method
        )
    }
    if (is.na(t_values[[7]])==T){
      raster_index_cut[[7]] <- setValues(ri[[1]],  NA)
      message(paste("Can't calculate",index.[7], "authothreshold by", threshold.method, "method." ))
    }else{
    raster_index_cut[[7]]  <- ri[[1]] <= t_values[[7]]
    }
  }
  
  # NorR autothreshold -------------------------------------------------
  if (any(index.==index_order[8])== TRUE) {
    ri <- list.raster.results[index_order[8]]
    if(calculate.thresh==T){
      t_values[[8]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = 1,
          min.index = 0,
          method    = threshold.method
        )
    }
    if (is.na(t_values[[8]])==T){
      raster_index_cut[[8]] <- setValues(ri[[1]],  NA)
      message(paste("Can't calculate",index.[8], "authothreshold by", threshold.method, "method." ))
    }else{
    raster_index_cut[[8]]  <- ri[[1]] >= t_values[[8]]
    }
  }
  
  # NorG autothreshold ------------------------------------------------
  if (any(index.==index_order[9])== TRUE) {
    ri <- list.raster.results[index_order[9]]
    
    if(calculate.thresh==T){
      t_values[[9]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = 1,
          min.index = 0,
          method    = threshold.method
        )
    }
    if (is.na(t_values[[9]])==T){
      raster_index_cut[[9]] <- setValues(ri[[1]],  NA)
      message(paste("Can't calculate",index.[9], "authothreshold by", threshold.method, "method." ))
    }else{
    raster_index_cut[[9]]  <- ri[[1]] <= t_values[[9]]
    }
  }
  
  # NorB autothreshold -----------------------------------------------
  if (any(index.==index_order[10])== TRUE) {
    ri <- list.raster.results[index_order[10]]
    
    if(calculate.thresh==T){
      t_values[[10]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = 1,
          min.index = 0,
          method    = threshold.method
        )
    }
    if (is.na(t_values[[10]])==T){
      # raster_index_cut[[10]] <- NA
      raster_index_cut[[10]] <- setValues(ri[[1]],  NA)
      message(paste("Can't calculate",index.[10], "authothreshold by", threshold.method, "method." ))
    }else{
    raster_index_cut[[10]] <- ri[[1]] <= t_values[[10]]
    }
  }
  
  # ExR autothreshold ------------------------------------------------
  if (any(index.==index_order[11])== TRUE) {
    ri <- list.raster.results[index_order[11]]
    if(calculate.thresh==T){
      t_values[[11]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = 1.4,
          min.index = -1,
          method    = threshold.method
        )
    }
    if (is.na(t_values[[11]])==T){
      raster_index_cut[[11]] <- setValues(ri[[1]],  NA)
      message(paste("Can't calculate",index.[11], "authothreshold by", threshold.method, "method." ))
    }else{
    raster_index_cut[[11]] <- ri[[1]] >= t_values[[11]]
    }
  }
  
  # ExG autothreshold -----------------------------------------------
  if (any(index.==index_order[12])== TRUE) {
    ri <- list.raster.results[index_order[12]]
    if(calculate.thresh==T){
      t_values[[12]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = 2,
          min.index = -2,
          method    = threshold.method
        )
    }
    if (is.na(t_values[[12]])==T){
      raster_index_cut[[12]] <- setValues(ri[[1]],  NA)
      message(paste("Can't calculate",index.[12], "authothreshold by", threshold.method, "method." ))
    }else{
    raster_index_cut[[12]] <- ri[[1]] <= t_values[[12]]
    }
  }
  
  # ExB autothreshold -----------------------------------------------
  if (any(index.==index_order[13])== TRUE) {
    ri <- list.raster.results[index_order[13]]
    if(calculate.thresh==T){
      t_values[[13]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = 1.4,
          min.index = -1,
          method    = threshold.method
        )
    }
    if (is.na(t_values[[13]])==T){
      raster_index_cut[[13]] <- setValues(ri[[1]],  NA)
      message(paste("Can't calculate",index.[13], "authothreshold by", threshold.method, "method." ))
    }else{
    raster_index_cut[[13]] <- ri[[1]] <= t_values[[13]]
    }
  } 
  
  # ExGR autothreshold ----------------------------------------------
  if (any(index.==index_order[14])== TRUE) {
    ri <- list.raster.results[index_order[14]]
    if(calculate.thresh==T){
      t_values[[14]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = 3,
          min.index = -3.4,
          method    = threshold.method
        )
    }
    if (is.na(t_values[[14]])==T){
      # raster_index_cut[[14]] <- NA
      raster_index_cut[[14]] <- setValues(ri[[1]],  NA)
      message(paste("Can't calculate",index.[14], "authothreshold by", threshold.method, "method." ))
    }else{
    raster_index_cut[[14]] <- ri[[1]] <= t_values[[14]]
    }
  } 
  
  # CIVE autothreshold ----------------------------------------------
  if (any(index.==index_order[15])== TRUE) {
    ri <- list.raster.results[index_order[15]]
    if(calculate.thresh==T){
      t_values[[15]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = 19.61345,
          min.index = 17.97645,
          method    = threshold.method
        )
    }
    if (is.na(t_values[[15]])==T){
      raster_index_cut[[15]] <- setValues(ri[[1]],  NA)
      message(paste("Can't calculate",index.[15], "authothreshold by", threshold.method, "method." ))
    }else{
    raster_index_cut[[15]] <- ri[[1]] >= t_values[[15]]
    }
  }
  
  # VEG autothreshold -----------------------------------------------
  if (any(index.==index_order[16])== TRUE) {
    ri <- list.raster.results[index_order[16]]
    if(calculate.thresh==T){
      t_values[[16]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = max(values(ri[[1]]), na.rm = T),
          min.index = 0,
          method    = threshold.method
        )
    }
    if (is.na(t_values[[16]])==T){
      raster_index_cut[[16]] <- setValues(ri[[1]],  NA)
      message(paste("Can't calculate",index.[16], "authothreshold by", threshold.method, "method." ))
    }else{
    raster_index_cut[[16]] <- ri[[1]] <= t_values[[16]]
    }
  } 
 
  # HUE autothreshold -------------------------------------------------
  if (any(index.==index_order[17])== TRUE) {
    ri <- list.raster.results[index_order[17]]
    if(calculate.thresh==T){
      t_values[[17]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = 1,
          min.index = 0,
          method    = threshold.method
        )
    }
    if (is.na(t_values[[17]])==T){
      raster_index_cut[[17]] <- setValues(ri[[1]],  NA)
      message(paste("Can't calculate",index.[17], "authothreshold by", threshold.method, "method." ))
    }else{
    raster_index_cut[[17]] <- ri[[1]] <= t_values[[17]]
    }
  }

  # SAT autothreshold -------------------------------------------------
  if(any(index.==index_order[18])== TRUE) {
    ri <- list.raster.results[index_order[18]]
    if(calculate.thresh==T){
      t_values[[18]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = 1,
          min.index = 0,
          method    = threshold.method
        )
    }
    if (is.na(t_values[[18]])==T){
      raster_index_cut[[18]] <- setValues(ri[[1]], NA)
      message(paste("Can't calculate",index.[18], "authothreshold by", threshold.method, "method." ))
    }else{
    raster_index_cut[[18]] <- ri[[1]] >= t_values[[18]]
    }
  }

  # VAL autothreshold -------------------------------------------------
  if(any(index.==index_order[19])== TRUE) {
    ri <- list.raster.results[index_order[19]]
    if(calculate.thresh==T){
      t_values[[19]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = 1,
          min.index = 0,
          method    = threshold.method
        )
    }
    if (is.na(t_values[[19]])==T){
      raster_index_cut[[19]] <- setValues(ri[[1]],  NA)
      message(paste("Can't calculate",index.[19], "authothreshold by", threshold.method, "method." ))
    }else{
    raster_index_cut[[19]] <- ri[[1]] <= t_values[[19]]
    }
  }
 
  
  # Filter values that are not NA
  
t_values <- t_values[index_order%in%index.]
names(t_values) <- index_order[index_order%in%index.]
    
  # Filter rasters that are not NULL
  raster_index_cut <- raster_index_cut[index_order%in%index.]
  names(raster_index_cut) <- paste0(index_order[index_order%in%index.],  "_thresh_mask")
  
  
  out <- list(raster_index_cut, t_values)
  return(out)
}
  
  
