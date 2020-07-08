# list.raster.results = list_raster_results
# threshold.method = threshold.method


calculate.raster.thresh.fun <- function(list.raster.results, 
                                 calculate.thresh, 
                                 threshold.vector,
                                 threshold.method){
  
  index.      <- names(list.raster.results)
  
  index_order <- c("NDVI", "SR", "MSAVI", "EVI", "CI",  "BSCI", "BI", "NORR", "NORG", 
                   "NORB", "EXR", "EXG", "EXB", "EXGR","CIVE", "VEG", "HUE", 
                   "SAT" , "VAL")
  
  if(calculate.thresh==T){
    t_values <- list()
  }else{
    t_values <-  rep(NA, length(index_order))
    t_values[grepl(paste(c(index.,"00"), collapse ="|"), index_order)] <- threshold.vector
    t_values <- as.list(t_values)
  }
  
  raster_index_cut <- list()
  
  # NDVI autothres ------------------------------------------------------------
  if (any(unique(grepl(index_order[1], index.))) == TRUE) { 
    ri <- list.raster.results[grepl(index_order[1], index.)][[1]]
    if(calculate.thresh==T){
      t_values[[1]] <-  
        autothreshold.value(
          raster = ri,
          max.index = 1,
          min.index = -1,
          method    = threshold.method
        )
    }
    raster_index_cut[[1]]  <- ri >= t_values[[1]]
    
  }else{
    t_values[[1]] <- NULL
    raster_index_cut[[1]] <- NULL
  }
  
  # SR autothres -----------------------------------------------------------
  if (any(unique(grepl(index_order[2], index.))) == TRUE) {
    ri <- list.raster.results[grepl(index_order[2], index.)][[1]]
    if(calculate.thresh==T){
      t_values[[2]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = max(getValues(ri), na.rm = T),
          min.index = 0,
          method    = threshold.method
        )
    }
    raster_index_cut[[2]]  <- as.matrix(ri >= t_values[[2]]
  }else{
    t_values[[2]]<- NULL
    raster_index_cut[[2]] <- NULL
  }
  
  # MSAVI autothreshold -----------------------------------------------------
  if (any(unique(grepl(index_order[3], index.))) == TRUE) {
    ri <- list.raster.results[grepl(index_order[3], index.)][[1]]
    if(calculate.thresh==T){
      t_values[[3]]  <- 
        autothreshold.value(
          raster    = ri,
          max.index = 1,
          min.index = -1,
          method    = threshold.method
        )
    }
    raster_index_cut[[3]]  <- as.matrix(ri >= t_values[[3]]
  }else{
    t_values[[3]] <- NULL
    raster_index_cut[[3]] <- NULL
  }
  
  # EVI autothreshold -------------------------------------------------------
  if (any(unique(grepl(index_order[4], index.))) == TRUE) {
    ri <- list.raster.results[grepl(index_order[4], index.)][[1]]
    if(calculate.thresh==T){
      t_values[[4]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = 1,
          min.index = -1,
          method    = threshold.method
        )
    }
    raster_index_cut[[4]]  <- as.matrix(ri >= t_values[[4]]
  }else{
    t_values[[4]] <- NULL
    raster_index_cut[[4]]  <- NULL
  }
  
  # CI autothreshold -------------------------------------------------------
  if (any(unique(grepl(index_order[5], index.))) == TRUE) {
    ri <- list.raster.results[grepl(index_order[5], index.)][[1]]
    
    if(calculate.thresh==T){
      t_values[[5]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = 2,
          min.index = 0,
          method    = threshold.method
        )
    }
    raster_index_cut[[5]]  <- ri <= t_values[[5]]
  }else{
    t_values[[5]]<- NULL
    raster_index_cut[[5]] <- NULL
  }
  # BSCI autothreshold ----------------------------------------------------
  if (any(unique(grepl(index_order[6], index.))) == TRUE) {
    ri <- list.raster.results[grepl(index_order[6], index.)][[1]]
    if(calculate.thresh==T){
      t_values[[6]] <- 
        autothreshold.value(
          raster  = ri,
          max.index = max(getValues(ri), na.rm = T),
          min.index = 0,
          method    = threshold.method
        )
    }
    raster_index_cut[[6]]  <- ri >= t_values[[6]]
  }else{
    t_values[[6]]<- NULL
    raster_index_cut[[6]]  <- NULL
  }
  # BI autothreshold ---------------------------------------------------
  if (any(unique(grepl(index_order[7], index.))) == TRUE) {
    ri <- list.raster.results[grepl(index_order[7], index.)][[1]]
    if(calculate.thresh==T){
      t_values[[7]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = 2,
          min.index = 0,
          method    = threshold.method
        )
    }
    raster_index_cut[[7]]  <- ri <= t_values[[7]]
  }else{
    t_values[[7]]<- NULL
    raster_index_cut[[7]] <- NULL
  }
  # NorR autothreshold -------------------------------------------------
  if (any(unique(grepl(index_order[8], index.))) == TRUE) {
    ri <- list.raster.results[grepl(index_order[8], index.)][[1]]
    if(calculate.thresh==T){
      t_values[[8]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = 1,
          min.index = 0,
          method    = threshold.method
        )
    }
    raster_index_cut[[8]]  <- ri >= t_values[[8]]
  }else{
    t_values[[8]]<- NULL
    raster_index_cut[[8]] <- NULL
  }
  # NorG autothreshold ------------------------------------------------
  if (any(unique(grepl(index_order[9], index.))) == TRUE) {
    ri <- list.raster.results[grepl(index_order[9], index.)][[1]]
    
    if(calculate.thresh==T){
      t_values[[9]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = 1,
          min.index = 0,
          method    = threshold.method
        )
    }
    raster_index_cut[[9]]  <- ri <= t_values[[9]]
  }else{
    t_values[[9]] <- NULL
    raster_index_cut[[9]] <- NULL
  }
  # NorB autothreshold -----------------------------------------------
  if (any(unique(grepl(index_order[10], index.))) == TRUE) {
    ri <- list.raster.results[grepl(index_order[10], index.)][[1]]
    
    if(calculate.thresh==T){
      t_values[[10]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = 1,
          min.index = 0,
          method    = threshold.method
        )
    }
    raster_index_cut[[10]] <- ri <= t_values[[10]]
  }else{
    t_values[[10]]<- NULL
    raster_index_cut[[10]] <- NULL
  }
  # ExR autothreshold ------------------------------------------------
  if (any(unique(grepl(index_order[11], index.))) == TRUE) {
    ri <- list.raster.results[grepl(index_order[11], index.)][[1]]
    if(calculate.thresh==T){
      t_values[[11]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = 1.4,
          min.index = -1,
          method    = threshold.method
        )
    }
    raster_index_cut[[11]] <- ri >= t_values[[11]]
  }else{
    t_values[[11]] <- NULL
    raster_index_cut[[11]] <- NULL
  }
  # ExG autothreshold -----------------------------------------------
  if (any(unique(grepl(index_order[12], index.))) == TRUE) {
    ri <- list.raster.results[grepl(index_order[12], index.)][[1]]
    if(calculate.thresh==T){
      t_values[[12]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = 2,
          min.index = -2,
          method    = threshold.method
        )
    }
    raster_index_cut[[12]] <- ri <= t_values[[12]]
  } else{
    t_values[[12]]<- NULL
    raster_index_cut[[12]] <- NULL
  }
  # ExB autothreshold -----------------------------------------------
  if (any(unique(grepl(index_order[13], index.))) == TRUE) {
    ri <- list.raster.results[grepl(index_order[13], index.)][[1]]
    if(calculate.thresh==T){
      t_values[[13]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = 1.4,
          min.index = -1,
          method    = threshold.method
        )
    }
    raster_index_cut[[13]] <- ri <= t_values[[13]]
  } else{
    t_values[[13]]<- NULL
    raster_index_cut[[13]] <- NULL
  }
  # ExGR autothreshold ----------------------------------------------
  if (any(unique(grepl(index_order[14], index.))) == TRUE) {
    ri <- list.raster.results[grepl(index_order[14], index.)][[1]]
    if(calculate.thresh==T){
      t_values[[14]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = 3,
          min.index = -3.4,
          method    = threshold.method
        )
    }
    raster_index_cut[[14]] <- ri <= t_values[[14]]
  } else{
    t_values[[14]] <- NULL
    raster_index_cut[[14]] <- NULL
  }
  # CIVE autothreshold ----------------------------------------------
  if (any(unique(grepl(index_order[15], index.))) == TRUE) {
    ri <- list.raster.results[grepl(index_order[15], index.)][[1]]
    if(calculate.thresh==T){
      t_values[[15]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = 19.61345,
          min.index = 17.97645,
          method    = threshold.method
        )
    }
    raster_index_cut[[15]] <- ri >= t_values[[15]]
  }else{
    t_values[[15]] <- NULL
    raster_index_cut[[15]] <- NULL
  }
  # VEG autothreshold -----------------------------------------------
  if (any(unique(grepl(index_order[16], index.))) == TRUE) {
    ri <- list.raster.results[grepl(index_order[16], index.)][[1]]
    if(calculate.thresh==T){
      t_values[[16]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = max(values(ri), na.rm = T),
          min.index = 0,
          method    = threshold.method
        )
    }
    raster_index_cut[[16]] <- ri <= t_values[[16]]
  } else{
    t_values[[16]] <- NULL
    raster_index_cut[[16]] <- NULL
  }
  # HUE autothreshold -------------------------------------------------
  if (any(unique(grepl(index_order[17], index.))) == TRUE) {
    ri <- list.raster.results[grepl(index_order[17], index.)][[1]]
    if(calculate.thresh==T){
      t_values[[17]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = 1,
          min.index = 0,
          method    = threshold.method
        )
    }
    raster_index_cut[[17]] <- ri <= t_values[[17]]
  }else{
    t_values[[17]] <- NULL
    raster_index_cut[[17]] <- NULL
  }
  # SAT autothreshold -------------------------------------------------
  if(any(unique(grepl(index_order[18], index.))) == TRUE) {
    ri <- list.raster.results[grepl(index_order[18], index.)][[1]]
    if(calculate.thresh==T){
      t_values[[18]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = 1,
          min.index = 0,
          method    = threshold.method
        )
    }
    raster_index_cut[[18]] <- ri >= t_values[[18]]
  }else{
    t_values[[18]] <- NULL
    raster_index_cut[[18]] <- NULL
  }
  # VAL autothreshold -------------------------------------------------
  if(any(unique(grepl(index_order[19], index.))) == TRUE) {
    ri <- list.raster.results[grepl(index_order[19], index.)][[1]]
    if(calculate.thresh==T){
      t_values[[19]] <- 
        autothreshold.value(
          raster    = ri,
          max.index = 1,
          min.index = 0,
          method    = threshold.method
        )
    }
    raster_index_cut[[19]] <- ri <= t_values[[19]]
  }else{
    t_values[[19]] <- NULL
    raster_index_cut[[19]] <- NULL
  }
  
  # Filter values that are not NA
  # t_values <- as.list(t_values[is.null(t_values)==FALSE])
  # t_values <- plyr::compact(t_values)
  # t_values <- do.call(c,t_values)
  
  names(t_values) <- paste0(index.,"_thresh_value")
  
  # Filter rasters that are not NULL
  raster_index_cut <- plyr::compact(raster_index_cut)
  # raster_index_cut[names(raster_index_cut)
  #                     [lapply(raster_index_cut, length) != 0]]
  
  # 
  # raster_index_cut <- lapply(c(1:length(raster_index_cut)), function(i)
  #  extract(raster_index_cut[[i]], 1:ncell( raster_index_cut[[i]])))
  # 


  names(raster_index_cut) <- paste0(index., "_thresh_mask")
  
  
  out <- list(raster_index_cut, t_values)
  return(out)
}
  
  
