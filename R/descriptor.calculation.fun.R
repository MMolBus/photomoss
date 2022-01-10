# x = numeric vector object 
# descriptores.= character vector with descriptors that you whant to calculate over vector x
# "median", "mean", "sd", "min", "max", "diff.range"

descriptor.fun <- function(x, des) {
  
  possible_descrip <- c("median", "mean","sd", "min", "max", "diff.range"
                        # "n.cell"
                        )
  
  res_l <- list()
  
  res_l[[1]]       <- length(x)
   
  
  if(any(unique(grepl(possible_descrip[1], des)))== TRUE){
    res_l[[2]]       <- median(x)
    }else{res_l[[2]] <- NULL}
  
  if(any(unique(grepl(possible_descrip[2], des)))== TRUE){
    res_l[[3]]       <- mean(x)
    }else{res_l[[3]] <- NULL}
  
  if(any(unique(grepl(possible_descrip[3], des)))== TRUE){
    res_l[[4]]       <- sd(x)
    }else{res_l[[4]] <- NULL}
  
  if(any(unique(grepl(possible_descrip[4], des)))== TRUE){
    res_l[[5]]       <- min(x)
    }else{res_l[[5]] <- NULL}
  
  if(any(unique(grepl(possible_descrip[5], des)))== TRUE){
    res_l[[6]]       <- max(x)
    }else{res_l[[6]] <- NULL}
  
  if(any(unique(grepl(possible_descrip[6], des)))== TRUE){
    res_l[[7]]       <- diff(range(x))
    }else{res_l[[7]] <- NULL}
  
  res_l        <- plyr::compact(res_l)
  names(res_l) <- c("ncell",des)
  
  res          <- unlist(res_l)
  return(res)
  
}
