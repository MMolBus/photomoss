#' @title Compute Descriptive Statistics
#' @description This internal function computes various descriptive statistics 
#' for a given numeric vector based on the requested descriptors.
#' 
#' @param x A numeric vector.
#' @param des A character vector specifying which descriptive statistics to 
#' compute.
#' Available options: 
#' `"median"`, `"mean"`, `"sd"`, `"min"`, `"max"`, and `"diff.range"`.
#' 
#' @return A named numeric vector containing the computed statistics.
#' 
#' @keywords internal

descriptor.fun <-
      function(x, des) {
      # Define possible descriptive statistics that can be computed
            possible_descrip <-
                  c("median", "mean", "sd", "min", "max", "diff.range")
            
      # Initialize an empty list to store results
            res_l <- list()
            
      # Store the count of non-missing values in x as the first element
            res_l[[1]] <- length(x[!is.na(x)])
            
      # Check if "median" is requested in des and compute the median if true
            if (any(unique(grepl(possible_descrip[1], des))) == TRUE) {
                  res_l[[2]] <- median(x)
            } else{
                  res_l[[2]] <- NULL
            }
            
      # Check if "mean" is requested in des and compute the mean if true
            if (any(unique(grepl(possible_descrip[2], des))) == TRUE) {
                  res_l[[3]] <- mean(x)
            } else{
                  res_l[[3]] <- NULL
            }
            
      # Check if "sd" is requested in des and compute the standard deviation 
      # if true
            if (any(unique(grepl(possible_descrip[3], des))) == TRUE) {
                  res_l[[4]] <- sd(x)
            } else{
                  res_l[[4]] <- NULL
            }
            
      # Check if "min" is requested in des and compute the minimal value if true
            if (any(unique(grepl(possible_descrip[4], des))) == TRUE) {
                  res_l[[5]] <- min(x)
            } else{
                  res_l[[5]] <- NULL
            }
            
      # Check if "max" is requested in des and compute the maximal value if true
            if (any(unique(grepl(possible_descrip[5], des))) == TRUE) {
                  res_l[[6]] <- max(x)
            } else{
                  res_l[[6]] <- NULL
            }
            
      # Check if "diff.range" (range difference) is requested in des and compute
      # if true
            if (any(unique(grepl(possible_descrip[6], des))) == TRUE) {
                  res_l[[7]] <- diff(range(x))
            } else{
                  res_l[[7]] <- NULL
            }
            
            # Remove NULL values from the result list
            res_l <- 
                  plyr::compact(res_l)
            
            # Assign names to the result elements
            names(res_l) <- c("ncell", des)
            
            # Convert the list to a vector for output
            res <- unlist(res_l)
            
            # Return the computed statistics
            return(res)
            
      }
