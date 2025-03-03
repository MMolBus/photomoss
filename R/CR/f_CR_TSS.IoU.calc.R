#'  @title TSS and IoU Calculator
#'
#' @description Computes the True Skill Statistics (TSS) and Intersection over 
#' Union (IoU).
#'
#' - True Skill Statistics (TSS) is derived from sensitivity and specificity, 
#'   calculated using presence and absence records, and is used for assessing 
#'   model performance.
#'
#' - Intersection over Union (IoU) is commonly used in image segmentation 
#'   performance assessment by comparing the ground truth polygon with the 
#'   predicted polygon.
#'
#' @param df.result A data frame where each row represents a pixel classified as
#' 0 (absence) or 1 (presence) by the classification model and the ground truth.
#' The data frame contains the following columns:  
#'   - `TP`: True Positives  
#'   - `FP`: False Positives  
#'   - `TN`: True Negatives  
#'   - `FN`: False Negatives  
#'   
#' @return A named numeric vector with two elements:
#'   - `"TSS"`: The computed True Skill Statistics value.
#'   - `"IoU"`: The computed Intersection over Union value.
#'
#' @keywords internal
TSS.IoU.calc <- 
      function(df.result) {
      # TSS calculation
      ## Calculate sensitivity (True Positive Rate)
      sensitivity <- 
            sum(df.result$TP) / 
            (sum(df.result$TP) + sum(df.result$FN))
      
      ## Calculate specificity (True Negative Rate)
      specificity <- 
            sum(df.result$TN) / 
            (sum(df.result$TN) + sum(df.result$FP))
      
      ## Compute True Skill Statistics (TSS)
      TSS <- 
            sensitivity + specificity - 1
      
      # IoU calculation
      IoU <- 
            sum(df.result$TP) / 
            (sum(df.result$TP) + sum(df.result$FP) + sum(df.result$FN))
      
      ## Store TSS and IoU in a named vector
      TSS_IoU <- 
            c(TSS, IoU)
      
      names(TSS_IoU) <- 
            c("TSS", "IoU")
      
      ## Return the computed TSS and IoU values
      return(TSS_IoU)
}
