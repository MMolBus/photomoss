# TSS and IoU calculator
# list.result <- list.results[[1]][[i]]
TSS.IoU.calc <-
  function(list.result) {
    
    # TSS calculation
    sensitivity <-
      sum(list.result$TP) / (sum(list.result$TP) + sum(list.result$FN))
    
    specificity <-
      sum(list.result$TN) / (sum(list.result$TN) + sum(list.result$FP))
    
    TSS <-
      sensitivity + specificity - 1
    
    # IoU calculation
    IoU <-
      sum(list.result$TP) / (sum(list.result$TP) + sum(list.result$FP) + sum(list.result$FN))
  
    TSS_IoU <- c(TSS, IoU)
    names(TSS_IoU) <-  c("TSS", "IoU")
    return(TSS_IoU)
    }
