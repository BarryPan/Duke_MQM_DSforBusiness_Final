##
## FPR_TPR
FPR_TPR <- function(prediction, actual){
  
 
  TP <- sum((prediction)*(actual))
  FP <- sum((prediction)*(!actual))
  FN <- sum((!prediction)*(actual))
  TN <- sum((!prediction)*(!actual))
  result.FPR <- FP / (FP + TN)
  result.TPR <- TP / (TP + FN)
  result = c(result.FPR, result.TPR, TP, FP, FN, TN)
  return(result)
}

