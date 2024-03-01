GiniV2 <- function(Target,Prediction,Weights=NULL,Normalise=FALSE){
  GiniInternal <- function(Actual,Pred,Weights){
    if(length(Actual)!=length(Pred)) stop("Target and Prediction Vectors do not have equal length")
    tmp <- data.frame(Actual=Actual,Pred=Pred,Range=c(1:length(Actual)))
    tmp <- tmp[order(-tmp$Pred,tmp$Range),]
    if(is.null(Weights)){
      population.delta <- 1/length(Actual)
      total.act <- sum(Actual)
      null.act <- rep(population.delta,length(Actual))
      accum.act <- tmp$Actual/total.act
      gini.sum <- cumsum(accum.act-null.act)
      sum(gini.sum)/length(Actual)
    }
  }
  result <- GiniInternal(Target,Prediction,Weights)
  result <- ifelse(Normalise,result/GiniInternal(Target,Target,Weights),result)
  return(result)
}

RMSE <- function(Target,Prediction){
  se <- (Target-Prediction)^2
  mse <- mean(se)
  rmse <- sqrt(mse)
  return(rmse)
}

MAE <- function(Target,Prediction){
  ae <- abs(Target-Prediction)
  mae <- mean(ae)
  return(mae)
}