#' Prediction from MKL model
#'
#' This function creates gramm matrix for traning set baed upon several types of kernel
#' and specified hyper paremeters. Matrix corresponds to similarity betwween 
#' each sample in the training set.
#' @param ktest Gramm matrix of training data and test data
#' @param model MKL model 
#' @param train.outcome Outcome for the training data
#' @return yhat Predicted value for each test point
#' @return predicted Sign of yhat, which is the final predicted outcome 
#' @export

prediction.Regression=function(model,ktest,outcome){
  product=list()
  product=lapply(1:length(ktest), function(a) ktest[[a]]*model$gamma[a])
  fushion=Reduce('+',product)
  yhat=(model$alpha-model$alpha.star)%*%t(fushion)-model$b
  return(yhat)
}
