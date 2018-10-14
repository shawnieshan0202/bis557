#' Fit a ridge regression model
#'
#' @description This function passes parameters to the ridge regression function.
#' @param formula a formula
#' @param data a data.frame
#' @return An ridge regression object
#' @importFrom stats model.matrix
#' @export

#read data files
#ridge_train<-read.csv("/Users/shawnieshan/Desktop/ridge_train.csv")
#ridge_test<-read.csv("/Users/shawnieshan/Desktop/ridge_test.csv")

library(MASS)
#library(faraway)
library(stats)

#build ridge function
ridge_reg<-function(formula, data, lambda){
  m<-model.matrix(formula, data)
  y<-matrix(data[,as.character(formula)[2]],ncol=1)
  y<-y[as.numeric(rownames(m)),,drop=FALSE]
  
#Fit via svd
svd_obj<-svd(m)
U<-svd_obj$u
V<-svd_obj$v
svals<-svd_obj$d

D<-diag(svals/(svals^2 +lambda))
beta<-V %*% D %*% t(U) %*% y
ret<- list(coefficients = beta, formula=formula, lambda=lambda)
class(ret)<-"ridge_reg"
return(ret)

}#function end


