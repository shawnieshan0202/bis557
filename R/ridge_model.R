#' Fit a ridge regression model
#'
#' @description This function passes parameters to the ridge regression function.
#' @param form a formula
#' @param lambda a hyper parameter
#' @param d a data.frame
#' @return An ridge regression object
#' @export

#read data files
#ridge_train<-read.csv("/Users/shawnieshan/Desktop/ridge_train.csv")
#ridge_test<-read.csv("/Users/shawnieshan/Desktop/ridge_test.csv")

#library(MASS)
#library(faraway)
#library(stats)

#build ridge function
ridge_reg<-function(form, lambda, d){
  rownames(data) = NULL
  m<-model.matrix(form, d)
  y<-matrix(d[,as.character(form)[2]],ncol=1)
  y<-y[as.numeric(rownames(m)),,drop=FALSE]
  
#Fit via svd
svd_obj<-svd(m)
U<-svd_obj$u
V<-svd_obj$v
svals<-svd_obj$d

D<-diag(svals/(svals^2 +lambda))
beta<-V %*% D %*% t(U) %*% y
rownames(beta) = colnames(m)
ret<- list(coefficients = beta, lambda=lambda,form=form)
class(ret)<-"ridge_reg"
ret
}#function end


