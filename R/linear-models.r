
#' Fit a linear model
#'
#' @description This function passes parameters to the lm function.
#' @param formula a formula
#' @param data a data.frame
#' @return An lm object
#' @importFrom stats model.matrix
#' @export
#load("/Users/shawnieshan/Desktop/BIS557/bis557/data/lm_patho.rda")
#lmod<-lm(lm_patho$y~.,data=lm_patho)
#summary(lmod)

linear_model <- function(formula, data) {
  #browser()
  all<-all.vars(formula)
  x<-model.matrix(formula, data)
  y<-data[,all[1]]
  #svd_output <- svd(x)
  #U <- svd_output[["u"]]
  #Sinv <- diag(1 / svd_output[["d"]])
  #V <- svd_output[["v"]]
  #pseudo_inv <- V %*% Sinv %*% t(U)
  #betahat <- pseudo_inv %*% y
  #colnames(betahat) <- "regression coefficient"
  #rownames(betahat) <- c("Sepal.Width","Patal.Length","Petal.Width","Species_versicolor","Species_virginica")
  #print(betahat)
#lm(formula, data)
  beta<-list()
  #beta$call=call("linear_model", formula)
  beta$coefficients=qr.coef(qr(x),y)
  class(beta)="lm"
  return(beta)
}
#linear_model(y~.,data=lm_patho)

#test
#setwd("/Users/shawnieshan/Desktop/BIS557/bis557")

#library(devotions)
#check()
#library(testthat)
#test()















