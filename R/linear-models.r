
#' Fit a linear model
#'
#' @description This function passes parameters to the lm function.
#' @param formula a formula
#' @param data a data.frame
#' @return An lm object
#' @importFrom stats lm
#' @examples
#' fit <- linear_model(Sepal.Length ~., iris)
#' summary(fit)
#' @export
data(iris)
View(data)
linear_model <- function(formula, data) {
  #browser()
  mm<-model.matrix(formula, data)
  y<-data$Sepal.Length[as.integer(row.names(mm))]
  x<-mm[,2:6]
  svd_output <- svd(x)
  U <- svd_output[["u"]]
  Sinv <- diag(1 / svd_output[["d"]])
  V <- svd_output[["v"]]
  pseudo_inv <- V %*% Sinv %*% t(U)
  betahat <- pseudo_inv %*% y
  colnames(betahat) <- "regression coefficient"
  rownames(betahat) <- c("Sepal.Width","Patal.Length","Petal.Width","Species_versicolor","Species_virginica")
  print(betahat)
#lm(formula, data)
}
