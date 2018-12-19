#3. Consider the sparse matrix implementation from class and the sparse add function:

#Implement a `sparse_multiply` function that multiplies two sparse matrices.
#Create a new class `sparse.matrix` that has add `+`, multiply `%*%`, and transpose `t()` methods.
#Add test-sparse-matrix.r to the testthat directory of your bis557 package to show it works.

#' Sparse matrix and its operations
#'
#' @description create a new class sparse.matrix consists of add, multiply and transpose
#' @param i row index of a non-zero element
#' @param j col index of a non-zero element
#' @param x value of the non-zero element
#' @param dims dimensions of the sparse matrix
#' @return A sparse.matrix object
#' @export
sparse.matrix <- function(i, j, x, dims = c(max(i), max(j))){
  
  #if (length(i) != length(j) || length(j) != length(x))
  #stop("Incorrect dimensions.")
  
  structure(list(data.frame(i = c(1, 2), j = c(1, 1), x = c(3, 1)), dims), class = "sparse.matrix")
}

#' add
#'
#' @description This function is used to plus two sparse matrices stored as triplet lists.
#' @param a A list describing a sparse matrix.
#' @param b A list describing a sparse matrix.
#' @reture A list describing the sum of a and b as a sparse matrix.
#' @examples
#' a <- sparse.matrix(i = c(1, 2), j = c(1, 1), x = c(3, 1))
#' b <- sparse.matrix(i = c(1, 2, 2), j = c(1, 1, 2), x = c(4.4, 1.2, 3))
#' sparse_add <- function(a,b)
#' @export

`+.sparse.matrix` <- function(a, b)
{
  if (!inherits(a, "sparse.matrix"))
    stop ("a is not a sparse.matrix.")
  if (!inherits(b, "sparse.matrix"))
    stop ("b is not a sparse.matrix.")
  if (!identical(a[[2]], b[[2]]))
    stop("incorrect dimension")
  
  c <- merge(a[[1]], b[[1]], by = c("i", "j"), all = TRUE, suffixes = c("1", "2"))
  c$x1[is.na(c$x1)] <- 0
  c$x2[is.na(c$x2)] <- 0
  c$x <- c$x1 + c$x2
  c[, c("i", "j", "x")]
  sparse.matrix(c$i, c$j, c$x, dims = a[[2]])
}

#' Multiply-textbook p294-295
#' 
#' @description This function is used to plus two sparse matrices stored as triplet lists.
#' @param a A list describing a sparse matrix.
#' @param b A list describing a sparse matrix.
#' @reture A list describing the sum of a and b as a sparse matrix.
#' @examples
#' a <- sparse.matrix(i = c(1, 2), j = c(1, 1), x = c(3, 1))
#' b <- sparse.matrix(i = c(1, 2, 2), j = c(1, 1, 2), x = c(4.4, 1.2, 3))
#' sparse_multiply <- function(a,b)
#' @export

`%*%.sparse.matrix` <- function(a, b)
{
  if (!inherits(a, "sparse.matrix"))
    stop ("a is not a sparse.matrix.")
  if (!inherits(b, "sparse.matrix"))
    stop ("b is not a sparse.matrix.")
  if ((a[[2]][2] != b[[2]][1]))
    stop("incorrect dimension")
  
  colnames(b[[1]]) <- c("i2", "j2", "x2")
  c <- merge(a[[1]], b[[1]], by.x = "j", by.y = "i2",
             all = FALSE, suffixes = c("1", "2"))
  c$x <- c$x * c$x2
  c$key <- paste(c$i, c$j, sep = "-")
  x <- tapply(c$x, c$key, sum)
  key <- strsplit(names(x), "-")
  d <- data.frame(i = sapply(key, getElement, 1),
                  j = sapply(key, getElement, 2),
                  x = as.numeric(x))
  sparse.matrix(c$i, c$j, c$x, dims = c(a[[2]][1], b[[2]][2]))
}

#' Transpose
#' 
#' @description This function is used to plus two sparse matrices stored as triplet lists.
#' @param a A list describing a sparse matrix.
#' @reture A list describing the sum of a and b as a sparse matrix.
#' @examples
#' a <- sparse.matrix(i = c(1, 2), j = c(1, 1), x = c(3, 1))
#' sparse_transpose <- function(a,b)
#' @export


`t.sparse.matrix` <- function(a)
{
  c <- a[[1]]$i
  a[[1]]$i <- a[[1]]$j
  a[[1]]$j <- c
  a[[2]] <- rev(a[[2]])
  return(a)
}

`%*%.default` = .Primitive("%*%")
`%*%` = function(x,...){ 
  UseMethod("%*%",x)
}
`%*%` <- function(x, y) {
  UseMethod("%*%", x)
}

t <- function (x, ...) {
  UseMethod("t", x)
}