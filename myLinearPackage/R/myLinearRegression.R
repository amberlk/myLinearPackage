#' Creates a scatterplot of each pair of covariates.
#' Runs a linear regression of y on x.
#'
#' @param X A matrix of covariates.
#' @param Y A vector of outcomes.
#' @param sub a list of subjects (i.e. a set of integers corresponding to rows in X)
#' @return p: a plot of the linear models
#' @return coef: a set of coefficients from the linear regression of Y on X
#' @return pvals: the set of corrresponding p-values
#' Imports: ggplot2
#' @export
#' @examples
#' myData<- read.csv("myData.csv", header = TRUE)
#' y <- myData[,1]
#' x <- data.matrix(myData[,-1])
#' myLinearRegression(x[,-1], y, c(1:5))


#load in the packages used
library(GGally)

#create the function myLinearRegression

myLinearRegression <- function(x, y, sub) {
  if (ncol(x) < 5){
    if (sub == 0){
      data.1 <- cbind.data.frame(x,y)
      model1 <- lm(y ~ ., data = data.1)
      p1 <- GGally::ggpairs(data.1)
      coef <- coefficients(model1)
      pvals <- summary(model1)$coefficients[,4]
      return(list(p1, coef, pvals))
    }
    else{
      data.2 <- cbind.data.frame(x[sub,],y)
      model2 <- lm(y ~ ., data = data.2)
      p2 <- GGally::ggpairs(data.2)
      coef <- coefficients(model2)
      pvals <- summary(model2)$coefficients[,4]
      return(list(p2, coef, pvals))
    }
  }
  else {
    print("Too many variables to plot")
  }}

