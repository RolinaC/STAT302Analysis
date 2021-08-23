#' Linear Regression Function
#'
#' This function fits linear regression model.
#'
#' @param formula A formula class object to fit \code{data}.
#' @param data Numeric data frame to fit \code{formula}.
#' @keywords inference
#'
#' @return A table of linear model parameter of \code{formula} including:
#'   \code{data} with \code{Estimate}, \code{Std. Error},
#'   \code{t value}, and \code{Pr(>|t|)}.
#'
#' @examples
#' my_lm(mpg ~ hp + wt, data = mtcars)
#'
#' @export
my_lm <- function(formula, data) {
  # extracr the model matrix
  X <- model.matrix(formula, data)
  # extract the model response
  Y <- model.response(model.frame(formula, data))
  # beta_hat
  beta_hat <- solve((t(X) %*% X)) %*% t(X) %*% Y
  # calculate the degree of freedom
  df <- nrow(X) - ncol(X)
  # estimate sigma_sqrt_hat
  sigma_sqrt_hat <- sum((Y - (X %*% beta_hat)) ^ 2 / df)
  # estimate standard error
  se <- abs(diag(sqrt(sigma_sqrt_hat * solve(t(X) %*% X))))
  # t value
  test_stat <- beta_hat / se
  # p-value
  p_val <- 2 * pt(abs(test_stat), df, lower.tail = FALSE)
  # create a result dataframe
  result <- as.table(cbind(beta_hat, se, test_stat, p_val))
  colnames(result) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  return(result)
}
