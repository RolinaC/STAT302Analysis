#' Random Forest Cross-Validation Function
#'
#' This function performs Cross-Validation Random Forest Cross-Validation.
#'
#' @param k Integer representing the number of folds.
#' @keywords inference, prediction
#' @importFrom stats model.frame model.matrix model.response predict pt sd
#'  na.omit
#' @return A numeric with the cross-validation error.
#'
#' @examples
#' my_rf_cv(5)
#'
#' @export
my_rf_cv <- function(k) {
  # remove na from the data
  data_cleaned <- na.omit(my_penguins)
  # create train data
  data_train <- data_cleaned[, c("bill_length_mm", "bill_depth_mm",
                                 "flipper_length_mm", "body_mass_g")]
  # define folds
  fold <- sample(rep(1:k, length = nrow(data_cleaned)))
  mse <- c()
  for (i in 1:k) {
    training <- dplyr::filter(data_cleaned, fold != i)
    testing <- dplyr::filter(data_cleaned, fold == i)
    rf_m <- randomForest::randomForest(body_mass_g ~ bill_length_mm +
                                         bill_depth_mm + flipper_length_mm,
                                       data = training, ntree = 100)
    pred <- stats::predict(rf_m, testing[, -1])
    mse[i] <- mean((testing$body_mass_g - pred)^2)
  }
  return(mean(mse))
}
utils::globalVariables(c("bill_length_mm", "bill_depth_mm", "body_mass_g", "flipper_length_mm"))
