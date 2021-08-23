#' k-Nearest Neighbors Cross-Validation Function
#'
#' This function perform Cross-Validation k-Nearest Neighbors Cross-Validation.
#'
#' @param train The training input data frame.
#' @param cl True class value of \code{train}.
#' @param k_nn Integer representing the number of neighbors.
#' @param k_cv Integer representing the number of folds.
#' @keywords prediction
#'
#' @return A list of predicted \code{class} and a numeric with the
#'   cross-validation misclassification error \code{cv-err}.
#'
#' @examples
#' data_cleaned <- na.omit(my_penguins)
#'
#' data_train <- data_cleaned[, c("bill_length_mm", "bill_depth_mm",
#'   "flipper_length_mm", "body_mass_g")]
#'   my_knn_cv(data_train, data_cleaned$species, 1, 5)
#'   my_knn_cv(data_train, data_cleaned$species, 5, 5)
#'
#' @export

my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # fold into k_cv
  fold <- base::sample(base::rep(1:k_cv, length = nrow(train)))
  
  # store prediction
  pred_class <- base::rep(NA, length(cl))
  #store the missclassification rate
  cv_err <- base::rep(NA, k_cv)
  
  for (i in 1:k_cv) {
    
    fold_i <- base::which(fold == i)
    
    # split train into train and test
    train_train <- train[-fold_i,]
    train_test <- train[fold_i,]
    
    # split cl into train and test
    cl_train <- cl[-fold_i]
    cl_test <- cl[fold_i]
    
    # predict K-nearest
    result <-  class::knn(train = train_train, test = train_test,
                          cl = cl_train, k = k_nn)
    # compute missclassification rate
    cv_err[i] <- mean(result != cl_test)
  }
  
  # prediction output class
  class <- class::knn(train = train, test = train, cl = cl, k = k_nn)
  
  # store class and cv_error as a list
  cv_err <- sum(cv_err) / k_cv
  result <- list("class" = class, "cv_error" = cv_err)
  
  return(result)
}
