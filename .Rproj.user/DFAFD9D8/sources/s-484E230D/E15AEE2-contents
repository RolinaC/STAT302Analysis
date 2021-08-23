#' T-test Function
#'
#' This function run a one sample t-test.
#'
#' @param x A numeric vector of data.
#'   value.
#' @param alternative a character string specifying the alternative hypothesis.
#'   This should only accept `"two.sided"`, `"less"`, or `"greater"`. Otherwise,
#'   the function should throw an informative error.
#' @param mu A numeric indicating the null hypothesis value of the mean.
#'
#' @keywords Test hypothesis inference
#'
#' @return A list of numeric including the t-value, the degrees of freedom, the
#'   alternative, and the p-value.
#'
#' @examples
#' my_t_test(my_gapminder$lifeExp, "two.sided", 60)
#' my_t_test(my_gapminder$lifeExp, "less", 60)
#' my_t_test(my_gapminder$lifeExp, "greater", 60)
#'
#' @export
my_t_test <- function(x, alternative, mu) {
  # calculate t value
  test_stat <- (mean(x) - mu) / (sd(x) / sqrt(length(x)))
  # calculate degree of freedom
  df <- length(x) - 1
  # calculate p-value or one tail that could be less or greater
  if (alternative == "two.sided") {
    # two sides
    p_val <- pt(abs(test_stat), df, lower.tail = FALSE)
    p_val <- p_val * 2
  } else if (alternative == "less") {
    # lower tail
    p_val <- pt(test_stat, df, lower.tail = TRUE)
  } else if (alternative == "greater") {
    # upper tail
    p_val <- pt(abs(test_stat), df, lower.tail = FALSE)
  } else {
    # error when input is not 'two.sided', 'less', or 'greater'
    stop("alternative should only accept 'two.sided', 'less', or 'greater'!")
  }
  # create a list
  result <- list(test_stat, df, alternative, p_val)
  return(result)
}
