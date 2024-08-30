#' @title Perform Group-wise Statistical Tests
#' @description
#' The `matrix_test` function performs statistical tests between groups in a data frame.
#' It can conduct either chi-squared tests (if `chisq_test = TRUE`) or other statistical tests
#' provided by the user through the `test` parameter (default is `auto_test`).
#'
#' @param dat A data frame containing the data to be tested.
#' @param group A character string specifying the name of the column in `dat` that contains the group labels. Default is "Group".
#' @param chisq_test Logical; if \code{TRUE}, the function performs a chi-squared test between groups. Default is \code{FALSE}.
#' @param test A function specifying the statistical test to use for non-chi-squared tests. Default is \code{auto_test}.
#'
#' @return A data table (if using `data.table::rbindlist`) summarizing the test results, including:
#' \itemize{
#'   \item \code{x}: The first group in the comparison.
#'   \item \code{y}: The second group in the comparison.
#'   \item \code{p}: The p-value of the test.
#'   \item \code{method}: The method used for the test.
#'   \item \code{df}: Degrees of freedom (if applicable).
#'   \item \code{statistic}: The test statistic.
#'   \item \code{mean_x}: Mean of the first group.
#'   \item \code{sd_x}: Standard deviation of the first group.
#'   \item \code{mean_y}: Mean of the second group.
#'   \item \code{sd_y}: Standard deviation of the second group.
#' }
#'
#' @importFrom data.table rbindlist
#' @examples
#' # Example 1
#' n <- 1e3
#' dat <- data.frame(
#'   Group = paste0("Group_", 1:100) |> rep(each = 10),
#'   GLD = rnorm(n, 3, 2),
#'   WOJ = rpois(n, 3),
#'   WOK = rnorm(n, 2, 4)
#' )
#' suppressWarnings(
#'   ret1 <- matrix_test(dat, group = "Group")
#' )
#' ret1
#'
#' # Example 2
#' dat <- data.frame(
#'   Type = paste0("Group_", 1:100) |> rep(each = 10),
#'   Smoking = rpois(n, 2),
#'   nonSmoking = rpois(n, 5)
#' ) |>
#'   aggregate(. ~ Type, data = _, sum)
#'
#' ret2 <- matrix_test(dat, group = "Type", chisq_test = TRUE)
#' ret2
#' @export
matrix_test <- function(dat, group = "Group", chisq_test = FALSE, test = auto_test) {
  dat <- as.data.frame(dat)
  stopifnot("group is not the colname of dat!" = group %in% names(dat))
  names(dat)[which(names(dat) == group)] <- "group"
  dat$group <- as.factor(dat$group)
  groups_levels <- dat$group |> levels()
  compare_groups <- seq_along(groups_levels) |>
    combn(m = 2) |>
    t()
  if (chisq_test) {
    # stopifnot("dat 需要为三列" = ncol(dat) == 3)
    apply(compare_groups, 1, \(x) {
      a <- groups_levels[x[[1]]]
      b <- groups_levels[x[[2]]]
      ret_tmp <- dat[dat$group %in% c(a, b), !(names(dat) %in% c("group"))] |>
        data.frame(row.names = c(a, b)) |>
        as.matrix() |>
        auto_test(y = NULL)
      return(list(
        x = a,
        y = b,
        p = ret_tmp$p.value,
        method = ret_tmp$method,
        df = ret_tmp$parameter,
        statistic = ret_tmp$statistic
      ))
    }) |>
      rbindlist(fill = TRUE)
  } else {
    lapply(seq_len(nrow(compare_groups)), \(x) {
      a <- groups_levels[compare_groups[x, 1]]
      b <- groups_levels[compare_groups[x, 2]]
      dat_remove_group <- dat[, !(names(dat) %in% c("group"))]
      dat_group <- dat$group
      showProgress(
        x / nrow(compare_groups),
        sprintf("%s vs. %s", a, b)
      )
      lapply(seq_len(ncol(dat_remove_group)), \(y) {
        vecX <- dat_remove_group[dat_group == a, y]
        vecY <- dat_remove_group[dat_group == b, y]
        ret_tmp <- zero_or_value(test(vecX, vecY), zero = NULL)
        if (length(ret_tmp) == 1) {
          ret_tmp <- paste0(a, " vs. ", b, ret_tmp)
          message(ret_tmp)
        }
        return(list(
          .id = names(dat_remove_group)[y],
          x = a,
          y = b,
          p = ret_tmp$p.value |> zero_or_value(),
          method = ret_tmp$method |> zero_or_value(zero = NULL),
          df = ret_tmp$parameter |> zero_or_value(),
          statistic = ret_tmp$statistic |> zero_or_value(),
          mean_x = mean(vecX, na.rm = TRUE),
          sd_x = sd(vecX, na.rm = TRUE),
          mean_y = mean(vecY, na.rm = TRUE),
          sd_y = sd(vecY, na.rm = TRUE)
        ))
      })
    }) |>
      unlist(recursive = FALSE) |>
      rbindlist(fill = TRUE)
  }
}


#' Perform Automatic Statistical Test
#'
#' This function automatically selects and performs the appropriate statistical test
#' based on the input data. It first determines whether to use a chi-squared test
#' for a 2x2 matrix. For non-matrix inputs, it checks for normality and chooses
#' between a t-test (if both samples are normally distributed) or a Wilcoxon
#' signed-rank test (if one or both samples are not normally distributed).
#'
#' @param x A numeric vector or a 2x2 matrix. If \code{x} is a matrix, a chi-squared test is performed.
#' @param y A numeric vector. This parameter is required unless \code{x} is a matrix.
#' @param paired Logical; if \code{TRUE}, indicates that the data are paired.
#'
#' @return The result of the statistical test as an object of class \code{"htest"}.
#' If the values in \code{x} or \code{y} are all the same, the function returns \code{0} and prints a message.
#'
#' @details The function performs the following checks:
#' \itemize{
#'   \item If \code{x} is a 2x2 matrix, a chi-squared test is performed.
#'   \item If all values in \code{x} or \code{y} are the same, a message is displayed and \code{0} is returned.
#'   \item For numeric vectors, normality is tested using the Shapiro-Wilk test.
#'         If both vectors are normally distributed, an independent or paired t-test is performed
#'         based on the \code{paired} argument.
#'   \item If normality is violated, the Wilcoxon signed-rank test is used.
#' }
#'
#' @examples
#' # Example usage with a 2x2 matrix for chi-squared test
#' matrix_data <- matrix(c(10, 20, 30, 40), nrow = 2)
#' auto_test(matrix_data)
#'
#' # Example usage with numeric vectors
#' x <- rnorm(10)
#' y <- rnorm(10)
#' auto_test(x, y)
#'
#' @export
auto_test <- function(x, y, paired = FALSE) {
  # Check if a chi-squared test should be used
  if ((missing(y) || is.null(y)) && is.matrix(x)) {
    # stopifnot("x needs to be a 2x2 matrix for chi-squared test" = nrow(x) == ncol(x) & nrow(x) == 2)
    return(chisq.test(x))
  }
  # Check if all values in x or y are the same
  if (all(x[1] == x) | all(y[1] == y)) {
    message("All values are the same")
    return(0)
  }
  # Perform Shapiro-Wilk test for normality
  shapiro_test_x <- shapiro.test(x)
  shapiro_test_y <- shapiro.test(y)
  # Check if both samples come from normal distributions
  if (shapiro_test_x$p.value > 0.05 & shapiro_test_y$p.value > 0.05) {
    # If both samples are normally distributed, use t-test
    lsVarTest <- bartlett.test(list(x, y))
    if (lsVarTest$p.value > 0.05) {
      result <- t.test(x, y, paired = paired, var.equal = TRUE)
      result$var.equal <- TRUE
    } else {
      result <- t.test(x, y, paired = paired, var.equal = FALSE)
      result$var.equal <- FALSE
    }
  } else {
    # If one or both samples are not normally distributed, use Wilcoxon signed-rank test
    result <- wilcox.test(x, y, paired = paired)
    result$parameter <- NA_real_
  }
  return(result)
}
