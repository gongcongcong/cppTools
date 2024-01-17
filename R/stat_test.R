#' @title matrix_test
#' @export
#'

matrix_test <- function(n) {
        ret <- .C("R_matrix_test", n = as.integer(n))
        ret
}

#' auto_test
#' 
#' Automatically selecting a test analysis (t.test or wilcox.test) to compare two samples based on their distributions.
#'
#' @export

auto_test <- function(x, y, paired = FALSE) {
        # 使用 Shapiro-Wilk 检验检查正态性
        shapiro_test_x <- shapiro.test(x)
        shapiro_test_y <- shapiro.test(y)
        # 检查两个样本是否都来自正态分布
        if (shapiro_test_x$p.value > 0.05 & shapiro_test_y$p.value > 0.05) {
                # 如果两个样本都是正态分布，则使用 t.test
                result <- t.test(x, y, paired = paired)
        } else {
                 # 如果有一个或两个样本不是正态分布，则使用 Wilcoxon signed-rank test
                result <- wilcox.test(x, y, paired = paired)
        }
        return(result)
} 
