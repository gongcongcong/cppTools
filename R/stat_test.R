#' @title matrix_test
#' @importFrom data.table rbindlist
#' @export
#'

matrix_test <- function(dat, group = "group", chisq_test = FALSE, test = auto_test) {
        names(dat)[which(names(dat) == group)] <- "group"
        dat$group <- as.factor(dat$group)
        groups_levels <- dat$group |> levels()
        compare_groups <- seq_along(groups_levels) |>
                combn(m = 2) |> t()
        if (chisq_test) {
                stopifnot("dat 需要为三列" = ncol(dat) == 3)
                apply(compare_groups, 1, \(x) {
                        a <- groups_levels[x[[1]]]
                        b <- groups_levels[x[[2]]]
                        ret_tmp <- dat[group %in% c(a, b), !(names(dat) %in% c("group"))] |>
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
                        rbindlist()
        } else {
                apply(compare_groups, 1, \(x) {
                        a <- groups_levels[x[[1]]]
                        b <- groups_levels[x[[2]]]
                        lapply(dat[, !(names(dat) %in% c("group"))], \(y) {
                                vecX <- y[dat$group == a]
                                vecY <- y[dat$group == b]
                                ret_tmp <- test(vecX, vecY)
                                return(list(
                                        x = a,
                                        y = b,
                                        p = ret_tmp$p.value,
                                        method = ret_tmp$method,
                                        df = ret_tmp$parameter,
                                        statistic = ret_tmp$statistic,
                                        mean_x = mean(vecX, na.rm = TRUE),
                                        sd_x = sd(vecX, na.rm = TRUE),
                                        mean_y = mean(vecY, na.rm = TRUE),
                                        sd_y = sd(vecY, na.rm = TRUE)
                                ))
                        })
                }) |>
                        unlist(recursive = FALSE) |>
                        rbindlist(idcol = TRUE, fill = TRUE)
        }


}

#' auto_test
#'
#' Automatically selecting a test analysis (t.test or wilcox.test) to compare two samples based on their distributions.
#'
#' @export

auto_test <- function(x, y, paired = FALSE) {
        # 判断是否使用chisq test
        if ((missing(y) | is.null(y)) & is.matrix(x)) {
                stopifnot("x 需要为2 X 2 的矩阵方可进行卡方检验" = nrow(x) == ncol(x) & nrow(x) == 2)
                return(chisq.test(x))
        }
        # 使用 Shapiro-Wilk 检验检查正态性
        shapiro_test_x <- shapiro.test(x)
        shapiro_test_y <- shapiro.test(y)
        # 检查两个样本是否都来自正态分布
        if (shapiro_test_x$p.value > 0.05 & shapiro_test_y$p.value > 0.05) {
                # 如果两个样本都是正态分布，则使用 t.test
                lsVarTest <- bartlett.test(list(x, y))
                if (lsVarTest$p.value > 0.05) {
                        result <- t.test(x, y, paired = paired, var.equal = TRUE)
                        result$var.equal <- TRUE
                } else {
                        result <- t.test(x, y, paired = paired, var.equal = FALSE)
                        result$var.equal <- FALSE
                }
        } else {
                 # 如果有一个或两个样本不是正态分布，则使用 Wilcoxon signed-rank test
                result <- wilcox.test(x, y, paired = paired)
        }
        return(result)
}

