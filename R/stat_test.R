#' @title matrix_test
#' @importFrom data.table rbindlist
#' @examples
#' #example 1
#' n <- 1e3
#' dat <- data.frame(
#'      Group = paste0("Group_", 1:100) |> rep(each = 10),
#'      GLD = rnorm(n, 3, 2),
#'      WOJ = rpois(n, 3),
#'      WOK = rnorm(n, 2, 4)
#'      )
#' suppressWarnings(
#' #        ret1 <- prof_check(matrix_test, dat, group = "Group")
#'          ret1 <- matrix_test(dat, group = "Group")
#'         )
#' ret1
#'
#' #example 2
#' dat <- data.frame(
#'      Type = paste0("Group_", 1:100) |> rep(each = 10),
#'      Smoking = rpois(n, 2),
#'      nonSmoking = rpois(n, 5)
#'      ) |>
#'      aggregate(. ~ Type, data = _, sum)
#'
#' #ret2 <- prof_check(matrix_test, dat, group = "Type", chisq_test = TRUE)
#' ret2 <- matrix_test(dat, group = "Type", chisq_test = TRUE)
#' ret2
#' @export
#'

matrix_test <- function(dat, group = "Group", chisq_test = FALSE, test = auto_test) {
        dat <- as.data.frame(dat)
        stopifnot("group is not the colname of dat!" = group %in% names(dat))
        names(dat)[which(names(dat) == group)] <- "group"
        dat$group <- as.factor(dat$group)
        groups_levels <- dat$group |> levels()
        compare_groups <- seq_along(groups_levels) |>
                combn(m = 2) |> t()
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
                        showProgress(x/nrow(compare_groups),
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
                                        .id =  names(dat_remove_group)[y],
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

#' auto_test
#'
#' Automatically selecting a test analysis (t.test or wilcox.test) to compare two samples based on their distributions.
#'
#' @export

auto_test <- function(x, y, paired = FALSE) {
        # 判断是否使用chisq test
        if ((missing(y) | is.null(y)) & is.matrix(x)) {
                # stopifnot("x 需要为2 X 2 的矩阵方可进行卡方检验" = nrow(x) == ncol(x) & nrow(x) == 2)
                return(chisq.test(x))
        }
        if (all(x[1] == x) | all(y[1] == y)) {
                message("值都一样")
                return(0)
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
                result$parameter <- NA_real_
        }
        return(result)
}

