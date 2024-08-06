#' area_shoelace
#'
#' area_shoelace computes the area under a curve using the shoelace formula.
#'
#' @param x x axis
#' @param y y axis
#' @examples
#' x <- c(2, 4, 6, 2)
#' y <- c(4, 4, 1, 4)
#' area_shoelace(x, y)
#' @export

area_shoelace <- function(x, y) {
    .Call(R_shoelace, x = as.double(x), y = as.double(y))
}

#' alpha_count
#'
#' count the alpha frequency in a fasta file
#'
#' @param file the path of fasta file
#' @param verbose whether print the results
#' @param simple whether remove the alpha with zero frequency
#' @export

alpha_count <- function(file, seq_min = 0, seq_max, verbose = FALSE, simple = TRUE) {
    if (missing(seq_max)) seq_max <- 1e6
    alpha <- .Call(R_alpha_count, file = file, seq_min = as.integer(seq_min), seq_max = as.integer(seq_max),
                    verbose = verbose)
    names(alpha) <- LETTERS
    alpha <- alpha[names(alpha) %in% .vecAminoAcids]
    if (simple) {
        alpha[alpha !=0 ]
    } else {
        alpha <- as.data.frame(alpha)
        names(alpha) <- "Freq"
        alpha$Prop <- alpha$Freq/sum(alpha$Freq)
        alpha
    }
}

#' prof_check
#'
#' check the function
#'
#' @param fun function used to check
#' @param  args to the function
#' @importFrom utils Rprof
#' @examples
#'
#'   n <- 1e3
#'   pcr <- rnorm(n, 50, 3)
#'   age <- runif(n, 40, 70)
#'   sex <- sample(c(0L, 1L), n, TRUE) |> factor(labels = c("Female", "Male"))
#'   sex_f <- ifelse(sex == "Female", 0.742, 1)
#'   prof_check(eGFR, pcr, age, sex, "mg/dL")
#'
#' @export

prof_check <- function(fun, ...) {
    args <- list(...)
    tmp <- tempfile()
    on.exit(unlink(tmp))


    # 开始性能分析，设置 filter.callframes 参数为 TRUE
    Rprof(tmp, memory.profiling = TRUE, filter.callframes = TRUE,
          line.profiling = TRUE)

    # 执行传入的函数
    ret <- do.call(fun, args)

    # 停止性能分析
    Rprof(NULL)


    # 分析结果
    prof_data <- summaryRprof(tmp, memory = "both")

    # 输出性能分析结果
    cat("Performance analysis:\n")
    print(prof_data)

    invisible(ret)
}


#' showProgress
#'
#' show the progression of the implement
#'
#' @examples
#'
#' showProgress(0.82, "0.82")
#'
#' @export
#'

showProgress <- function(percentage, value) {
    invisible(
        .C("printProgress", as.numeric(percentage), as.character(value))
    )
}
