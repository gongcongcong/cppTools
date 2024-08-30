#' @title Compute Area Using Shoelace Formula
#' @description
#' `area_shoelace` computes the area enclosed by a polygon defined by the points
#' (\code{x}, \code{y}) using the shoelace formula (also known as Gauss's area formula).
#'
#' @param x A numeric vector representing the x-coordinates of the polygon vertices.
#' @param y A numeric vector representing the y-coordinates of the polygon vertices.
#'
#' @details
#' The shoelace formula calculates the area of a simple polygon whose vertices are described by their Cartesian coordinates in the plane. The vertices must be defined in order, either clockwise or counterclockwise.
#'
#' @return A numeric value representing the computed area.
#'
#' @examples
#' # Example: Compute the area of a triangle
#' x <- c(2, 4, 6, 2)
#' y <- c(4, 4, 1, 4)
#' area_shoelace(x, y)
#'
#' @export
area_shoelace <- function(x, y) {
  .Call(R_shoelace, x = as.double(x), y = as.double(y))
}



#' @title Count Amino Acid Frequency in a FASTA File
#' @description
#' `alpha_count` counts the frequency of amino acids in a given FASTA file, optionally filtering sequences by length and simplifying the output.
#'
#' @param file A character string representing the path to the FASTA file.
#' @param seq_min An integer specifying the minimum length of sequences to be counted. Default is 0.
#' @param seq_max An integer specifying the maximum length of sequences to be counted. Default is \code{1e6}.
#' @param verbose Logical; if \code{TRUE}, prints the results to the console. Default is \code{FALSE}.
#' @param simple Logical; if \code{TRUE}, removes amino acids with zero frequency from the output. Default is \code{TRUE}.
#'
#' @details
#' This function reads a FASTA file and counts the occurrence of each amino acid. The results can be filtered by sequence length and simplified to exclude zero frequencies.
#'
#' @return If \code{simple = TRUE}, returns a named vector of non-zero frequencies. If \code{simple = FALSE}, returns a data frame with columns:
#' \itemize{
#'   \item \code{Freq}: Frequency of each amino acid.
#'   \item \code{Prop}: Proportion of each amino acid in the file.
#' }
#'
#' @examples
#' # Example: Count amino acids in a FASTA file
#' # alpha_count("path/to/your/file.fasta")
#'
#' @export
alpha_count <- function(file, seq_min = 0, seq_max, verbose = FALSE, simple = TRUE) {
  if (missing(seq_max)) seq_max <- 1e6
  alpha <- .Call(R_alpha_count,
    file = file, seq_min = as.integer(seq_min), seq_max = as.integer(seq_max),
    verbose = verbose
  )
  names(alpha) <- LETTERS
  alpha <- alpha[names(alpha) %in% .vecAminoAcids]
  if (simple) {
    alpha[alpha != 0]
  } else {
    alpha <- as.data.frame(alpha)
    names(alpha) <- "Freq"
    alpha$Prop <- alpha$Freq / sum(alpha$Freq)
    alpha
  }
}



#' @title Profile a Function's Performance
#' @description
#' `prof_check` profiles the performance of a specified function, capturing both
#' memory usage and execution time. It utilizes R's built-in `Rprof` tool for profiling.
#'
#' @param fun A function to be profiled.
#' @param ... Additional arguments to pass to the function specified in `fun`.
#'
#' @importFrom utils Rprof
#' @examples
#' # Example: Profile the performance of the eGFR function
#' n <- 1e3
#' pcr <- rnorm(n, 50, 3)
#' age <- runif(n, 40, 70)
#' sex <- sample(c(0L, 1L), n, TRUE) |> factor(labels = c("Female", "Male"))
#' prof_check(eGFR, pcr, age, sex, "mg/dL")
#'
#' @return The result of the profiled function call (invisibly).
#' @export
prof_check <- function(fun, ...) {
  args <- list(...)
  tmp <- tempfile()
  on.exit(unlink(tmp))

  # Start profiling with detailed options
  Rprof(tmp, memory.profiling = TRUE, filter.callframes = TRUE, line.profiling = TRUE)

  # Execute the function with provided arguments
  ret <- do.call(fun, args)

  # Stop profiling
  Rprof(NULL)

  # Analyze the profiling data
  prof_data <- summaryRprof(tmp, memory = "both")

  # Display profiling results
  cat("Performance analysis:\n")
  print(prof_data)

  invisible(ret)
}

#' @title Show Progress
#' @description
#' `showProgress` is used to display the progress of a process in the console.
#'
#' @param percentage Numeric; the percentage of completion (between 0 and 1).
#' @param value Character; a string representing the current progress state.
#'
#' @examples
#' # Example: Show progress at 82%
#' showProgress(0.82, "82%")
#'
#' @export
showProgress <- function(percentage, value) {
  invisible(
    .C("printProgress", as.numeric(percentage), as.character(value))
  )
}
