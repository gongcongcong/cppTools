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
        alpha$prop <- alpha$alpha/sum(alpha$alpha)
        alpha
    }
}
