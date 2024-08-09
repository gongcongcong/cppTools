#' zero_or_value
#'
#' Return zero if the expression encounters an error, or return the value.
#'
#' @param expre Expression to be evaluated.
#' @param zero Return value if the expression encounters an error. If NULL, returns the error message as a character string.
#' @param verbose Logical indicating whether to display error messages.
#' @export
#' @examples
#' zero_or_value(1 + "a", verbose = TRUE) # Displays error message
#' zero_or_value(1 + 2) # Returns 3
#' zero_or_value(1 + "a") # Returns 0 without displaying error message
#' zero_or_value(1 + "a", zero = NULL)

zero_or_value <- function(expre, zero = NA_real_, verbose = FALSE) {
        result <- tryCatch({
                eval(expre)
        }, error = function(e) {
                if (verbose) {
                        message(conditionMessage(e))
                }
                if (is.null(zero)) {
                        return(error_to_character(e))
                } else {
                        return(zero)
                }
        })

        return(result)
}


#' error_to_character
#'
#' Convert error object to character string.
#'
#' @param e Error object.
#'
error_to_character <- function(e) {
        return(conditionMessage(e))
}


#' calculate mode
#'
#' @param x numeric vector
#' @export
#'
identify_mode <- function(x) {
        .Call("R_mode", x)
}
