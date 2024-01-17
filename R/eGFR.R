#' @title eGFR
#' @name eGFR
#' @param Pcr NumericVector - Serum creatinine levels.
#' @param Age NumericVector - Age of the individual. Units is year
#' @param Sex IntegerVector - Gender information
#' @param Pcr_units TRUE means units is mg/dL. if the units is umol/L should set the parameter `Pcr_units` to FALSE
#' @description
#' Calculate eGFR (estimated Glomerular Filtration Rate) based on input parameters
#' calculate the eGFR using the following formula:
#' \eqn{
#' eGFR(ml/min\ per\ 1.73 m^2) = 186 * Pcr^{-1.154}( mg/dl) * age^{-0.203}(year) * 1.227 * 0.742\ (if\ female)
#' }
#' \cite{Ma, Y. et.al (2006). Modified glomerular filtration rate estimating equation for Chinese patients with chronic kidney disease. Journal of the American Society of Nephrology, 17(10), 2937-2944.}
#' @examples
#'  n <- 1e3
#'  pcr <- rnorm(n, 50, 3)
#'  age <- runif(n, 40, 70)
#'  sex <- sample(c(0L, 1L), n, TRUE) |> factor(labels = c("Female", "Male"))
#'  sex_f <- ifelse(sex == "Female", 0.742, 1)
#'  egfr <- eGFR(pcr, age, sex, Pcr_units = TRUE)
#'  egfr2 <- 186 * (pcr^(-1.154)) * (age^(-0.203)) * 1.227 * sex_f
#'  all.equal(egfr, egfr2)
#'
#' @export
#'
eGFR <- function(Pcr, Age, Sex, Units = c("mg/dL", "umol/L")) {
        Sex <- ifelse(Sex == "Female", 0.742, 1.0)
        Units <- ifelse(Units == "mg/dL", 1, 2)
        ret <- .C(R_eGFR, as.double(Pcr), as.double(Age), as.double(Sex), as.integer(length(Pcr)), egfr =  double(length(Pcr)), as.integer(Units))
        ret$egfr
}
