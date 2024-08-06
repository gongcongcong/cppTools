#' @title eGFR
#' @name eGFR
#' @param Pcr Serum creatinine levels.
#' @param Age Age of the individual. Units is year
#' @param Sex Gender information
#' @param Units mg/dL or umol/L
#' @description
#' Calculate eGFR (estimated Glomerular Filtration Rate) based on input parameters
#' calculate the eGFR using the following formula modified for Chinese:
#' \deqn{
#' eGFR(ml/min\ per\ 1.73 m^2) = 186 \times Pcr^{-1.154}( mg/dl) \times age^{-0.203}(year) \times 1.227 \times 0.742 (if\ female)
#' }
#' \cite{Ma, Y. et.al (2006). Modified glomerular filtration rate estimating equation for Chinese patients with chronic kidney disease. Journal of the American Society of Nephrology, 17(10), 2937-2944.}
#' @examples
#'  n <- 1e3
#'  pcr <- rnorm(n, 50, 3)
#'  age <- runif(n, 40, 70)
#'  sex <- sample(c(0L, 1L), n, TRUE) |> factor(labels = c("Female", "Male"))
#'  sex_f <- ifelse(sex == "Female", 0.742, 1)
#'  egfr <- prof_check(eGFR, pcr, age, sex, Units = "mg/dL")
#'  egfr2 <- 186 * (pcr^(-1.154)) * (age^(-0.203)) * 1.227 * sex_f
#'  all.equal(egfr, egfr2)
#' @export
#'
eGFR <- function(pcr, age, sex, Units = c("mg/dL", "umol/L")) {
        sex <- ifelse(sex == "Female", 0.742, 1.0)
        unit <- match.arg(Units)
        .Call("R_eGFR", pcr = as.numeric(pcr), age = as.numeric(age), sex = as.numeric(sex), unit = as.character(unit))
}
