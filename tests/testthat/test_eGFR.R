test_that("multiplication works", {
  n <- 1e3
  pcr <- rnorm(n, 50, 3)
  age <- runif(n, 40, 70)
  sex <- sample(c(0L, 1L), n, TRUE) |> factor(labels = c("Female", "Male"))
  sex_f <- ifelse(sex == "Female", 0.742, 1)
  egfr <- eGFR(pcr, age, sex, "mg/dL")
  egfr2 <- 186 * (pcr^(-1.154)) * (age^(-0.203)) * 1.227 * sex_f
  expect_equal(egfr, egfr2)
})
