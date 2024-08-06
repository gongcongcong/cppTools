 test_that("test matrix_test", {
   n <- 10
   dat <- data.frame(Group = rep(LETTERS[seq_len(n)], each = 3), TPI = rnorm(n), SAS = rnorm(n, 3))
   test <- matrix_test(dat, test = t.test)
   test2 <- rstatix::t_test(dat, TPI~Group)$p
   test3 <- rstatix::t_test(dat, SAS~Group)$p
   expect_equal(test[test$`.id` == "TPI", p] |> round(digits = 3), test2)
   expect_equal(test[test$`.id` == "SAS", p] |> round(digits = 3), test3)
 })

 test_that("test auto_test", {
   n <- 1e3
   dat <- data.frame(normX1 = rnorm(n), normX2 = rnorm(n, 3, sd = 10), poisX3 = rpois(n, 4))
   test <- auto_test(dat$normX1, dat$normX2)[c("method", "p.value")]
   test2 <- auto_test(dat$normX1, dat$poisX3)[c("method", "p.value")]
   test3 <- t.test(dat$normX1, dat$normX2)[c("method", "p.value")]
   test4 <- wilcox.test(dat$normX1, dat$poisX3)[c("method", "p.value")]
   expect_equal(test, test3, tolerance = .1)
   expect_equal(test2, test4, tolerance = .1)
 })

