test_that("multiplication works", {
  n <- 1e3
  n2 <- 1e2
  dat <- matrix(rnorm(n), ncol = n2,
                dimnames = list(NULL, paste0("Col_", seq_len(n2))))
  comp <- combn(ncol(dat), 2)
  res <- test_matrix(dat, comp, t.test)
  res <- data.table::rbindlist(res)
  res <- res$p.value
  res_target <- vector("numeric", ncol(comp))
  for (i in seq_len(ncol(comp))) {
    res_target[i] <- t.test(dat[, comp[1, i]], dat[, comp[2, i]])$p.value
  }
  expect_equal(res, res_target)
})
test_that("multiplication works", {
  n <- 1e3
  n2 <- 1e2
  dat <- matrix(rnorm(n), ncol = n2,
                dimnames = list(NULL, paste0("Col_", seq_len(n2))))
  comp <- combn(ncol(dat), 2)
  res <- test_matrix(dat, comp, wilcox.test)
  res <- data.table::rbindlist(res)
  res <- res$p.value
  res_target <- vector("numeric", ncol(comp))
  for (i in seq_len(ncol(comp))) {
    res_target[i] <- wilcox.test(dat[, comp[1, i]], dat[, comp[2, i]])$p.value
  }
  expect_equal(res, res_target)
})
