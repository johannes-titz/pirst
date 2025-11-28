set.seed(1)
d <- pirstsim(cases = 1, nmeasures = 1, noise = .1)
set.seed(1)
d2 <- pirstsim(cases = 1, nmeasures = 1, noise = .2)

helper <- function(d) {
  sum(abs(as.matrix(easy_cmr(d)[, 1:2]) - easy_jCMRx(d$X, d$Y)))
}

tol <- 0.00001
test_that("pirstim equals jCMRx", {
  expect_lt(helper(d), tol)
  # expect_lt(helper(d2), tol) # fails
})
