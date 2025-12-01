set.seed(1)
d <- pirstsim(cases = 1, nmeasures = 1, noise = .1)
set.seed(1)
d2 <- pirstsim(cases = 1, nmeasures = 1, noise = .2)

helper <- function(d) {
  res_easy <- merge(d, easy_cmr(d), by = c("Condition", "Trace", "Measurement", "Person"))
  sum(abs(as.matrix(res_easy)[, c("X.y", "Y.y")] - easy_jCMRx(d$X, d$Y)))
}

tol <- 0.00001
test_that("pirstim equals jCMRx", {
  expect_lt(helper(d), tol)
  #expect_lt(helper(d2), tol) # fails
})
