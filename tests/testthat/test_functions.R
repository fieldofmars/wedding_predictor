context("Business logic functions")

test_that("fit_lognormal_from_quantiles returns positive sigma", {
  params <- fit_lognormal_from_quantiles(q50 = 12, q90 = 23)
  expect_true(is.list(params))
  expect_true(params$sigma > 0)
})

test_that("visa_cdf is between 0 and 1", {
  params <- fit_lognormal_from_quantiles(q50 = 12, q90 = 23)
  t_vals <- c(1, 12, 23, 60)
  probs  <- visa_cdf(t_vals, mu = params$mu, sigma = params$sigma)
  expect_true(all(probs >= 0 & probs <= 1))
})

test_that("expected_cost decreases with later d1", {
  params <- fit_lognormal_from_quantiles(q50 = 12, q90 = 23)
  c1 <- expected_cost(d1 = 10, mu = params$mu, sigma = params$sigma)
  c2 <- expected_cost(d1 = 20, mu = params$mu, sigma = params$sigma)
  expect_true(c2 <= c1)
})

test_that("find_optimal_d1 returns value within bounds", {
  params <- fit_lognormal_from_quantiles(q50 = 12, q90 = 23)
  opt <- find_optimal_d1(mu = params$mu,
                         sigma = params$sigma,
                         lambda = 50,
                         d2     = 30,
                         lower  = 6,
                         upper  = 30)
  expect_true(opt$d1_opt >= 6 && opt$d1_opt <= 30)
})
