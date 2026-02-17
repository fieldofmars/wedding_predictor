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

test_that("visa_cdf at q50 returns ~0.5", {
  params <- fit_lognormal_from_quantiles(q50 = 12, q90 = 23)
  expect_equal(visa_cdf(12, params$mu, params$sigma), 0.5, tolerance = 1e-6)
})

test_that("visa_cdf at q90 returns ~0.9", {
  params <- fit_lognormal_from_quantiles(q50 = 12, q90 = 23)
  expect_equal(visa_cdf(23, params$mu, params$sigma), 0.9, tolerance = 1e-6)
})

test_that("scenario_analysis probabilities sum to 1", {
  params <- fit_lognormal_from_quantiles(q50 = 12, q90 = 23)
  
  for (d1 in c(5, 10, 15, 20, 30)) {
    sc <- scenario_analysis(d1, mu = params$mu, sigma = params$sigma)
    total <- sc$p_on_time + sc$p_resched + sc$p_new_book
    expect_equal(total, 1, tolerance = 1e-10)
  }
})

test_that("scenario_analysis returns correct cumulative costs with defaults", {
  params <- fit_lognormal_from_quantiles(q50 = 12, q90 = 23)
  
  sc <- scenario_analysis(d1 = 10, mu = params$mu, sigma = params$sigma,
                          booking_cost = 400, resched_cost = 200)
  
  expect_equal(sc$cost_on_time, 400)
  expect_equal(sc$cost_resched, 600)
  expect_equal(sc$cost_new_book, 1000)
})

test_that("scenario_analysis returns correct cumulative costs with custom values", {
  params <- fit_lognormal_from_quantiles(q50 = 12, q90 = 23)
  
  sc <- scenario_analysis(d1 = 10, mu = params$mu, sigma = params$sigma,
                          booking_cost = 1000, resched_cost = 500)
  
  expect_equal(sc$cost_on_time, 1000)
  expect_equal(sc$cost_resched, 1500)
  expect_equal(sc$cost_new_book, 2500)
})

test_that("scenario_analysis returns correct costs with custom new_booking_cost", {
  params <- fit_lognormal_from_quantiles(q50 = 12, q90 = 23)
  
  sc <- scenario_analysis(d1 = 10, mu = params$mu, sigma = params$sigma,
                          booking_cost = 400, resched_cost = 200,
                          new_booking_cost = 600)
  
  expect_equal(sc$cost_on_time, 400)
  expect_equal(sc$cost_resched, 600)
  expect_equal(sc$cost_new_book, 1200)
})

test_that("scenario_analysis: later d1 means higher p_on_time", {
  params <- fit_lognormal_from_quantiles(q50 = 12, q90 = 23)
  
  sc_early <- scenario_analysis(d1 = 6, mu = params$mu, sigma = params$sigma)
  sc_late  <- scenario_analysis(d1 = 20, mu = params$mu, sigma = params$sigma)
  
  expect_true(sc_late$p_on_time > sc_early$p_on_time)
})

test_that("scenario_analysis: very late d1 has near-zero reschedule/new booking", {
  params <- fit_lognormal_from_quantiles(q50 = 12, q90 = 23)
  
  sc <- scenario_analysis(d1 = 60, mu = params$mu, sigma = params$sigma)
  
  expect_true(sc$p_on_time > 0.99)
  expect_true(sc$p_resched < 0.01)
  expect_true(sc$p_new_book < 0.001)
})

test_that("scenario_analysis: very early d1 has low p_on_time", {
  params <- fit_lognormal_from_quantiles(q50 = 12, q90 = 23)
  
  sc <- scenario_analysis(d1 = 3, mu = params$mu, sigma = params$sigma)
  
  expect_true(sc$p_on_time < 0.2)
})

test_that("scenario_analysis: costs are strictly increasing across scenarios", {
  params <- fit_lognormal_from_quantiles(q50 = 12, q90 = 23)
  
  sc <- scenario_analysis(d1 = 10, mu = params$mu, sigma = params$sigma,
                          booking_cost = 400, resched_cost = 200)
  
  expect_true(sc$cost_on_time < sc$cost_resched)
  expect_true(sc$cost_resched < sc$cost_new_book)
})

test_that("find_optimal_d1 returns the correct quantile", {
  params <- fit_lognormal_from_quantiles(q50 = 12, q90 = 23)
  
  d1 <- find_optimal_d1(mu = params$mu, sigma = params$sigma, target_prob = 0.75)
  
  expect_equal(visa_cdf(d1, params$mu, params$sigma), 0.75, tolerance = 1e-6)
})

test_that("find_optimal_d1: higher target_prob gives later d1", {
  params <- fit_lognormal_from_quantiles(q50 = 12, q90 = 23)
  
  d1_low  <- find_optimal_d1(mu = params$mu, sigma = params$sigma, target_prob = 0.5)
  d1_high <- find_optimal_d1(mu = params$mu, sigma = params$sigma, target_prob = 0.9)
  
  expect_true(d1_high > d1_low)
})

test_that("find_optimal_d1 at 0.5 returns q50", {
  params <- fit_lognormal_from_quantiles(q50 = 12, q90 = 23)
  
  d1 <- find_optimal_d1(mu = params$mu, sigma = params$sigma, target_prob = 0.5)
  
  expect_equal(d1, 12, tolerance = 1e-6)
})

test_that("find_optimal_d1 at 0.9 returns q90", {
  params <- fit_lognormal_from_quantiles(q50 = 12, q90 = 23)
  
  d1 <- find_optimal_d1(mu = params$mu, sigma = params$sigma, target_prob = 0.9)
  
  expect_equal(d1, 23, tolerance = 1e-6)
})

test_that("metrics_grid returns correct columns", {
  params <- fit_lognormal_from_quantiles(q50 = 12, q90 = 23)
  
  df <- metrics_grid(mu = params$mu, sigma = params$sigma,
                     d1_seq = seq(6, 12, by = 1))
  
  expect_true("p_on_time" %in% names(df))
  expect_true("p_resched" %in% names(df))
  expect_true("p_new_booking" %in% names(df))
  expect_true("cost_on_time" %in% names(df))
  expect_true("cost_resched" %in% names(df))
  expect_true("cost_new_booking" %in% names(df))
})

test_that("metrics_grid costs are fixed and cumulative", {
  params <- fit_lognormal_from_quantiles(q50 = 12, q90 = 23)
  
  df <- metrics_grid(mu = params$mu, sigma = params$sigma,
                     d1_seq = seq(6, 20, by = 2),
                     booking_cost = 400, resched_cost = 200)
  
  expect_true(all(df$cost_on_time == 400))
  expect_true(all(df$cost_resched == 600))
  expect_true(all(df$cost_new_booking == 1000))
})

test_that("metrics_grid probabilities sum to 1 for each row", {
  params <- fit_lognormal_from_quantiles(q50 = 12, q90 = 23)
  
  df <- metrics_grid(mu = params$mu, sigma = params$sigma,
                     d1_seq = seq(6, 30, by = 1))
  
  row_sums <- df$p_on_time + df$p_resched + df$p_new_booking
  expect_true(all(abs(row_sums - 1) < 1e-10))
})

# ── Monte Carlo function tests ───────────────────────────

test_that("run_monte_carlo returns correct structure", {
  params <- fit_lognormal_from_quantiles(q50 = 12, q90 = 23)
  
  results <- run_monte_carlo(
    n_sims = 100, d1 = 15,
    mu = params$mu, sigma = params$sigma,
    seed = 42
  )
  
  expect_true(is.data.frame(results))
  expect_equal(nrow(results), 100)
  expect_true(all(c("sim_id", "grant_time", "scenario", "cost") %in% names(results)))
  expect_true(all(results$grant_time > 0))
  expect_true(all(results$scenario %in% c("on_time", "reschedule", "new_booking")))
})

test_that("run_monte_carlo scenario classification is correct", {
  params <- fit_lognormal_from_quantiles(q50 = 12, q90 = 23)
  
  results <- run_monte_carlo(
    n_sims = 10000, d1 = 15,
    mu = params$mu, sigma = params$sigma,
    seed = 123
  )
  
  # Check that scenario assignment matches grant_time
  expect_true(all(results$grant_time[results$scenario == "on_time"] <= 15))
  expect_true(all(results$grant_time[results$scenario == "reschedule"] > 15))
  expect_true(all(results$grant_time[results$scenario == "reschedule"] <= 27))
  expect_true(all(results$grant_time[results$scenario == "new_booking"] > 27))
})

test_that("run_monte_carlo costs match scenarios", {
  params <- fit_lognormal_from_quantiles(q50 = 12, q90 = 23)
  
  results <- run_monte_carlo(
    n_sims = 1000, d1 = 15,
    mu = params$mu, sigma = params$sigma,
    booking_cost = 400, resched_cost = 200,
    seed = 42
  )
  
  expect_true(all(results$cost[results$scenario == "on_time"] == 400))
  expect_true(all(results$cost[results$scenario == "reschedule"] == 600))
  expect_true(all(results$cost[results$scenario == "new_booking"] == 1000))
})

test_that("run_monte_carlo is reproducible with seed", {
  params <- fit_lognormal_from_quantiles(q50 = 12, q90 = 23)
  
  r1 <- run_monte_carlo(n_sims = 100, d1 = 15,
                         mu = params$mu, sigma = params$sigma, seed = 42)
  r2 <- run_monte_carlo(n_sims = 100, d1 = 15,
                         mu = params$mu, sigma = params$sigma, seed = 42)
  
  expect_equal(r1$grant_time, r2$grant_time)
  expect_equal(r1$cost, r2$cost)
})

test_that("run_monte_carlo converges to closed-form with large N", {
  params <- fit_lognormal_from_quantiles(q50 = 12, q90 = 23)
  d1 <- 15
  
  results <- run_monte_carlo(
    n_sims = 50000, d1 = d1,
    mu = params$mu, sigma = params$sigma,
    seed = 999
  )
  
  sc <- scenario_analysis(d1, mu = params$mu, sigma = params$sigma)
  
  mc_p_on_time <- mean(results$scenario == "on_time")
  mc_p_resched <- mean(results$scenario == "reschedule")
  
  # Should be within ~1% of closed-form with 50k sims
  expect_equal(mc_p_on_time, sc$p_on_time, tolerance = 0.02)
  expect_equal(mc_p_resched, sc$p_resched, tolerance = 0.02)
  
  cf_expected <- sc$p_on_time * sc$cost_on_time +
    sc$p_resched * sc$cost_resched +
    sc$p_new_book * sc$cost_new_book
  
  expect_equal(mean(results$cost), cf_expected, tolerance = 5)
})

test_that("summarise_monte_carlo returns correct structure", {
  params <- fit_lognormal_from_quantiles(q50 = 12, q90 = 23)
  
  results <- run_monte_carlo(
    n_sims = 1000, d1 = 15,
    mu = params$mu, sigma = params$sigma,
    seed = 42
  )
  
  s <- summarise_monte_carlo(results)
  
  expect_true(is.list(s))
  expect_equal(s$n_sims, 1000)
  expect_equal(s$ci_level, 0.95)
  expect_true(s$cost_mean > 0)
  expect_true(s$cost_sd > 0)
  expect_true(s$cost_se > 0)
  expect_true(s$time_mean > 0)
  expect_equal(length(s$cost_ci), 2)
  expect_equal(length(s$time_ci), 2)
  expect_true(s$cost_ci[1] <= s$cost_ci[2])
  expect_true(s$time_ci[1] <= s$time_ci[2])
})

test_that("summarise_monte_carlo scenario counts sum to n_sims", {
  params <- fit_lognormal_from_quantiles(q50 = 12, q90 = 23)
  
  results <- run_monte_carlo(
    n_sims = 5000, d1 = 15,
    mu = params$mu, sigma = params$sigma,
    seed = 42
  )
  
  s <- summarise_monte_carlo(results)
  
  expect_equal(sum(s$scenario_counts$n), 5000)
  expect_equal(sum(s$scenario_counts$proportion), 1, tolerance = 1e-10)
})
