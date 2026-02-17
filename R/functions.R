# R/functions.R

#' Fit log-normal parameters from two quantiles
#'
#' @param q50 Numeric. Time (months) at which 50% of visas are processed.
#' @param q90 Numeric. Time (months) at which 90% of visas are processed.
#'
#' @return A list with elements \code{mu} and \code{sigma} for a log-normal distribution.
#' @export
fit_lognormal_from_quantiles <- function(q50, q90) {
  stopifnot(q50 > 0, q90 > 0, q90 >= q50)
  
  mu    <- log(q50)
  sigma <- (log(q90) - mu) / qnorm(0.9)
  
  list(mu = mu, sigma = sigma)
}

#' CDF of processing time for a log-normal model
#'
#' @param t Numeric vector of times (months).
#' @param mu Meanlog parameter of the log-normal.
#' @param sigma Sdlog parameter of the log-normal.
#'
#' @return Numeric vector of probabilities P(T <= t).
#' @export
visa_cdf <- function(t, mu, sigma) {
  plnorm(t, meanlog = mu, sdlog = sigma)
}

#' PDF of processing time for a log-normal model
#'
#' @param t Numeric vector of times (months).
#' @param mu Meanlog parameter of the log-normal.
#' @param sigma Sdlog parameter of the log-normal.
#'
#' @return Numeric vector of density values.
#' @export
visa_pdf <- function(t, mu, sigma) {
  dlnorm(t, meanlog = mu, sdlog = sigma)
}

#' Compute scenario probabilities and costs for a given ceremony date
#'
#' Three scenarios based on when the visa is granted (time g):
#'
#' 1. g <= d1: Visa granted in time. You pay booking_cost only.
#' 2. d1 < g <= d1 + 12: Visa granted within the 12-month rebooking window.
#'    You pay booking_cost + resched_cost (full flat fee).
#' 3. g > d1 + 12: Visa not granted within rebooking window. You've already
#'    paid the booking and the reschedule fee, and now need a new booking.
#'    Total cost: booking_cost + resched_cost + new_booking_cost (cumulative).
#'
#' @param d1 First ceremony time (months after lodgement).
#' @param mu Meanlog of processing-time log-normal.
#' @param sigma Sdlog of processing-time log-normal.
#' @param booking_cost First booking cost (default 400).
#' @param resched_cost Reschedule cost - full flat fee (default 200).
#' @param new_booking_cost Cost of a completely new booking if the 12-month
#'   rebooking window is exceeded (default = booking_cost).
#'
#' @return A list with scenario probabilities and per-scenario total costs.
#' @export
scenario_analysis <- function(d1,
                              mu,
                              sigma,
                              booking_cost     = 400,
                              resched_cost     = 200,
                              new_booking_cost = NULL) {
  if (is.null(new_booking_cost)) {
    new_booking_cost <- booking_cost
  }
  
  p_on_time   <- visa_cdf(d1, mu, sigma)
  p_resched   <- visa_cdf(d1 + 12, mu, sigma) - visa_cdf(d1, mu, sigma)
  p_new_book  <- 1 - visa_cdf(d1 + 12, mu, sigma)
  
  cost_on_time  <- booking_cost
  cost_resched  <- booking_cost + resched_cost
  # Cumulative: you already paid booking + reschedule, now add new booking
  cost_new_book <- booking_cost + resched_cost + new_booking_cost
  
  list(
    p_on_time      = p_on_time,
    p_resched      = p_resched,
    p_new_book     = p_new_book,
    cost_on_time   = cost_on_time,
    cost_resched   = cost_resched,
    cost_new_book  = cost_new_book
  )
}

#' Find optimal ceremony time (months after lodgement)
#'
#' Uses a simple rule: the user picks a target probability of the visa
#' being granted in time. We find the d1 where P(g <= d1) = target_prob.
#' This is just the quantile of the log-normal distribution.
#'
#' @param mu Meanlog of processing-time log-normal.
#' @param sigma Sdlog of processing-time log-normal.
#' @param target_prob Target probability of visa being granted by ceremony
#'   date (between 0 and 1). Higher = safer but longer wait.
#'
#' @return Numeric: months after lodgement for the ceremony.
#' @export
find_optimal_d1 <- function(mu, sigma, target_prob = 0.75) {
  stopifnot(target_prob > 0, target_prob < 1)
  qlnorm(target_prob, meanlog = mu, sdlog = sigma)
}

#' Create a grid of scenario metrics over candidate first ceremony dates
#'
#' Useful for plotting. Shows per-scenario probabilities and fixed costs
#' at each candidate d1.
#'
#' @param mu Meanlog of processing-time log-normal.
#' @param sigma Sdlog of processing-time log-normal.
#' @param d1_seq Numeric vector of candidate d1 values (months).
#' @param booking_cost First booking cost.
#' @param resched_cost Reschedule cost (full flat fee).
#' @param new_booking_cost Cost of new booking if window exceeded. NULL = booking_cost.
#'
#' @return A tibble with columns: d1, p_on_time, p_resched, p_new_booking,
#'   cost_on_time, cost_resched, cost_new_booking.
#' @export
metrics_grid <- function(mu,
                         sigma,
                         d1_seq           = seq(6, 30, by = 0.5),
                         booking_cost     = 400,
                         resched_cost     = 200,
                         new_booking_cost = NULL) {
  if (is.null(new_booking_cost)) {
    new_booking_cost <- booking_cost
  }
  
  tibble(d1 = d1_seq) %>%
    mutate(
      p_on_time     = visa_cdf(d1, mu, sigma),
      p_resched     = visa_cdf(d1 + 12, mu, sigma) - visa_cdf(d1, mu, sigma),
      p_new_booking = 1 - visa_cdf(d1 + 12, mu, sigma),
      cost_on_time     = booking_cost,
      cost_resched     = booking_cost + resched_cost,
      cost_new_booking = booking_cost + resched_cost + new_booking_cost
    )
}

#' Run Monte Carlo simulation of visa processing outcomes
#'
#' Simulates N visa grant times from the log-normal distribution,
#' classifies each into one of three scenarios, and computes the
#' cost for each simulated "life".
#'
#' @param n_sims Number of simulations to run.
#' @param d1 Ceremony date in months after lodgement.
#' @param mu Meanlog of processing-time log-normal.
#' @param sigma Sdlog of processing-time log-normal.
#' @param booking_cost First booking cost.
#' @param resched_cost Reschedule cost (full flat fee).
#' @param new_booking_cost Cost of new booking if window exceeded. NULL = booking_cost.
#' @param seed Optional random seed for reproducibility.
#'
#' @return A tibble with columns: sim_id, grant_time, scenario, cost.
#' @export
run_monte_carlo <- function(n_sims,
                            d1,
                            mu,
                            sigma,
                            booking_cost     = 400,
                            resched_cost     = 200,
                            new_booking_cost = NULL,
                            seed             = NULL) {
  if (is.null(new_booking_cost)) {
    new_booking_cost <- booking_cost
  }
  
  if (!is.null(seed)) set.seed(seed)
  
  grant_times <- rlnorm(n_sims, meanlog = mu, sdlog = sigma)
  
  scenario <- case_when(
    grant_times <= d1        ~ "on_time",
    grant_times <= d1 + 12   ~ "reschedule",
    TRUE                     ~ "new_booking"
  )
  
  cost <- case_when(
    scenario == "on_time"     ~ booking_cost,
    scenario == "reschedule"  ~ booking_cost + resched_cost,
    scenario == "new_booking" ~ booking_cost + resched_cost + new_booking_cost
  )
  
  tibble(
    sim_id     = seq_len(n_sims),
    grant_time = grant_times,
    scenario   = factor(scenario, levels = c("on_time", "reschedule", "new_booking")),
    cost       = cost
  )
}

#' Summarise Monte Carlo results with confidence intervals
#'
#' @param mc_results Tibble from run_monte_carlo().
#' @param ci_level Confidence interval level (default 0.95).
#'
#' @return A list with summary statistics and confidence intervals.
#' @export
summarise_monte_carlo <- function(mc_results, ci_level = 0.95) {
  alpha <- 1 - ci_level
  
  total_n <- nrow(mc_results)
  
  # Scenario proportions
  # count() produces a column called "n" — use .data$n to reference it
  # and total_n (not n) for the denominator to avoid name collision
  scenario_counts <- mc_results %>%
    count(scenario, .drop = FALSE) %>%
    mutate(proportion = n / total_n)
  
  # Cost statistics
  cost_mean   <- mean(mc_results$cost)
  cost_sd     <- sd(mc_results$cost)
  cost_median <- median(mc_results$cost)
  cost_ci     <- quantile(mc_results$cost, probs = c(alpha / 2, 1 - alpha / 2))
  
  # Grant time statistics
  time_mean   <- mean(mc_results$grant_time)
  time_sd     <- sd(mc_results$grant_time)
  time_median <- median(mc_results$grant_time)
  time_ci     <- quantile(mc_results$grant_time, probs = c(alpha / 2, 1 - alpha / 2))
  
  # Standard error of the mean cost (for convergence display)
  cost_se <- cost_sd / sqrt(total_n)
  
  list(
    n_sims          = total_n,
    ci_level        = ci_level,
    scenario_counts = scenario_counts,
    cost_mean       = cost_mean,
    cost_sd         = cost_sd,
    cost_median     = cost_median,
    cost_ci         = cost_ci,
    cost_se         = cost_se,
    time_mean       = time_mean,
    time_sd         = time_sd,
    time_median     = time_median,
    time_ci         = time_ci
  )
}
