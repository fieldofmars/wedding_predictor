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

#' Expected monetary cost for a given first ceremony date
#'
#' Policy: pay booking_cost now for a ceremony at d1 months; if the visa
#' is not granted by d1, pay resched_cost to move to a safe date d2.
#'
#' @param d1 First ceremony time (months after lodgement).
#' @param mu Meanlog of processing-time log-normal.
#' @param sigma Sdlog of processing-time log-normal.
#' @param booking_cost First booking cost (default 400).
#' @param resched_cost Reschedule cost (default 200).
#'
#' @return Expected monetary cost in dollars.
#' @export
expected_cost <- function(d1,
                          mu,
                          sigma,
                          booking_cost = 400,
                          resched_cost = 200) {
  p_resched <- 1 - visa_cdf(d1, mu, sigma)
  booking_cost + resched_cost * p_resched
}

#' Expected waiting time between grant and ceremony
#'
#' If visa is granted at time g:
#' - If g <= d1: ceremony at d1, wait = d1 - g
#' - If g > d1: ceremony at d2, wait = d2 - g
#'
#' @param d1 First ceremony time (months after lodgement).
#' @param d2 Safe (late) ceremony time for rescheduled booking (months).
#' @param mu Meanlog of processing-time log-normal.
#' @param sigma Sdlog of processing-time log-normal.
#'
#' @return Expected waiting time (months).
#' @export
expected_wait <- function(d1,
                          d2 = 30,
                          mu,
                          sigma) {
  # For numerical integration, ensure positive bounds
  if (d1 <= 0) stop("d1 must be > 0")
  if (d2 <= 0) stop("d2 must be > 0")
  
  # Integrate over grant times
  part1 <- integrate(
    f     = function(g) (d1 - g) * visa_pdf(g, mu, sigma),
    lower = 0,
    upper = d1
  )$value
  
  part2 <- integrate(
    f     = function(g) (d2 - g) * visa_pdf(g, mu, sigma),
    lower = d1,
    upper = Inf
  )$value
  
  part1 + part2
}

#' Combined loss function: dollars + lambda * waiting time
#'
#' @param d1 First ceremony time (months after lodgement).
#' @param mu Meanlog of processing-time log-normal.
#' @param sigma Sdlog of processing-time log-normal.
#' @param lambda Dollar value per month of waiting (preference weight).
#' @param d2 Safe (late) ceremony time (months).
#' @param booking_cost First booking cost.
#' @param resched_cost Reschedule cost.
#'
#' @return Scalar loss value for optimisation.
#' @export
loss_function <- function(d1,
                          mu,
                          sigma,
                          lambda       = 50,
                          d2           = 30,
                          booking_cost = 400,
                          resched_cost = 200) {
  ecost <- expected_cost(
    d1           = d1,
    mu           = mu,
    sigma        = sigma,
    booking_cost = booking_cost,
    resched_cost = resched_cost
  )
  
  ewait <- expected_wait(
    d1 = d1, d2 = d2,
    mu = mu, sigma = sigma
  )
  
  ecost + lambda * ewait
}

#' Find optimal ceremony time (months after lodgement)
#'
#' Minimises loss_function() over a search range.
#'
#' @param mu Meanlog of processing-time log-normal.
#' @param sigma Sdlog of processing-time log-normal.
#' @param lambda Dollar value per month of waiting.
#' @param d2 Safe rescheduled ceremony date (months).
#' @param lower Lower bound of search interval for d1.
#' @param upper Upper bound of search interval for d1.
#'
#' @return A list with elements: d1_opt, loss_opt.
#' @export
find_optimal_d1 <- function(mu,
                            sigma,
                            lambda = 50,
                            d2     = 30,
                            lower  = 6,
                            upper  = 30) {
  opt <- optimize(
    f     = function(d1) loss_function(
      d1     = d1,
      mu     = mu,
      sigma  = sigma,
      lambda = lambda,
      d2     = d2
    ),
    lower = lower,
    upper = upper
  )
  
  list(
    d1_opt  = opt$minimum,
    loss_opt = opt$objective
  )
}

#' Create a grid of metrics over candidate first ceremony dates
#'
#' Useful for plotting.
#'
#' @param mu Meanlog of processing-time log-normal.
#' @param sigma Sdlog of processing-time log-normal.
#' @param lambda Dollar value per month of waiting.
#' @param d2 Safe rescheduled ceremony date (months).
#' @param d1_seq Numeric vector of candidate d1 values (months).
#'
#' @return A tibble with columns: d1, p_resched, expected_cost,
#'   expected_wait, loss.
#' @export
metrics_grid <- function(mu,
                         sigma,
                         lambda = 50,
                         d2     = 30,
                         d1_seq = seq(6, 30, by = 0.5)) {
  tibble(d1 = d1_seq) %>%
    mutate(
      p_resched     = 1 - visa_cdf(d1, mu, sigma),
      expected_cost = map_dbl(
        d1,
        ~ expected_cost(.x, mu = mu, sigma = sigma)
      ),
      expected_wait = map_dbl(
        d1,
        ~ expected_wait(.x, d2 = d2, mu = mu, sigma = sigma)
      ),
      loss          = map_dbl(
        d1,
        ~ loss_function(.x, mu = mu, sigma = sigma, lambda = lambda, d2 = d2)
      )
    )
}
