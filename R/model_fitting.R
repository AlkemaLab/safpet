
# model fitting

fit_density <- function(dat, # dat is tibble,
                        # must have reported_value (time: integer) and reported_unit (unit: "day", "week", "month", "year")
                        specification, # model specification
                        # for subsetting data, can specify ctry AND use, if so, dat is filtered based on that
                        ctry = NULL, # country name
                        use = NULL, # logical
                        knots = NULL,
                        filename = NULL, #"fit",
                        ...) {

  if (!is.null(ctry) & !is.null(use)) {
    dat <- dat %>%
      filter(country == ctry, user == use)
  }

  dat <- dat %>%
    mutate(
      reported_value = time,
      reported_unit = unit
    )

  dat2 <- dat %>%
    left_join(specification$df)

  fit <- fit_spline_model(dat2$min_trueday, dat2$max_trueday, specification$grid, knots, ...)
  if (!is.null(filename)){
    saveRDS(fit, here::here(paste(filename, ".rds", sep = "")))
  }

  res <- spread_draws(fit, pmf[days]) %>%
    rename(samp = .draw) %>%
    select(days, pmf, samp) # note: days=1 is day0
  res$days <- res$days - 1

  list(
    res = res,
    dat = dat2
  )
}


fit_spline_model <- function(min_y, max_y,
                             grid, knots,
                             use_splines = TRUE,
                             ...
) {
  N <- length(min_y)
  if (use_splines){
    B <- t(ibs(max(grid)-grid, knots = max(grid) - knots, degree = 3, intercept = FALSE))
    B[1,] <- 0
  } else {
    B <- diag(length(grid))
  }

  num_basis <- nrow(B)
  num_grid <- ncol(B)

  stan_data <- list(
    N = N,
    min_Y = min_y,
    max_Y = max_y,
    num_grid = num_grid,
    num_basis = num_basis,
    B = B
  )

  # rstan
  fit <- stan(
    file = here::here(
      "stan/fit_density_spline_softmax_heapingyear.stan"),
    data = stan_data,
    ...
  )

  # cmd stan
  #  model <- cmdstan_model(here::here(
  #    "stan/fit_density_spline_softmax_heapingyear.stan"))

  #  fit <- model$sample(
  #    stan_data,
  # parallel_chains = 4,
  # iter_warmup = iter_warmup,
  #  iter_sampling =  iter_sampling
  #   ...
  #  )

  fit
}


# softmax <- function(x) exp(x) / sum(exp(x))


# tidy_draws.CmdStanMCMC <- function(model, ...) {
#   return(posterior::as_draws_df(model$draws()))
# }
