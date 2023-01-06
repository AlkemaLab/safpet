plot_fit <- function(
    # plot contains pmf_est and/or dat and/or pmf_true
    pmf_est = NULL,
    dat = NULL,
    pmf_true = NULL,

    specification = NULL,
    grid = seq(0, 364 + 365),
    c_levels = c(0.5, 0.8, 0.95),
    xlab = "Time since last sex",
    ylab = "density",
    title, ...) {

  if (!is.null(pmf_est)) {
    pout <- pmf_est %>%
      group_by(days) %>%
      median_qi(pmf, .width = c_levels) %>%
      ggplot(aes(x = days)) +
      geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = as.character(.width)), alpha = 0.5) +
      scale_fill_manual(name = "CI %", values = c("darkblue", "blue", "skyblue")) +
      geom_line(aes(y = pmf, colour = "fit"), size = 0.5) +
      labs(x = xlab, y = ylab, colour = "Estimates") +
      ggtitle(title) # processed data fit
  } else {
    pout <- ggplot()
  }

  if (!is.null(dat)) {
    # check if specs need to be added
    if (!("true_days" %in% names(dat))) {
      dat <- dat %>%
        mutate(
          reported_value = time,
          reported_unit = unit
        ) %>%
        left_join(specification$df)
    }
    dat <- dat %>%
      unnest(cols = true_days) %>%
      mutate(
        true_days = ifelse(is.na(true_days), min_trueday, true_days),
        true_prop = 1 / (max_trueday - min_trueday + 1)
      ) %>%
      mutate(reported_unit = factor(reported_unit, levels = c("day", "week", "month", "year")))
    pout <- pout +
      new_scale_fill() +
      geom_histogram(
        data = dat,
        aes(
          x = true_days, fill = reported_unit, weight = true_prop,
          y = stat(count) / sum(count)
        ), binwidth = 1, alpha = 0.5
      ) +
      labs(fill = "Reported units")
  }

  if (!is.null(pmf_true)) {
    pout <- pout +
      geom_line(data = pmf_true, aes(x = time, y = pmf, color = "truth"))
  }
  pout
}

