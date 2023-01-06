
generate_grids_and_units_specification <- function(heaped = TRUE) {
  grid <- seq(0, 364 + 365)

  z_range_week <- seq(1, floor((max(grid) - 1) / 7))
  z_range_month <- seq(1, floor((max(grid) - 1) / 30))
  z_range_year <- seq(1, floor((max(grid) - 1) / 365))

  z_range_day_heap <- c(seq(7, 28, by = 7), 30, 60, 90)

  if (!heaped) {
    z_range_day_heap <- .Machine$integer.max
  }

  days <- tibble::tibble(
    reported_value = grid,
    reported_unit = forcats::as_factor("day"),
  ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      offset = ifelse(
        reported_value >= 7 && any(reported_value %% z_range_day_heap == 0),
        2,
        0
      )
    ) |>
    dplyr::mutate(
      min_trueday = reported_value - offset,
      max_trueday = reported_value + offset
    ) |>
    dplyr::select(-offset)

  weeks <- tibble::tibble(
    reported_value = z_range_week,
    reported_unit = forcats::as_factor("week")
  ) |>
    dplyr::mutate(
      min_trueday = reported_value * 7,
      max_trueday = min_trueday + 6
    )

  months <- tibble::tibble(
    reported_value = z_range_month,
    reported_unit = forcats::as_factor("month")
  ) |>
    dplyr::mutate(
      min_trueday = reported_value * 30 + 1,
      max_trueday = min_trueday + 29
    )

  years <- tibble::tibble(
    reported_value = z_range_year,
    reported_unit = forcats::as_factor("year")
  ) |>
    dplyr::mutate(
      min_trueday = reported_value * 365 - 31,
      max_trueday = reported_value * 365 + 364
    )

  df <- dplyr::bind_rows(days, weeks, months, years) |>
    dplyr::mutate(true_days = list(seq(min_trueday, max_trueday))) |>
    dplyr::relocate(true_days, .before = min_trueday)

  list(
    grid = seq(0, 364 + 365),
    heaped = heaped,
    z_range_day_heap = z_range_day_heap,
    z_range_week = z_range_week,
    z_range_month = z_range_month,
    z_range_year = z_range_year,
    df = df
  )
}
