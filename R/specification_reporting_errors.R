clamp_days <- function(days, grid) {
  pmax(pmin(days, max(grid)), 0)
}

get_true_day_range_week <- function(week, grid) {
  tibble::tibble(
    reported_value = week,
    reported_unit = forcats::as_factor("week"),
    min_true_day = clamp_days(week * 7, grid),
    max_true_day = clamp_days(week * 7 + 6, grid)
  )
}

get_true_day_range_month <- function(month, grid) {
  tibble::tibble(
    reported_value = month,
    reported_unit = forcats::as_factor("month"),
    min_true_day = clamp_days(month * 30 + 1, grid),
    max_true_day = clamp_days(month * 30 + 30, grid)
  )
}

get_true_day_range_year <- function(year, grid) {
  tibble::tibble(
    reported_value = year,
    reported_unit = forcats::as_factor("year"),
    min_true_day = clamp_days(year * 365 - 31, grid),
    max_true_day = clamp_days(year * 365 + 364, grid)
  )
}

get_true_day_range_day_heap <- function(day, grid, z_range_day_heap) {
  tibble::tibble(
    reported_value = day,
    reported_unit = forcats::as_factor("day"),
  ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      offset = ifelse(reported_value >= 7 && any(reported_value %% z_range_day_heap == 0), 2, 0)
    ) |>
    dplyr::mutate(
      min_true_day = clamp_days(reported_value - offset, grid),
      max_true_day = clamp_days(reported_value + offset, grid)
    ) |>
    dplyr::select(-offset)
}

generate_grids_and_units_specification <- function(heaped = TRUE) {
  grid <- seq(0, 364 + 365)

  z_range_week <- seq(1, floor((max(grid) - 1) / 7))
  z_range_month <- seq(1, floor((max(grid) - 1) / 30))
  z_range_year <- seq(1, floor((max(grid) - 1) / 365))

  z_range_day_heap <- c(seq(7, 28, by = 7), 30, 60, 90)

  if (!heaped) {
    z_range_day_heap <- .Machine$integer.max
  }

  days <- get_true_day_range_day_heap(grid, grid, z_range_day_heap)
  weeks <- get_true_day_range_week(z_range_week, grid)
  months <- get_true_day_range_month(z_range_month, grid)
  years <- get_true_day_range_year(z_range_year, grid)

  df <- dplyr::bind_rows(days, weeks, months, years) |>
    dplyr::mutate(true_days = list(seq(min_true_day, max_true_day))) |>
    dplyr::relocate(true_days, .before = min_true_day) |>
    dplyr::rename(min_trueday = min_true_day, max_trueday = max_true_day)

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
