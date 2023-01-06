
# calculate measures

estimate_tbs <- function(fit) {
  res <- fit %>%
    group_by(samp) %>%
    group_modify(~ estimate_onetbs(.x$days, .x$pmf))

  exptbs <- fit %>%
    group_by(samp) %>%
    filter(days == 0) %>%
    mutate(expx = 1/pmf) %>%
    pull(expx)

  # returning res with tbs samples and samples of expected(tbs)
  list(res = res,
       exptbs = exptbs)
}

estimate_onetbs <- function(days, pmf) {
  pmf <- pmf[order(days)]
  pmftbs <- c(pmf[-length(pmf)]/pmf[1]-pmf[-1]/pmf[1], pmf[length(pmf)]/pmf[1])
  tibble(days = days[order(days)], pmf = pmftbs)
}

calculate_old_measures <- function(dat) {
  # old measures: SA, contraceptive use (CU), Unmet need (UN), demand satisfied (DS)
  # dat filtered by country; days need to be as reported, use TIMESINCESEXD in DHS, TIMESINCESEXD==days for days<=30
  # checked reported days using #dat%>%filter(TIMESINCESEXD<=30)%>%summarise(out=prod(days==TIMESINCESEXD))
  dat %>% summarise(prop_user = sum(user)/n(),
                                    prop_nonuser = (1-sum(user)/n()),
                    # for non-users, when days are NA, TSLS assumed to be > 28 days
                                    sa_nonuser = (sum(na.omit(days <= 28  & !user))/sum(!user)),
                                    un = (sum(na.omit(days <= 28 & !user))/n()),
                                    ds = (sum(user)/(sum(user) + sum(na.omit(days <= 28 & !user))))
                                    )
}

calculate_new_measures <- function(dat, fit_non_user, fit_user, ci_width = NULL) {
  median_only <- is.null(ci_width)

  if (median_only) {
    ci_width <- 0
  }

  dat_prop_nosex <- dat |>
    dplyr::count(user, non_sa = (nosex | !sa2yrs)) |>
    dplyr::group_by(user) |>
    dplyr::mutate(tot_n = sum(n), prop = prop.table(n))

  non_user_prop_sex <- dat_prop_nosex |>
    dplyr::filter(!user, !non_sa) |>
    dplyr::pull(prop)
  user_prop_sex <- dat_prop_nosex |>
    dplyr::filter(user, !non_sa) |>
    dplyr::pull(prop)

  non_user_tbs <- estimate_tbs(fit_non_user)
  user_tbs <- estimate_tbs(fit_user)

  new_measures_dat <- dat |>
    dplyr::summarise(
      non_user_p_nosex = (1 - non_user_prop_sex),
      user_p_nosex = (1 - user_prop_sex),
      ase_non_user = non_user_prop_sex * 1 / non_user_tbs$exptbs,
      ase_user = user_prop_sex * 1 / user_tbs$exptbs,
      upe = ase_non_user * sum(!user) / n(),
      rpe = ase_user * sum(user) / (ase_user * sum(user) + ase_non_user * sum(!user))
    )

  ci_df <- new_measures_dat |>
    tidybayes::median_qi(.width = ci_width)

  if (median_only) {
    ci_df |>
      dplyr::select(!dplyr::contains("."))
  } else {
    ci_df |> tibble::add_column(.before = 1, ci_width = ci_width)
  }
}
