stats_excess_munis <- function(y, l) {
  munis <- ts |> filter(date %in% y, level == l)
  munis_no_sign_excess <- munis |> filter(any(cmr_excess_sign) == FALSE)
  munis_sign_excess <- munis |> filter(any(cmr_excess_sign) == TRUE)
  n_munis <- length(unique(munis$id))
  n_munis_sign_excess <- length(unique(munis_sign_excess$id))
  sign_excess_p <- n_munis_sign_excess / n_munis
  print(paste0("Level: ", l, ", year: ", y))
  print(paste0(
    "Sign Excess: ", n_munis_sign_excess, " (", as_pct(sign_excess_p), ")"
  ))

  a <- munis_sign_excess |>
    group_by(id) |>
    summarize(cmr = sum(cmr), cmr_baseline = sum(cmr_baseline)) |>
    mutate(cmr_excess_p = cmr / cmr_baseline - 1)

  b <- munis_no_sign_excess |>
    group_by(id) |>
    summarize(cmr = sum(cmr), cmr_baseline = sum(cmr_baseline)) |>
    mutate(cmr_excess_p = cmr / cmr_baseline - 1)

  print(paste0("w/ sign. excess, eCMR=", as_pct(mean(a$cmr_excess_p))))
  print(paste0("w/o sign. excess, eCMR=", as_pct(mean(b$cmr_excess_p))))
}

# State
stats_excess_munis(2020, 2)
stats_excess_munis(2021, 2)
stats_excess_munis(2022, 2)
stats_excess_munis(c(2020:2022), 2)

# County
stats_excess_munis(2020, 5)
stats_excess_munis(2021, 5)
stats_excess_munis(2022, 5)
stats_excess_munis(c(2020:2022), 5)

# Muni
stats_excess_munis(2020, 8)
stats_excess_munis(2021, 8)
stats_excess_munis(2022, 8)
stats_excess_munis(c(2020:2022), 8)

# Largest Excess/No Excess
munis <- ts |> filter(level == 8, is_cumulative == TRUE)
munis |>
  filter(any(cmr_excess_sign) == FALSE) |>
  arrange(desc(population))
munis |>
  filter(any(cmr_excess_sign) == TRUE) |>
  arrange(desc(population))
