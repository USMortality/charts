source("lib/common.r")
source("mortality/deu/mortality_states.r")
source("lib/asmr.r")

df <- deu_mortality_states |>
  filter(iso3c == "DEU", age_group != "all") |>
  mutate(date = year(date), cmr = deaths / population * 100000) |>
  group_modify(~ calculate_asmr_variants(.x), .keep = TRUE) |>
  ungroup() |>
  as_tsibble(index = date) |>
  filter(row_number() <= n() - 1) # remove last incomplete month.

training_df <- df |> filter(date < 2020)
fit <- training_df |>
  model(
    ets = ETS(asmr_country),
    arima = ARIMA(asmr_country),
    lm = TSLM(asmr_country ~ trend())
  ) |>
  mutate(
    average = (ets + arima + lm) / 3
  )

fit |>
  forecast(h = "4 years") |>
  # filter(.model == "lm") |>
  autoplot(df, level = 95) +
  facet_wrap(vars(.model), scales = "free")
