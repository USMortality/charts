source("lib/common.r")
source("mortality/deu/mortality_states.r")

df <- deu_mortality_states |>
  filter(age_group == "all") |>
  mutate(date = yearmonth(date), cmr = deaths / population * 100000) |>
  group_by(date) |>
  summarise(cmr = sum(cmr)) |>
  as_tsibble(index = date) |>
  filter(row_number() <= n() - 2) # remove last incomplete month.

training_df <- df |> filter(date < make_yearmonth(year = 2020, month = 3))
fit <- training_df |>
  model(
    ets = ETS(cmr),
    arima = ARIMA(cmr),
    lm = TSLM(cmr ~ trend() + season())
  ) |>
  mutate(
    average = (ets + arima + lm) / 3
  )

fit |>
  forecast(h = "4 years") |>
  # filter(.model == "lm") |>
  autoplot(df, level = 95) +
  facet_wrap(vars(.model), scales = "free")
