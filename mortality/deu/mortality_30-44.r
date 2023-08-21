source("lib/common.r")
source("mortality/deu/mortality_states.r")

df <- deu_mortality_states |>
  filter(age_group == "30-44") |>
  mutate(date = yearmonth(date), cmr = deaths / population * 100000) |>
  group_by(date) |>
  summarise(cmr = sum(cmr)) |>
  as_tsibble(index = date) |>
  filter(row_number() <= n() - 1) # remove last incomplete month.

training_df <- df |> filter(date < make_yearmonth(year = 2020, month = 3))
fit <- training_df |>
  model(
    ets = ETS(cmr),
    arima = ARIMA(cmr),
    lm = TSLM(cmr ~ trend() + season())
  )

fit |>
  forecast(h = "4 years") |>
  autoplot(df, level = 95)
