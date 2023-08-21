source("lib/common.r")
source("mortality/_collection/mortality_org.r")

df <- mortality_org |>
  filter(iso3c == "DNK", age_group == "all") |>
  mutate(date = year(date), cmr = deaths / population * 100000) |>
  group_by(date) |>
  summarise(cmr = sum(cmr)) |>
  as_tsibble(index = date) |>
  filter(row_number() <= n() - 1) # remove last incomplete month.

training_df <- df |> filter(
  date >= 2010,
  date <= 2014
)
fit <- training_df |>
  model(
    ets = ETS(cmr),
    arima = ARIMA(cmr),
    lm = TSLM(cmr ~ trend())
  ) |>
  mutate(
    average = (ets + arima + lm) / 3
  )

accuracy(fit)

fit |>
  forecast(h = "8 years") |>
  filter(.model == "average") |>
  autoplot(df |> filter(), level = 95, alpha = 0.67)
