source("lib/common.r")
options(vsc.dev.args = list(width = 1920, height = 1080, res = 72 * sf))

make_chart <- function(data, title) {
  ggplot(
    data,
    aes(x = date, y = rate_py, group = vaxx_status, color = vaxx_status)
  ) +
    labs(
      title = title,
      subtitle = "Source: ons.gov.uk",
      x = "Month of Year",
      y = "Mortality rate / 100,000 person-years"
    ) +
    geom_line(linewidth = 1) +
    geom_ribbon(aes(
      ymin = rate_py_lower,
      ymax = rate_py_upper,
      fill = vaxx_status
    ), alpha = .3, linetype = 0) +
    twitter_theme() +
    watermark(df$yearmonth, df$value_p) +
    scale_y_continuous(
      labels = label_number(suffix = "k", scale = 1e-3),
      # limits = c(0, 2500)
    ) +
    facet_wrap(vars(age_group), scales = "free") +
    theme(
      # panel.spacing = unit(0.3, "in"),
      legend.position = "top"
    )
}

data <- read_excel(
  "./data/uk_acm_vaxx.xlsx",
  sheet = "Table 2",
  range = "A4:K9999"
)

df <- data |>
  select(1, 2, 3, 4, 5, 8, 10, 11) |>
  setNames(c(
    "type",
    "year",
    "month",
    "age_group",
    "vaxx_status",
    "rate_py",
    "rate_py_lower",
    "rate_py_upper"
  )) |>
  mutate(date = make_yearmonth(
    year = year,
    month = match(month, month.name)
  ), .after = type) |>
  mutate(
    rate_py = as.double(rate_py),
    rate_py_lower = as.double(rate_py_lower),
    rate_py_upper = as.double(rate_py_upper)
  ) |>
  select(-year, -month)

# All causes
acm <- df |>
  filter(type == "All causes") |>
  select(-type)
make_chart(acm, "Monthly All-Cause Mortality Rate-PY by Vaxx [UK]")

# Non-COVID-19
acm_non_covid <- df |>
  filter(type == "Non-COVID-19 deaths") |>
  select(-type)
make_chart(acm_non_covid, "Monthly Non-COVID-19 Mortality Rate-PY by Vaxx [UK]")

# COVID-19 deaths
covid <- df |>
  filter(type == "Deaths involving COVID-19") |>
  select(-type)
make_chart(covid, "Monthly COVID-19 Mortality Rate-PY by Vaxx [UK]")
