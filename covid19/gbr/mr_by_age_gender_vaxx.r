source("lib/common.r")
options(vsc.dev.args = list(width = 1920, height = 1080, res = 72 * sf))

make_chart <- function(data, title) {
  ggplot(data, aes(
    x = date, y = asmr_py, color = sex, group = interaction(vaccination_status, sex)
  )) +
    geom_line(aes(linetype = vaccination_status, color = sex)) +
    # geom_point(aes(color = sex)) +
    labs(
      title = title,
      subtitle = "Source: ons.gov.uk",
      x = "Month of Year",
      y = "Mortality rate / 100,000 person-years"
    ) +
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
  sheet = "Table 4",
  range = "A4:L9999"
)

df <- data |>
  select(1, 2, 3, 4, 5, 6, 9) |>
  setNames(c(
    "sex",
    "type",
    "year",
    "month",
    "age_group",
    "vaccination_status",
    "asmr_py"
  )) |>
  mutate(date = make_yearmonth(
    year = year,
    month = match(month, month.name)
  ), .after = type) |>
  mutate(asmr_py = as.double(asmr_py)) |>
  select(-year, -month)

# Unvaccinated vs First
data <- df |>
  filter(
    type == "All causes",
    # type == "Deaths involving COVID-19",
    vaccination_status %in% c(
      "Unvaccinated",
      "First dose, at least 21 days ago"
    )
  ) |>
  arrange(age_group, sex, date, vaccination_status)
make_chart(data, "Monthly All-Cause AMSR/PY by Sex & Vaccination Status [England]")

# Unvaccinated vs Second
data <- df |>
  filter(
    type == "All causes",
    # type == "Deaths involving COVID-19",
    vaccination_status %in% c(
      "Unvaccinated",
      "Second dose, at least 21 days ago"
    ),
    # age_group == "80-89"
  ) |>
  arrange(age_group, sex, date, vaccination_status)
make_chart(data, "Monthly All-Cause AMSR/PY by Sex & Vaccination Status [England]")

# Unvaccinated vs Boosted
data <- df |>
  filter(
    type == "All causes",
    # type == "Deaths involving COVID-19",
    vaccination_status %in% c(
      "Unvaccinated",
      "Third dose or booster, at least 21 days ago"
    )
  ) |>
  arrange(age_group, sex, date, vaccination_status)
make_chart(data, "Monthly All-Cause AMSR/PY by Sex & Vaccination Status [England]")

# 80-89
sf = 3
options(vsc.dev.args = list(width = 1920, height = 1080, res = 72 * sf))
data <- df |>
  filter(
    type == "All causes",
    # type == "Deaths involving COVID-19",
    vaccination_status %in% c(
      "Unvaccinated",
      "Second dose, at least 21 days ago"
    ),
    age_group == "80-89"
  ) |>
  arrange(age_group, sex, date, vaccination_status)
make_chart(data, "Monthly All-Cause AMSR/PY by Sex & Vaccination Status [England]")
