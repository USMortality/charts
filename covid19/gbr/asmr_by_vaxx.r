source("lib/common.r")

make_chart <- function(data, title) {
  ggplot(
    data,
    aes(x = date, y = asmr_py, group = vaxx_status, color = vaxx_status)
  ) +
    labs(
      title = title,
      subtitle = "Dec 2022 Release | Jan-March 2021 now omitted by ONS | Source: ons.gov.uk",
      x = "Month of Year",
      y = "Age-standardised mortality rate / 100,000 person-years"
    ) +
    geom_line(linewidth = 1) +
    geom_ribbon(aes(
      ymin = asmr_py_lower,
      ymax = asmr_py_upper,
      fill = vaxx_status
    ), alpha = .3, linetype = 0) +
    twitter_theme() +
    watermark(df$yearmonth, df$value_p) +
    scale_x_yearmonth(date_breaks = "1 months", date_labels = "%Y/%m") +
    scale_y_continuous(
      labels = label_number(suffix = "k", scale = 1e-3),
      # limits = c(0, 6000)
    ) +
    theme(
      legend.position = "top",
      axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)
    )
}

data <- read_excel(
  # "./data/uk_acm_vaxx_old.xlsx",
  "./data/uk_acm_vaxx.xlsx",
  sheet = "Table 1",
  range = "A4:J9999"
)

df <- data |>
  select(1, 2, 3, 4, 7, 9, 10) |>
  setNames(c(
    "type",
    "year",
    "month",
    "vaxx_status",
    "asmr_py",
    "asmr_py_lower",
    "asmr_py_upper"
  )) |>
  mutate(date = make_yearmonth(
    year = year,
    month = match(month, month.name)
  ), .after = type) |>
  mutate(
    asmr_py = as.double(asmr_py),
    asmr_py_lower = as.double(asmr_py_lower),
    asmr_py_upper = as.double(asmr_py_upper)
  ) |>
  select(-year, -month)

# All causes
acm <- df |>
  filter(
    type == "All causes",
    vaxx_status %in% c("Unvaccinated", "Ever vaccinated")
  ) |>
  select(-type)
make_chart(acm, "Monthly All-Cause ASMR-PY by Vaxx [UK]")

# Non-COVID-19 deaths
acm_non_covid <- df |>
  filter(
    type == "Non-COVID-19 deaths",
    vaxx_status %in% c("Unvaccinated", "Ever vaccinated")
  ) |>
  select(-type)
make_chart(acm_non_covid, "Monthly Non-COVID-19 ASMR-PY by Vaxx [UK]")

# COVID-19 deaths
covid <- df |>
  filter(
    type == "Deaths involving COVID-19",
    vaxx_status %in% c("Unvaccinated", "Ever vaccinated")
  ) |>
  select(-type)
make_chart(covid, "Monthly COVID-19 ASMR-PY by Vaxx [UK]")


# Adjust rates for bias, via weekly ratio
ratio <- acm_non_covid |>
  select(1, 2, 3) |>
  pivot_wider(names_from = vaxx_status, values_from = asmr_py) |>
  mutate(ratio = Unvaccinated / `Ever vaccinated`) |>
  select(1, 4)

unvaxx_adjusted <- acm |>
  filter(vaxx_status == "Unvaccinated") |>
  inner_join(ratio) |>
  mutate(
    asmr_py = asmr_py / ratio,
    asmr_py_lower = asmr_py_lower / ratio,
    asmr_py_upper = asmr_py_upper / ratio
  ) |>
  select(-ratio)

acm_adjusted <- rbind(
  acm |> filter(vaxx_status == "Ever vaccinated"),
  unvaxx_adjusted
) |> arrange(date)
make_chart(acm_adjusted, "Monthly Adjusted All-Cause ASMR-PY by Vaxx [UK]")
