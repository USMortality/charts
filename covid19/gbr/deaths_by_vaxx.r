source("lib/common.r")
options(vsc.dev.args = list(width = 1920, height = 1080, res = 72 * sf))

make_chart <- function(data, title) {
  ggplot(
    data,
    aes(x = date, y = deaths, group = vaxx_status, color = vaxx_status)
  ) +
    labs(
      title = title,
      subtitle = "Source: ons.gov.uk",
      x = "Month of Year",
      y = "Deaths"
    ) +
    geom_line(linewidth = 1) +
    twitter_theme() +
    watermark(df$yearmonth, df$value_p) +
    scale_x_yearmonth(date_breaks = "2 months", date_labels = "%Y/%m") +
    scale_y_continuous(
      labels = label_number(suffix = "k", scale = 1e-3),
      # limits = c(0, 2500)
    ) +
    facet_wrap(vars(age_group), scales = "free") +
    theme(
      # panel.spacing = unit(0.3, "in"),
      legend.position = "top",
      axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)
    )
}

data <- read_excel(
  "./data/uk_acm_vaxx.xlsx",
  sheet = "Table 5",
  range = "A4:F591"
)

df <- data |>
  setNames(c(
    "type",
    "year",
    "month",
    "age_group",
    "vaxx_status",
    "deaths"
  )) |>
  mutate(date = make_yearmonth(
    year = year,
    month = match(month, month.name)
  ), .after = type) |>
  mutate(deaths = as.integer(deaths)) |>
  select(-year, -month)

# All cause deaths by vaxx
acm <- df |>
  filter(
    type == "All causes",
    vaxx_status %in% c("Unvaccinated", "Ever vaccinated")
  ) |>
  select(-type)
make_chart(acm, "Monthly All-Cause Deaths by Vaxx [UK]")

# COVID-19 deaths by vaxx
covid <- df |>
  filter(
    type == "Deaths involving COVID-19",
    vaxx_status %in% c("Unvaccinated", "Ever vaccinated")
  ) |>
  select(-type)
make_chart(covid, "Monthly COVID-19 Deaths by Vaxx [UK]")

colnames(covid)[4] <- "covid_deaths"
non_covid <- acm |>
  inner_join(covid, by = c("date", "age_group", "vaxx_status")) |>
  mutate(non_covid_deaths = deaths - ifelse(
    is.na(covid_deaths), 0, covid_deaths
  )) |>
  select(-deaths, -covid_deaths)
colnames(non_covid)[4] <- "deaths"

make_chart(non_covid, "Monthly Non-COVID-19 Deaths by Vaxx [UK]")
