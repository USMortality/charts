source("lib/common.r")

source("covid19/gbr/vaxx.r")
source("covid19/gbr/deaths_by_vaxx.r")

# options(vsc.dev.args = list(width = 1920, height = 1080, res = 72 * sf))

deaths <- df |>
  filter(type == "All causes") |>
  select(-type)

vaxxed |>
  group_by(date) |>
  summarise(vaxxed_lower = sum(vaxxed_lower), vaxxed_upper = sum(vaxxed_upper))

d <- deaths |>
  group_by(date, age_group) |>
  summarise(deaths = sum(deaths))

v <- vaxxed_population |> select(date, age_group, vaxxed_pct)

data <- d |> inner_join(v, by = c("date", "age_group"))

max_p <- 1
max_d <- max(data$deaths)

ggplot(
  data,
  aes(x = date, group = age_group)
) +
  labs(
    title = "All-Cause Deaths & Cumulative COVID-19 Vaccination [England]",
    subtitle = "Source: ons.gov.uk",
    x = "Month of Year",
    y = "Deaths"
  ) +
  geom_col(aes(y = deaths)) +
  geom_line(aes(y = vaxxed_pct / (max_p / max_d), group = 1)) +
  scale_y_continuous(
    labels = label_number(suffix = "k", scale = 1e-3),
    sec.axis = sec_axis(
      ~ . * (max_p / max_d),
      name = "Vaccinated (1st)",
      labels = scales::label_percent()
    )
  ) +
  twitter_theme() +
  scale_x_yearmonth(date_breaks = "2 months", date_labels = "%Y/%m") +
  facet_wrap(
    vars(age_group),
    #  scales = "free"
  ) +
  theme(
    # panel.spacing = unit(0.3, "in"),
    legend.position = "top",
    axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)
  )

make_chart <- function(data, age_group) {
  max_p <- 1
  max_d <- max(df$deaths)
  ggplot(
    data,
    aes(x = date, y = deaths)
  ) +
    labs(
      title = "All-Cause Deaths & Cumulative COVID-19 Vaccination [England]",
      subtitle = paste0("Age-Group | ", age_group, " | Source: ons.gov.uk"),
      x = "Month of Year",
      y = "Deaths"
    ) +
    # geom_line(aes(y = deaths), linewidth = 2, color = "darkblue") +
    geom_col(aes(y = deaths), fill = "darkblue") +
    geom_smooth(method = "lm") +
    geom_line(aes(y = vaxxed_pct / (max_p / max_d)), linewidth = 2, color = "#ff0000") +
    scale_y_continuous(
      # labels = label_number(suffix = "k", scale = 1e-3),
      sec.axis = sec_axis(
        ~ . * (max_p / max_d),
        name = "Vaccinated (1st)",
        labels = scales::label_percent()
      )
    ) +
    twitter_theme() +
    scale_x_yearmonth(date_breaks = "2 months", date_labels = "%Y/%m") +
    theme(
      # panel.spacing = unit(0.3, "in"),
      legend.position = "top",
      axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)
    )
}

make_chart2 <- function(data, age_group) {
  ggplot(data, aes(x = month, y = change, fill = change)) +
    labs(
      title = "Change in Monthly All Cause Deaths 2022/2021 [England]",
      subtitle = paste0("Age-Group | ", age_group, " | Source: ons.gov.uk"),
      x = "Month of Year",
      y = "Change from 2021 to 2022"
    ) +
    geom_bar(stat = "identity") +
    scale_fill_gradient2(low = "darkgreen", mid = "snow3", high = "darkred", space = "Lab") +
    twitter_theme() +
    scale_x_yearmonth(date_breaks = "2 months", date_labels = "%Y/%m") +
    scale_y_continuous(
      labels = scales::percent_format(scale = 100),
      limits = c(-1, 1)
    ) +
    theme(
      # panel.spacing = unit(0.3, "in"),
      legend.position = "top",
      axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)
    )
}

for (ag in unique(data$age_group)) {
  df <- data |> filter(age_group == ag)
  chart <- make_chart(df, ag)
  save_chart(chart, paste0("acd_vaxx_", ag), 2, FALSE)
}

# Month to Monty, Yearly increase
for (ag in unique(data$age_group)) {
  df <- data |>
    filter(age_group == ag) |>
    mutate(year = year(date), month = month(date)) |>
    reshape2::dcast(month ~ year, value.var = "deaths") |>
    mutate(change = `2022` / `2021` - 1)
  chart <- make_chart2(df, ag)
  save_chart(chart, paste0("acd_vaxx_chg", ag), 2, FALSE)
}
