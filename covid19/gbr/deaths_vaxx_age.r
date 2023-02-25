source("lib/common.r")

source("covid19/gbr/vaxx.r")
source("covid19/gbr/deaths_by_vaxx.r")

options(vsc.dev.args = list(width = 1920, height = 1080, res = 72 * sf))

deaths <- df |>
  filter(type == "All causes") |>
  select(-type) |>
  dplyr::arrange(date, age_group, vaccination_status) |>
  group_by(date) |>
  nest() |>
  mutate(data = map(data, ~ .x |> adorn_totals("row", name = "all"))) |>
  unnest(cols = c(data))

d <- deaths |>
  group_by(date, age_group) |>
  summarise(deaths = sum(deaths))

v <- vaxxed_population |> select(date, age_group, vaxxed_pct)

data <- d |> inner_join(v, by = c("date", "age_group"))


# All ages
all_ages <- data |> filter(age_group == "all")
max_p <- 1
max_d <- max(all_ages$deaths)

ggplot(
  all_ages,
  aes(x = date, y = deaths)
) +
  labs(
    title = "All-Cause Deaths & Cumulative COVID-19 Vaccination [England]",
    subtitle = "Source: ons.gov.uk",
    x = "Month of Year",
    y = "Deaths"
  ) +
  geom_col(aes(y = deaths), fill = "black") +
  geom_smooth(
    method = "lm", color = "blue",
    fill = "#0000ff", alpha = 0.3
  ) +
  geom_line(aes(
    y = vaxxed_pct / (max_p / max_d),
    linetype = "Vaccinationd Population"
  ), linewidth = 2, color = "#ff0000") +
  scale_y_continuous(
    labels = label_number(suffix = "k", scale = 1e-3),
    sec.axis = sec_axis(
      ~ . * (max_p / max_d),
      name = "Vaccinated (1st)",
      labels = scales::label_percent()
    )
  ) +
  twitter_theme() +
  scale_color_discrete(name = "") +
  scale_x_yearmonth(date_breaks = "1 months", date_labels = "%Y/%m") +
  theme(
    # panel.spacing = unit(0.3, "in"),
    legend.position = "top",
    axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5),
    legend.title = element_blank()
  )

df <- all_ages |>
  mutate(year = year(date), month = month(date)) |>
  reshape2::dcast(month ~ year, value.var = "deaths") |>
  mutate(change = `2022` / `2021` - 1)

ggplot(df, aes(x = month, y = change, fill = change)) +
  labs(
    title = "Change in Monthly All Cause Deaths 2022 vs 2021 [England]",
    subtitle = "Source: ons.gov.uk",
    x = "Month of Year",
    y = "Change from 2021 to 2022"
  ) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(
    low = "darkgreen",
    mid = "snow3",
    high = "darkred",
    space = "Lab"
  ) +
  twitter_theme() +
  scale_y_continuous(
    labels = scales::percent_format(scale = 100),
    # limits = c(-1, 1)
  ) +
  theme(
    # panel.spacing = unit(0.3, "in"),
    legend.position = "none",
    axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)
  )

# By age group

make_chart <- function(data, age_group) {
  max_d <- max(df$deaths)
  ggplot(
    data,
    aes(x = date, y = deaths, fill = age_group)
  ) +
    labs(
      title = "All-Cause Deaths & Cumulative COVID-19 Vaccination [England]",
      subtitle = paste0("Age-Group: ", age_group, " | Source: ons.gov.uk"),
      x = "Month of Year",
      y = "Deaths"
    ) +
    geom_col(aes(y = deaths), fill = "black") +
    geom_line(aes(
      y = vaxxed_pct / (max_p / max_d),
      linetype = "Vaccinatinated Population"
    ), linewidth = 2, color = "#ff0000") +
    scale_y_continuous(
      labels = label_number(suffix = "k", scale = 1e-3),
      sec.axis = sec_axis(
        ~ . * (max_p / max_d),
        name = "Vaccinated (1st)",
        labels = scales::label_percent()
      )
    ) +
    twitter_theme() +
    scale_color_discrete(name = "") +
    scale_x_yearmonth(date_breaks = "1 months", date_labels = "%Y/%m") +
    theme(
      # panel.spacing = unit(0.3, "in"),
      legend.position = "top",
      axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5),
      legend.title = element_blank()
    )
}

make_chart2 <- function(data, age_group) {
  ggplot(data, aes(x = month, y = change, fill = change)) +
    labs(
      title = "Change in Monthly All Cause Deaths 2022/2021 [England]",
      subtitle = paste0("Age-Group: ", age_group, " | Source: ons.gov.uk"),
      x = "Month of Year",
      y = "Change from 2021 to 2022"
    ) +
    geom_bar(stat = "identity") +
    scale_fill_gradient2(
      low = "darkgreen",
      mid = "snow3",
      high = "darkred",
      space = "Lab"
    ) +
    twitter_theme() +
    scale_x_yearmonth(date_breaks = "1 months", date_labels = "%m") +
    scale_y_continuous(
      labels = scales::percent_format(scale = 100),
      limits = c(-.6, .6)
    ) +
    theme(
      # panel.spacing = unit(0.3, "in"),
      legend.position = "none",
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
    mutate(
      change = `2022` / `2021` - 1,
      month = make_yearmonth(year = 2022, month = month)
    )
  chart <- make_chart2(df, ag)
  save_chart(chart, paste0("acd_vaxx_chg", ag), 2, FALSE)
}
