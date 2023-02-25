source("lib/common.r")

source("covid19/gbr/deaths_by_vaxx.r")
deaths <- df |>
  filter(type == "All causes") |>
  select(-type)

# Deaths
ve <- deaths |>
  inner_join(vaxxed, by = c("date", "age_group")) |>
  mutate(
    population = ifelse(
      vaccination_status == "Unvaccinated",
      1 - vaxxed,
      vaxxed
    ),
    population_lower = ifelse(
      vaccination_status == "Unvaccinated",
      1 - vaxxed_lower,
      vaxxed_lower
    ),
    population_upper = ifelse(
      vaccination_status == "Unvaccinated",
      1 - vaxxed_upper,
      vaxxed_upper
    )
  ) |>
  select(-vaxxed, -vaxxed_lower, -vaxxed_upper) |>
  pivot_wider(
    names_from = vaccination_status,
    values_from = c(deaths, population, population_lower, population_upper)
  ) |>
  set_names(c(
    "date",
    "age_group",
    "deaths_unvaxx",
    "deaths_vaxx",
    "population_unvaxx",
    "population_vaxx",
    "population_unvaxx_lower",
    "population_vaxx_lower",
    "population_unvaxx_upper",
    "population_vaxx_upper"
  )) |>
  mutate(
    ve = 1 - (deaths_vaxx / population_vaxx) /
      (deaths_unvaxx / population_unvaxx),
    ve_lower = 1 - (deaths_vaxx / population_vaxx_lower) /
      (deaths_unvaxx / population_unvaxx_lower),
    ve_upper = 1 - (deaths_vaxx / population_vaxx_upper) /
      (deaths_unvaxx / population_unvaxx_upper)
  ) |>
  select(date, age_group, ve, ve_lower, ve_upper)

ggplot(
  ve,
  aes(x = date, y = ve, group = age_group, color = age_group)
) +
  labs(
    title = "COVID-19 Vaccine Efficacy (Any Death) [England]",
    subtitle = "Source: ons.gov.uk",
    x = "Month of Year",
    y = "Vaccine Efficacy"
  ) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(
    ymin = ve_lower,
    ymax = ve_upper,
    fill = age_group
  ), alpha = .3, linetype = 0) +
  twitter_theme() +
  geom_hline(yintercept = 0) +
  watermark(df$yearmonth, df$value_p) +
  scale_x_yearmonth(date_breaks = "1 months", date_labels = "%Y/%m") +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(-1, 1)
  ) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5),
    legend.position = "top"
  )

options(vsc.dev.args = list(width = 1920, height = 1080, res = 72 * sf))
ggplot(
  ve,
  aes(x = date, y = ve, group = age_group, color = age_group)
) +
  labs(
    title = "COVID-19 Vaccine Efficacy (Any Death) [England]",
    subtitle = "Source: ons.gov.uk",
    x = "Month of Year",
    y = "Vaccine Efficacy"
  ) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(
    ymin = ve_lower,
    ymax = ve_upper,
    fill = age_group
  ), alpha = .3, linetype = 0) +
  twitter_theme() +
  geom_hline(yintercept = 0) +
  watermark(df$yearmonth, df$value_p) +
  scale_x_yearmonth(date_breaks = "2 months", date_labels = "%Y/%m") +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(-1, 1)
  ) +
  facet_wrap(vars(age_group), scales = "free") +
  theme(
    panel.spacing = unit(0.3, "in"),
    legend.position = "top"
  ) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5),
    legend.position = "top"
  )
