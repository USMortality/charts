source("lib/common.r")

# Excess Deaths
who <- read_excel(
  "./data_static/WHO_COVID_Excess_Deaths_EstimatesByRegion.xlsx",
  sheet = "Deaths by year and month",
  range = "A12:J180"
)

deaths <- who |>
  filter(location == "Global") |>
  mutate(date = make_yearmonth(year = year, month = month)) |>
  select(date, `excess.mean*`) |>
  setNames(c("date", "excess_deaths"))

# Vaccinations
df2 <- read.csv("./data/owid.csv") |> as_tibble()

vaxx <- df2 |>
  filter(iso_code == "OWID_WRL") |>
  select(date, new_vaccinations_smoothed) |>
  mutate(date = yearmonth(date(date))) |>
  group_by(date) |>
  summarise(new_vaccinations_smoothed = sum(
    new_vaccinations_smoothed,
    na.rm = TRUE
  ))

df <- deaths |> inner_join(vaxx, by = c("date"))

coeff <- max(df$new_vaccinations_smoothed) / max(df$excess_deaths)
ggplot(df, aes(x = date)) +
  labs(
    title = "Excess Deaths & COVID-19 Vaccine Doses [World]",
    subtitle = "Source: World Health Organization (WHO) | Our World In Data (OWID)",
    y = "COVID-19 Cases",
    x = "Date"
  ) +
  geom_col(aes(y = excess_deaths), fill = "red") +
  geom_line(
    aes(y = new_vaccinations_smoothed / coeff),
    color = "black", size = 2
  ) +
  twitter_theme() +
  watermark() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5),
    legend.position = "top",
    legend.title = element_blank(),
    axis.title.y = element_text(color = "red"),
    axis.title.y.right = element_text(color = "black")
  ) +
  scale_x_yearweek(date_breaks = "12 weeks", date_labels = "%Y W%W") +
  scale_y_continuous(
    name = "Excess Deaths",
    labels = label_number(suffix = "M", scale = 1e-6),
    sec.axis = sec_axis(~ . * coeff,
      name = "COVID-19 Vaccine Doses",
      labels = label_number(suffix = "B", scale = 1e-9)
    )
  )
