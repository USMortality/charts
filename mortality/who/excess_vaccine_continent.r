source("lib/common.r")

# Excess Deaths
who <- read_excel(
  "./data_static/WHO_COVID_Excess_Deaths_EstimatesByRegion.xlsx",
  sheet = "Deaths by year and month",
  range = "A12:J180"
)

deaths <- who |>
  mutate(location = case_when(
    location == "EUR" ~ "Europe",
    location == "AMR" ~ "America",
    location == "AFR" ~ "Africa",
    location %in% c("EMR", "WPR", "SEAR") ~ "Asia_Australia",
    .default = "other"
  )) |>
  filter(location != "other") |>
  mutate(date = make_yearmonth(year = year, month = month)) |>
  select(location, date, `excess.mean*`) |>
  setNames(c("location", "date", "excess_deaths")) |>
  group_by(location, date) |>
  summarise(excess_deaths = sum(excess_deaths, na.rm = TRUE))

# Vaccinations
df2 <- read.csv("./data/owid.csv") |> as_tibble()

vaxx <- df2 |>
  mutate(location = case_when(
    iso_code == "OWID_EUR" ~ "Europe",
    iso_code %in% c("OWID_NAM", "OWID_SAM") ~ "America",
    iso_code == "OWID_AFR" ~ "Africa",
    iso_code %in% c("OWID_ASI", "OWID_OCE") ~ "Asia_Australia",
    .default = "other"
  )) |>
  filter(location != "other") |>
  select(location, date, new_vaccinations_smoothed) |>
  mutate(date = yearmonth(date(date))) |>
  group_by(location, date) |>
  summarise(new_vaccinations_smoothed = sum(
    new_vaccinations_smoothed,
    na.rm = TRUE
  ))

df <- deaths |>
  inner_join(vaxx, by = c("date", "location")) |>
  filter(location == "Asia_Australia")

coeff <- max(df$new_vaccinations_smoothed) / max(df$excess_deaths)
ggplot(df, aes(x = date)) +
  labs(
    title = "Excess Deaths & COVID-19 Vaccine Doses [Asia_Australia]",
    subtitle = paste(
      "Source:",
      "World Health Organization (WHO)",
      "| Our World In Data (OWID)"
    ),
    y = "COVID-19 Cases",
    x = "Month of Year"
  ) +
  geom_col(aes(y = excess_deaths), fill = "red") +
  geom_line(
    aes(y = new_vaccinations_smoothed / coeff),
    color = "black", linewidth = 2
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
  scale_x_yearmonth(date_breaks = "2 months") +
  scale_y_continuous(
    name = "Excess Deaths",
    labels = label_number(suffix = "M", scale = 1e-6),
    sec.axis = sec_axis(~ . * coeff,
      name = "COVID-19 Vaccine Doses",
      labels = label_number(suffix = "B", scale = 1e-9)
    )
  )
