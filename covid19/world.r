source("lib/common.r")

data <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
df <- as_tibble(data) %>%
  filter(iso_code == "ISR") %>%
  select(date, new_cases_smoothed, new_vaccinations_smoothed) %>%
  mutate(date = date(date)) %>%
  as_tsibble(index = date) %>%
  # filter(date < date("2021-12-31")) %>%
  mutate(new_vaccinations_smoothed = as.numeric(new_vaccinations_smoothed) / 10) %>%
  pivot_longer(!date)

ggplot(
  df,
  aes(x = date, y = value, group_name = name, color = name)
) +
  labs(
    title = "Israel COVID-19 Cases & Vaccine Doses",
    subtitle = "Source: OWID",
    # y = "",
    x = "Date"
  ) +
  geom_line(linewidth = 1.2) +
  twitter_theme() +
  watermark(df$date, df$cmr) +
  scale_x_yearquarter(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
