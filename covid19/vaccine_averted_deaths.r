source("lib/common.r")

data <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
df <- as_tibble(data) %>%
  filter(iso_code == "DEU") %>%
  select(
    date,
    new_deaths_smoothed, new_cases_smoothed,
    people_fully_vaccinated_per_hundred
  ) %>%
  mutate(cfr = new_deaths_smoothed / new_cases_smoothed) %>%
  mutate(vaxxed = people_fully_vaccinated_per_hundred / 100) %>%
  mutate(unvaxxed = 1 - vaxxed) %>%
  mutate(
    cfr_unvaxx =
      ((new_deaths_smoothed * vaxxed * 1 / (1 - .9))
      + (new_deaths_smoothed * unvaxxed))
      / new_cases_smoothed
  ) %>%
  mutate(deaths_no_vaxx = new_cases_smoothed * cfr_unvaxx) %>%
  mutate(date = date(date)) %>%
  select(date, new_deaths_smoothed, deaths_no_vaxx) %>%
  as_tsibble(index = date)

df2 <- df %>%
  setNames(c("Date", "Actual Deaths", "Model fit without vaccines")) %>%
  pivot_longer(!Date)

ggplot(
  df2,
  aes(x = Date, y = value, group_name = name, color = name)
) +
  labs(
    title = "COVID-19 Flawed Vaccine Saved Lifes Model [Germany]",
    subtitle = "Source: OWID; WARNING: FLAWED MODEL",
    y = "COVID-19 Deaths",
    x = "Date"
  ) +
  geom_line(linewidth = 1.2) +
  twitter_theme() +
  watermark(df$date, df$cmr) +
  scale_x_yearquarter(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
  theme(legend.position = "top") +
  scale_color_manual(values = c("black", "red")) +
  scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3))

# Cumulative
df$saved_lifes <- as.numeric(round(df$deaths_no_vaxx - df$new_deaths_smoothed))
df <- df %>% filter(!is.na(saved_lifes))
df$cum_actual_deaths <- cumsum(df$new_deaths_smoothed)
df$cum_saved_lifes <- df$cum_actual_deaths + cumsum(df$saved_lifes)

df2 <- df %>%
  select(date, cum_actual_deaths, cum_saved_lifes) %>%
  setNames(c("Date", "Actual Deaths", "Model fit without vaccines")) %>%
  pivot_longer(!Date)

ggplot(
  df2,
  aes(x = Date, y = value, group_name = name, color = name)
) +
  labs(
    title = "COVID-19 Flawed Vaccine Saved Lifes Model [Germany]",
    subtitle = "Source: OWID; WARNING: FLAWED MODEL",
    y = "COVID-19 Deaths",
    x = "Date"
  ) +
  geom_line(linewidth = 1.2) +
  twitter_theme() +
  watermark(df$date, df$cmr) +
  scale_x_yearquarter(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
  theme(legend.position = "top") +
  scale_color_manual(values = c("black", "red")) +
  scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3))
