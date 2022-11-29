source("lib/common.r")

data_monthly <- as_tibble(read.csv("./out/mortality/world_monthly.csv"))

# Sweden's Neighbors
countries <- c("Sweden", "Norway", "Iceland", "Denmark", "Finland")
df <- data_monthly %>%
  filter(name %in% countries) %>%
  mutate(date = yearmonth(date)) %>%
  filter(year(date) >= 2010) %>%
  mutate(Country = name)

chart <-
  ggplot(
    df,
    aes(x = date, y = mortality, group_name = Country, color = Country)
  ) +
  labs(
    title = paste0("Monthly Mortality in the Nordics"),
    subtitle = "Source: github.com/USMortality/charts",
    y = "Deaths/100k",
    x = "Month of Year"
  ) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = loess, se = FALSE) +
  twitter_theme() +
  watermark(df$date, df$mortality) +
  scale_x_yearquarter(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))

save_chart(chart, paste("mortality/nordics", sep = "/"))

# Germany's Neighbors
countries <- c(
  "Germany", "Austria", "Belgium", "Czech Republic", "Denmark", "France",
  "Luxembourg", "Netherlands", "Poland", "Switzerland"
)

df <- data_monthly %>%
  filter(name %in% countries) %>%
  mutate(date = yearmonth(date)) %>%
  filter(year(date) >= 2010) %>%
  mutate(Country = name)

chart <-
  ggplot(
    df,
    aes(x = date, y = mortality, group_name = Country, color = Country)
  ) +
  labs(
    title = paste0("Monthly Mortality in Germany & Neighbors"),
    subtitle = "Source: github.com/USMortality/charts",
    y = "Deaths/100k",
    x = "Month of Year"
  ) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = loess, se = FALSE) +
  twitter_theme() +
  watermark(df$date, df$mortality) +
  scale_x_yearquarter(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))

save_chart(chart, paste("mortality/germany_neighbors", sep = "/"))
