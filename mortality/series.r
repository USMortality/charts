source("lib/common.r")

data_monthly <- as_tibble(read.csv("./out/mortality/world_monthly.csv"))

get_data <- function(countries, year = 1900) {
  data_monthly %>%
    filter(name %in% countries) %>%
    mutate(date = yearmonth(date)) %>%
    mutate(Country = name) %>%
    group_by(iso3c) %>%
    mutate(sma = SMA(mortality, n = 12)) %>%
    ungroup() %>%
    filter(year(date) >= year) %>%
    filter(!is.na(sma))
}

make_chart <- function(countries, title) {
  data <- get_data(countries, 2000)
  ggplot(
    data,
    aes(x = date, y = sma, group_name = Country, color = Country)
  ) +
    labs(
      title = title,
      subtitle = "Source: github.com/USMortality/charts",
      y = "Deaths/100k",
      x = "Month of Year"
    ) +
    geom_line(linewidth = 1.5) +
    geom_smooth(
      data = data %>% filter(year(date) < 2020),
      method = "lm_right", fullrange = TRUE, se = FALSE, level = .99,
      linetype = "dashed"
    ) +
    twitter_theme() +
    watermark(df$date, df$mortality) +
    scale_x_yearquarter(date_breaks = "1 year", date_labels = "%Y") +
    theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
}

# Sweden's Neighbors
countries <- c("Sweden", "Norway", "Iceland", "Denmark", "Finland")
chart <- make_chart(countries, "Monthly Mortality in the Nordics (12M SMA)")
save_chart(chart, paste("mortality_series/Nordics", sep = "/"))

# Germany's Neighbors
countries <- c(
  "Germany", "Austria", "Belgium", "Denmark", "France", "Luxembourg",
  "Netherlands"
)
chart <- make_chart(
  countries,
  "Monthly Mortality of Germany's Neighbors (12M SMA) [North/West]"
)
save_chart(chart, paste("mortality_series/Germany_Neighbors_1", sep = "/"))

countries <- c("Germany", "Austria", "Poland", "Switzerland", "Czech Republic")
chart <- make_chart(
  countries,
  "Monthly Mortality of Germany's Neighbors (12M SMA) [South/East]"
)
save_chart(chart, paste("mortality_series/Germany_Neighbors_2", sep = "/"))

countries <- c("United States", "Canada", "Mexico")
chart <- make_chart(
  countries,
  "Monthly Mortality of United States's Neighbors (12M SMA)"
)
save_chart(chart, paste("mortality_series/USA_Neighbors", sep = "/"))

countries <- c("Sweden", "New Zealand", "Taiwan", "Australia", "Japan")
chart <- make_chart(
  countries,
  "Monthly Mortality Sweden vs. 'Island Nations' (12M SMA)"
)
save_chart(chart, paste("mortality_series/Sweden_Island_Nations", sep = "/"))
