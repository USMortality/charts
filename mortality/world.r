source("lib/common.r")

data_weekly <- as_tibble(read.csv("./out/mortality/world_weekly.csv"))
data_monthly <- as_tibble(read.csv("./out/mortality/world_monthly.csv"))
data_quarterly <- as_tibble(read.csv("./out/mortality/world_quarterly.csv"))
data_yearly <- as_tibble(read.csv("./out/mortality/world_yearly.csv"))

print("1) Weekly")
# for (country in unique(data_weekly$iso3c)) {
#   print(country)

#   df <- data_weekly %>%
#     filter(iso3c == country) %>%
#     mutate(date = yearweek(date)) %>%
#     as_tsibble(index = date)

#   chart1 <-
#     ggplot(df, aes(x = date, y = mortality)) +
#     labs(
#       title = paste0("Weekly Mortality [", country, "]"),
#       subtitle = "Datasources: United Nations, World Mortality Dataset",
#       y = "Deaths/100k",
#       x = "Week of Year"
#     ) +
#     geom_line(color = "#5383EC", linewidth = 1) +
#     twitter_theme() +
#     watermark(df$yearmonth, df$value_p)
#   save_chart(chart1, paste("mortality", country, "weekly_line", sep = "/"))
# }

print("2) Monthly")
for (country in unique(data_monthly$iso3c)) {
  print(country)
  df <- data_monthly %>%
    filter(iso3c == country) %>%
    mutate(date = yearmonth(date)) %>%
    as_tsibble(index = date)

  # chart2 <-
  ggplot(df, aes(x = date, y = mortality)) +
    labs(
      title = paste0("Monthly Mortality [", country, "]"),
      subtitle = "Datasources: United Nations, World Mortality Dataset",
      y = "Deaths/100k",
      x = "Month of Year"
    ) +
    geom_line(color = "#5383EC", linewidth = 1) +
    twitter_theme() +
    watermark(df$yearmonth, df$value_p)
  # save_chart(chart2, paste("mortality", country, "mortality_line", sep = "/"))
}

print("3) Quarterly")
for (country in unique(data_monthly$iso3c)) {
  print(country)
  df <- data_quarterly %>%
    filter(iso3c == country) %>%
    mutate(date = yearquarter(date)) %>%
    as_tsibble(index = date)

  # chart2 <-
  ggplot(df, aes(x = date, y = mortality)) +
    labs(
      title = paste0("Quarterly Mortality [", country, "]"),
      subtitle = "Datasources: United Nations, World Mortality Dataset",
      y = "Deaths/100k",
      x = "Month of Year"
    ) +
    geom_line(color = "#5383EC", linewidth = 1) +
    twitter_theme() +
    watermark(df$yearmonth, df$value_p)
  # save_chart(chart2, paste("mortality", country, "mortality_line", sep = "/"))
}

print("3) Quarterly")
for (country in unique(data_monthly$iso3c)) {
  print(country)
  df <- data_quarterly %>%
    filter(iso3c == country) %>%
    mutate(date = yearquarter(date)) %>%
    as_tsibble(index = date)

  # chart2 <-
  ggplot(df, aes(x = date, y = mortality)) +
    labs(
      title = paste0("Quarterly Mortality [", country, "]"),
      subtitle = "Datasources: United Nations, World Mortality Dataset",
      y = "Deaths/100k",
      x = "Month of Year"
    ) +
    geom_line(color = "#5383EC", linewidth = 1) +
    twitter_theme() +
    watermark(df$yearmonth, df$value_p)
  # save_chart(chart2, paste("mortality", country, "mortality_line", sep = "/"))
}

print("4) Yearly")
for (country in unique(data_monthly$iso3c)) {
  print(country)
  df <- data_yearly %>%
    filter(iso3c == country) %>%
    as_tsibble(index = date)

  # chart2 <-
  ggplot(df, aes(x = date, y = mortality)) +
    labs(
      title = paste0("Yearly Mortality [", country, "]"),
      subtitle = "Datasources: United Nations, World Mortality Dataset",
      y = "Deaths/100k",
      x = "Month of Year"
    ) +
    geom_line(color = "#5383EC", linewidth = 1) +
    twitter_theme() +
    watermark(df$yearmonth, df$value_p)
  # save_chart(chart2, paste("mortality", country, "mortality_line", sep = "/"))
}

print("6) STL Decomposition")
for (country in unique(data_monthly$iso3c)) {
  print(country)

  df <- data_weekly %>%
    filter(iso3c == country) %>%
    mutate(date = yearweek(date)) %>%
    as_tsibble(index = date) %>%
    model(STL(mortality)) %>%
    components()
  # chart2 <-
  autoplot(df, .vars = mortality) +
    labs(
      title = paste0("Weekly Mortality - STL Decomposition [", country, "]"),
      subtitle = "Datasources: United Nations, World Mortality Dataset",
      y = "Deaths/100k",
      x = "Week of Year"
    ) +
    twitter_theme() +
    watermark(df$yearmonth, df$value_p)
  save_chart(chart2, paste("mortality", country, "mortality_line", sep = "/"))
}

print("7) Yearly")
for (country in unique(data_monthly$iso3c)) {
  print(country)

  df <- data_yearly %>%
    filter(iso3c == country) %>%
    as_tsibble(index = date)

  ### Calculate Baseline & Excess w/o outliers
  min_year <- min(df$date)
  start_year <- ifelse(min_year > 2010, min_year, 2010)
  data_bl <- df %>%
    filter(date %in% seq(start_year, 2019)) %>%
    subset(!(mortality %in% boxplot.stats(mortality)$out))
  model <- lm(y ~ x, data = data.frame(x = data_bl$date, y = data_bl$mortality))
  forecast <- predict(model, newdata = data.frame(x = seq(start_year, max(df$date))))
  fc_df <- data.frame(date = seq(2010, max(df$date)), baseline = forecast)
  data <- df %>%
    inner_join(fc_df, by = "date") %>%
    mutate(excess = mortality - baseline) %>%
    mutate(excess_percent = excess / baseline)

  # chart3a <- 
  ggplot(data, aes(x = date, y = mortality)) +
    labs(
      title = paste0("Mortality [", country, "]"),
      subtitle = paste0(
        "Baseline: ", toString(data_bl$date), "; ",
        "Source: UN, World Mortality Dataset"
      ),
      y = "Deaths/100k",
      x = "Year"
    ) +
    twitter_theme() +
    watermark(df$yearmonth, df$value_p) +
    geom_col(fill = "#5383EC") +
    geom_abline(
      intercept = model$coefficients[1],
      slope = model$coefficients[2],
      linetype = "dashed",
      colour = "#00000077",
      size = 1
    ) +
    geom_text(
      aes(label = round(mortality, 1)),
      vjust = 2.5, colour = "#ffffff"
    ) +
    geom_text(
      aes(label = percent(excess_percent, accuracy = 0.1)),
      vjust = -.2
    ) +
    scale_y_continuous(labels = comma_format(decimal.mark = ","))

  save_chart(chart3a, paste("mortality", country, "3a", sep = "/"))
}
