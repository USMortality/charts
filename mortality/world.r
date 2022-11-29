source("lib/common.r")

data_weekly <- as_tibble(read.csv("./out/mortality/world_weekly.csv"))
data_monthly <- as_tibble(read.csv("./out/mortality/world_monthly.csv"))
data_quarterly <- as_tibble(read.csv("./out/mortality/world_quarterly.csv"))
data_yearly <- as_tibble(read.csv("./out/mortality/world_yearly.csv"))
data_ytd <- as_tibble(read.csv("./out/mortality/world_ytd.csv"))
data_fluseason <- as_tibble(read.csv("./out/mortality/world_fluseason.csv"))

for (country in unique(data_weekly$iso3c)) {
  print(country)

  print("1) Weekly")
  df <- data_weekly %>%
    filter(iso3c == country) %>%
    mutate(date = yearweek(date)) %>%
    as_tsibble(index = date)

  chart1 <-
    ggplot(df, aes(x = date, y = mortality)) +
    labs(
      title = paste0("Weekly Mortality [", country, "]"),
      subtitle = "Source: github.com/USMortality/charts",
      y = "Deaths/100k",
      x = "Week of Year"
    ) +
    geom_line(color = "#5383EC", linewidth = 1) +
    twitter_theme() +
    watermark(df$yearmonth, df$value_p) +
    scale_x_yearweek(date_breaks = "1 year", date_labels = "%Y") +
    theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
  save_chart(chart1, paste("mortality", country, "weekly_line", sep = "/"))

  print("2) Monthly")
  df <- data_monthly %>%
    filter(iso3c == country) %>%
    mutate(date = yearmonth(date)) %>%
    as_tsibble(index = date)

  chart2 <-
    ggplot(df, aes(x = date, y = mortality)) +
    labs(
      title = paste0("Monthly Mortality [", country, "]"),
      subtitle = "Source: github.com/USMortality/charts",
      y = "Deaths/100k",
      x = "Month of Year"
    ) +
    geom_line(color = "#5383EC", linewidth = 1) +
    twitter_theme() +
    watermark(df$yearmonth, df$value_p) +
    scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y") +
    theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
  save_chart(chart2, paste("mortality", country, "monthly_line", sep = "/"))

  print("3) Quarterly")
  df <- data_quarterly %>%
    filter(iso3c == country) %>%
    mutate(date = yearquarter(date)) %>%
    as_tsibble(index = date)

  chart3 <-
    ggplot(df, aes(x = date, y = mortality)) +
    labs(
      title = paste0("Quarterly Mortality [", country, "]"),
      subtitle = "Source: github.com/USMortality/charts",
      y = "Deaths/100k",
      x = "Month of Year"
    ) +
    geom_line(color = "#5383EC", linewidth = 1) +
    twitter_theme() +
    watermark(df$yearmonth, df$value_p) +
    scale_x_yearquarter(date_breaks = "1 year", date_labels = "%Y") +
    theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
  save_chart(chart3, paste("mortality", country, "quarterly_line", sep = "/"))

  print("4) Yearly")
  df <- data_yearly %>%
    filter(iso3c == country) %>%
    mutate(date = ymd(date, truncated = 2L)) %>%
    as_tsibble(index = date)

  chart4 <-
    ggplot(df, aes(x = date, y = mortality)) +
    labs(
      title = paste0("Yearly Mortality [", country, "]"),
      subtitle = "Source: github.com/USMortality/charts",
      y = "Deaths/100k",
      x = "Month of Year"
    ) +
    geom_line(color = "#5383EC", linewidth = 1) +
    scale_y_continuous(labels = comma_format(decimal.mark = ",")) +
    twitter_theme() +
    watermark(df$yearmonth, df$value_p) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
  save_chart(chart4, paste("mortality", country, "yearly_line", sep = "/"))

  print("5) YTD")
  df <- data_ytd %>%
    filter(iso3c == country) %>%
    mutate(date = ymd(date, truncated = 2L)) %>%
    as_tsibble(index = date)

  chart5 <-
    ggplot(df, aes(x = date, y = mortality)) +
    labs(
      title = paste0("Mortality YTD [", country, "]"),
      subtitle = paste0(
        "Until: ", tail(df$max_date, n = 1),
        "; Source: github.com/USMortality/charts"
      ),
      y = "Deaths/100k",
      x = "Month of Year"
    ) +
    geom_line(color = "#5383EC", linewidth = 1) +
    scale_y_continuous(labels = comma_format(decimal.mark = ",")) +
    twitter_theme() +
    watermark(df$yearmonth, df$value_p) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
  save_chart(chart5, paste("mortality", country, "ytd_line", sep = "/"))

  print("6) Flu Season")
  df <- data_fluseason %>%
    filter(iso3c == country)

  df <- df %>%
    mutate(index = seq(1:length(df$date))) %>%
    mutate(date = paste0(mid(date, 3, 2), "/", right(date, 2)))
  chart6 <-
    ggplot(df, aes(x = index, y = mortality)) +
    labs(
      title = paste0("Mortality by Flu Season [", country, "]"),
      subtitle = "Oct 1 - Sep 30; Source: github.com/USMortality/charts",
      y = "Deaths/100k",
      x = "Flu Season"
    ) +
    geom_line(color = "#5383EC", linewidth = 1) +
    scale_x_continuous(breaks = 1:length(df$date), labels = df$date) +
    scale_y_continuous(labels = comma_format(decimal.mark = ",")) +
    twitter_theme() +
    watermark(df$yearmonth, df$value_p) +
    theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
  save_chart(chart6, paste("mortality", country, "fluseason_line", sep = "/"))

  print("7) STL Decomposition")
  df <- data_weekly %>%
    filter(iso3c == country) %>%
    mutate(date = yearweek(date)) %>%
    as_tsibble(index = date) %>%
    model(STL(mortality)) %>%
    components()
  chart7 <-
    autoplot(df, .vars = mortality) +
    labs(
      title = paste0("Weekly Mortality - STL Decomposition [", country, "]"),
      subtitle = "Source: github.com/USMortality/charts",
      y = "Deaths/100k",
      x = "Week of Year"
    ) +
    twitter_theme() +
    watermark(df$yearmonth, df$value_p)
  save_chart(chart7, paste("mortality", country, "stl_line", sep = "/"))

  print("8) Yearly")
  df <- data_yearly %>%
    filter(iso3c == country) %>%
    as_tsibble(index = date)

  ### Calculate Baseline & Excess
  start_year <- ifelse(min(df$date) > 2013, min(df$date), 2010)
  data_bl <- df %>% filter(date %in% seq(start_year, 2019))
  model <- lm(y ~ x, data = data.frame(x = data_bl$date, y = data_bl$mortality))
  forecast <- predict(
    model,
    newdata = data.frame(x = seq(start_year, max(df$date)))
  )
  fc_df <- data.frame(
    date = seq(max(2010, min(df$date)), max(df$date)), baseline = forecast
  )
  data <- df %>%
    inner_join(fc_df, by = "date") %>%
    mutate(excess = mortality - baseline) %>%
    mutate(excess_percent = excess / baseline)

  chart8 <-
    ggplot(data, aes(x = date, y = mortality)) +
    labs(
      title = paste0("Yearly Mortality [", country, "]"),
      subtitle = paste0(
        "Baseline: ", start_year, "-2019; ",
        "Source: github.com/USMortality/charts"
      ),
      y = "Deaths/100k",
      x = "Year"
    ) +
    twitter_theme() +
    watermark(data$date, data$mortality) +
    geom_col(fill = "#5383EC") +
    geom_abline(
      intercept = model$coefficients[1],
      slope = model$coefficients[2],
      linetype = "dashed",
      colour = "#00000077",
      size = 1
    ) +
    geom_text(
      aes(label = round(mortality)),
      vjust = 2.5, colour = "#ffffff"
    ) +
    geom_text(
      aes(label = percent(excess_percent, accuracy = 0.1)),
      vjust = -.2
    ) +
    scale_y_continuous(labels = comma_format(decimal.mark = ","))

  save_chart(chart8, paste("mortality", country, "yearly_bar", sep = "/"))

  print("9) YTD")
  df <- data_ytd %>%
    filter(iso3c == country) %>%
    as_tsibble(index = date)

  ### Calculate Baseline & Excess
  start_year <- ifelse(min(df$date) > 2013, min(df$date), 2010)
  data_bl <- df %>% filter(date %in% seq(start_year, 2019))
  model <- lm(y ~ x, data = data.frame(x = data_bl$date, y = data_bl$mortality))
  forecast <- predict(
    model,
    newdata = data.frame(x = seq(start_year, max(df$date)))
  )
  fc_df <- data.frame(
    date = seq(max(2010, min(df$date)), max(df$date)), baseline = forecast
  )
  data <- df %>%
    inner_join(fc_df, by = "date") %>%
    mutate(excess = mortality - baseline) %>%
    mutate(excess_percent = excess / baseline)

  chart9 <-
    ggplot(data, aes(x = date, y = mortality)) +
    labs(
      title = paste0("YTD Mortality [", country, "]"),
      subtitle = paste0(
        "Until: ", tail(df$max_date, n = 1),
        "; Baseline: ", start_year, "-2019; ",
        "Source: github.com/USMortality/charts"
      ),
      y = "Deaths/100k",
      x = "Year"
    ) +
    twitter_theme() +
    watermark(data$date, data$mortality) +
    geom_col(fill = "#5383EC") +
    geom_abline(
      intercept = model$coefficients[1],
      slope = model$coefficients[2],
      linetype = "dashed",
      colour = "#00000077",
      size = 1
    ) +
    geom_text(
      aes(label = round(mortality)),
      vjust = 2.5, colour = "#ffffff"
    ) +
    geom_text(
      aes(label = percent(excess_percent, accuracy = 0.1)),
      vjust = -.2
    ) +
    scale_y_continuous(labels = comma_format(decimal.mark = ","))

  save_chart(chart9, paste("mortality", country, "ytd_bar", sep = "/"))

  save_collage(
    chart1, chart2, chart3, chart4, chart5, chart6, chart7, chart8, chart9,
    path = paste("mortality", country, sep = "/"),
    ncol = 3, nrow = 3
  )
}
