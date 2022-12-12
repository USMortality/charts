source("lib/common.r")

data_weekly <- read_remote("mortality/world_weekly.csv")
data_monthly <- read_remote("mortality/world_monthly.csv")
data_quarterly <- read_remote("mortality/world_quarterly.csv")
data_yearly <- read_remote("mortality/world_yearly.csv")
data_ytd <- read_remote("mortality/world_ytd.csv")
data_fluseason <- read_remote("mortality/world_fluseason.csv")

types <- c("cmr", "asmr")
asmr_data <- data_weekly %>% filter(!is.na(asmr))

for (type in types) {
  countries <- if (type == "cmr") {
    unique(data_weekly$name)
  } else {
    unique(asmr_data$name)
  }
  mortality_col <- sym(type)
  mortality_title <- ifelse(
    type == "cmr",
    "Crude Mortality Rate",
    "Age Std. Mortality Rate"
  )

  print(paste("-----", type, "-----"))
  for (country in countries) {
    print(country)
    print("1) Weekly")
    df <- data_weekly %>%
      filter(name == country) %>%
      mutate(date = yearweek(date)) %>%
      as_tsibble(index = date)

    chart1 <-
      ggplot(df, aes(x = date, y = !!mortality_col)) +
      labs(
        title = paste0("Weekly ", mortality_title, " [", country, "]"),
        subtitle = "Source: www.mortality.watch",
        y = "Deaths/100k",
        x = "Week of Year"
      ) +
      geom_line(color = "#5383EC", linewidth = 1) +
      twitter_theme() +
      scale_x_yearweek(
        # date_breaks = "1 year",
        date_labels = "%Y"
      ) +
      theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
    save_chart(chart1, paste("mortality", type, country, "weekly_line", sep = "/"))

    print("2) Monthly")
    df <- data_monthly %>%
      filter(name == country) %>%
      mutate(date = yearmonth(date)) %>%
      as_tsibble(index = date)

    chart2 <-
      ggplot(df, aes(x = date, y = !!mortality_col)) +
      labs(
        title = paste0("Monthly ", mortality_title, " [", country, "]"),
        subtitle = "Source: www.mortality.watch",
        y = "Deaths/100k",
        x = "Month of Year"
      ) +
      geom_line(color = "#5383EC", linewidth = 1) +
      twitter_theme() +
      scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y") +
      theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
    save_chart(chart2, paste("mortality", type, country, "monthly_line", sep = "/"))

    print("3) Quarterly")
    df <- data_quarterly %>%
      filter(name == country) %>%
      mutate(date = yearquarter(date)) %>%
      as_tsibble(index = date)

    chart3 <-
      ggplot(df, aes(x = date, y = !!mortality_col)) +
      labs(
        title = paste0("Quarterly ", mortality_title, " [", country, "]"),
        subtitle = "Source: www.mortality.watch",
        y = "Deaths/100k",
        x = "Month of Year"
      ) +
      geom_line(color = "#5383EC", linewidth = 1) +
      twitter_theme() +
      scale_x_yearquarter(date_breaks = "1 year", date_labels = "%Y") +
      theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
    save_chart(chart3, paste("mortality", type, country, "quarterly_line", sep = "/"))

    print("4) Yearly")
    df <- data_yearly %>%
      filter(name == country) %>%
      mutate(date = ymd(date, truncated = 2L)) %>%
      as_tsibble(index = date)

    chart4 <-
      ggplot(df, aes(x = date, y = !!mortality_col)) +
      labs(
        title = paste0("Yearly ", mortality_title, " [", country, "]"),
        subtitle = "Source: www.mortality.watch",
        y = "Deaths/100k",
        x = "Month of Year"
      ) +
      geom_line(color = "#5383EC", linewidth = 1) +
      scale_y_continuous(labels = comma_format(decimal.mark = ",")) +
      twitter_theme() +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
    save_chart(chart4, paste(
      "mortality", type, country, "yearly_line", sep = "/"
    ))

    print("5) YTD")
    df <- data_ytd %>%
      filter(name == country) %>%
      mutate(date = ymd(date, truncated = 2L)) %>%
      as_tsibble(index = date)

    chart5 <-
      ggplot(df, aes(x = date, y = !!mortality_col)) +
      labs(
        title = paste0("YTD ", mortality_title, " [", country, "]"),
        subtitle = paste0(
          "Yearly until: ", tail(df$max_date, n = 1),
          "; Source: www.mortality.watch"
        ),
        y = "Deaths/100k",
        x = "Month of Year"
      ) +
      geom_line(color = "#5383EC", linewidth = 1) +
      scale_y_continuous(labels = comma_format(decimal.mark = ",")) +
      twitter_theme() +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
    save_chart(chart5, paste("mortality", type, country, "ytd_line", sep = "/"))

    print("6) Flu Season")
    df <- data_fluseason %>%
      filter(name == country)
    df <- df %>%
      mutate(index = seq(1:length(df$date))) %>%
      mutate(date = paste0(mid(date, 3, 2), "/", right(date, 2)))
    chart6 <-
      ggplot(df, aes(x = index, y = !!mortality_col)) +
      labs(
        title = paste0("Flu Season ", mortality_title, " [", country, "]"),
        subtitle = "Oct 1 - Sep 30; Source: www.mortality.watch",
        y = "Deaths/100k",
        x = "Flu Season"
      ) +
      geom_line(color = "#5383EC", linewidth = 1) +
      scale_x_continuous(breaks = 1:length(df$date), labels = df$date) +
      scale_y_continuous(labels = comma_format(decimal.mark = ",")) +
      twitter_theme() +
      theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
    save_chart(chart6, paste("mortality", type, country, "fluseason_line", sep = "/"))

    print("7) STL Decomposition")
    df <- data_weekly %>%
      filter(name == country) %>%
      filter(!is.na(!!mortality_col)) %>%
      mutate(date = yearweek(date)) %>%
      as_tsibble(index = date) %>%
      group_by_key() %>%
      fill_gaps() %>%
      fill(!!mortality_col, .direction = "down") %>%
      model(STL(!!mortality_col)) %>%
      components()
    chart7 <-
      autoplot(df, .vars = !!mortality_col) +
      labs(
        title = paste0("Weekly ", mortality_title, " - STL Decomp. [", country, "]"),
        subtitle = "Source: www.mortality.watch",
        y = "Deaths/100k",
        x = "Week of Year"
      ) +
      twitter_theme()
    save_chart(chart7, paste("mortality", type, country, "stl_line", sep = "/"))

    print("8) Yearly")
    df <- data_yearly %>%
      filter(name == country) %>%
      as_tsibble(index = date)

    chart8 <-
      ggplot(df, aes(x = date, y = !!mortality_col)) +
      labs(
        title = paste0("Yearly ", mortality_title, " [", country, "]"),
        subtitle = "Source: www.mortality.watch",
        y = "Deaths/100k",
        x = "Year"
      ) +
      twitter_theme() +
      geom_col(fill = "#5383EC") +
      geom_text(
        aes(label = round(!!mortality_col)),
        vjust = 2.5, colour = "#ffffff"
      ) +
      scale_x_continuous(breaks = df$date) +
      theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
      scale_y_continuous(labels = comma_format(decimal.mark = ","))

    save_chart(chart8, paste("mortality", type, country, "yearly_bar", sep = "/"))

    print("9) YTD")
    df <- data_ytd %>%
      filter(name == country) %>%
      as_tsibble(index = date)

    chart9 <-
      ggplot(df, aes(x = date, y = !!mortality_col)) +
      labs(
        title = paste0("YTD  ", mortality_title, " [", country, "]"),
        subtitle = paste0(
          "Yearly until: ", tail(df$max_date, n = 1),
          "; Source: www.mortality.watch"
        ),
        y = "Deaths/100k",
        x = "Year"
      ) +
      twitter_theme() +
      geom_col(fill = "#5383EC") +
      geom_text(
        aes(label = round(!!mortality_col)),
        vjust = 2.5, colour = "#ffffff"
      ) +
      scale_x_continuous(breaks = df$date) +
      theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
      scale_y_continuous(labels = comma_format(decimal.mark = ","))

    save_chart(chart9, paste("mortality", type, country, "ytd_bar", sep = "/"))

    print("10) Weekly (52W SMA)")
    df <- data_weekly %>%
      filter(name == country) %>%
      filter(!is.na(!!mortality_col)) %>%
      mutate(date = yearweek(date)) %>%
      as_tsibble(index = date) %>%
      mutate(sma = SMA(!!mortality_col, n = 52))

    chart10 <-
      ggplot(df, aes(x = date, y = sma)) +
      labs(
        title = paste0("Weekly ", mortality_title, " (52W SMA) [", country, "]"),
        subtitle = "Source: www.mortality.watch",
        y = "Deaths/100k",
        x = "Week of Year"
      ) +
      geom_line(color = "#5383EC", linewidth = 1) +
      twitter_theme() +
      scale_x_yearweek(
        # date_breaks = "1 year",
        date_labels = "%Y"
      ) +
      theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
    save_chart(
      chart10,
      paste("mortality", type, country, "weekly_52w_sma_line", sep = "/")
    )

    save_collage(
      chart10, chart1, chart2, chart3, chart4, chart5, chart6, chart7, chart8,
      chart9,
      path = paste("mortality", type, country, "collage", sep = "/"),
      ncol = 2, nrow = 5, scale = 5
    )
  }
}
