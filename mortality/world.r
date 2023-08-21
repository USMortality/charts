source("lib/common.r")

data_weekly <- read_remote("mortality/world_weekly.csv")
data_monthly <- read_remote("mortality/world_monthly.csv")
data_quarterly <- read_remote("mortality/world_quarterly.csv")
data_yearly <- read_remote("mortality/world_yearly.csv")
data_fluseason <- read_remote("mortality/world_fluseason.csv")

types <- c("cmr", "asmr_who")
asmr_data <- data_weekly |> filter(!is.na(asmr_who))

for (type in types) {
  countries <- if (type == "cmr") {
    unique(data_weekly$iso3c)
  } else {
    unique(asmr_data$iso3c)
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
    df <- data_weekly |>
      filter(iso3c == country, !is.na(!!mortality_col)) |>
      mutate(yearweek = yearweek(date)) |>
      mutate(date = date(yearweek)) |>
      as_tsibble(index = date)
    chart1 <-
      ggplot(df, aes(x = date, y = !!mortality_col)) +
      labs(
        title = paste0("Weekly ", mortality_title, " [", country, "]"),
        subtitle = paste0(
          "Data until: ", df$yearweek, "; Source: www.mortality.watch"
        ),
        y = "Deaths/100k",
        x = "Week of Year"
      ) +
      geom_line(color = "#5383EC", linewidth = 1) +
      twitter_theme() +
      scale_x_date(date_labels = "%Y", breaks = "1 year") +
      theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
    save_chart(
      chart1,
      paste("mortality", type, country, "weekly_line", sep = "/"),
      upload = TRUE
    )

    print("2) Monthly")
    df <- data_monthly |>
      filter(iso3c == country, !is.na(!!mortality_col)) |>
      mutate(date = yearmonth(date)) |>
      as_tsibble(index = date)
    chart2 <-
      ggplot(df, aes(x = date, y = !!mortality_col)) +
      labs(
        title = paste0("Monthly ", mortality_title, " [", country, "]"),
        subtitle = paste0(
          "Data until: ", df$date, "; Source: www.mortality.watch"
        ),
        y = "Deaths/100k",
        x = "Month of Year"
      ) +
      geom_line(color = "#5383EC", linewidth = 1) +
      twitter_theme() +
      scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y") +
      theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
    save_chart(
      chart2,
      paste("mortality", type, country, "monthly_line", sep = "/"),
      upload = TRUE
    )

    print("3) Quarterly")
    df <- data_quarterly |>
      filter(iso3c == country, !is.na(!!mortality_col)) |>
      mutate(date = yearquarter(date)) |>
      as_tsibble(index = date)
    chart3 <-
      ggplot(df, aes(x = date, y = !!mortality_col)) +
      labs(
        title = paste0("Quarterly ", mortality_title, " [", country, "]"),
        subtitle = paste0(
          "Data until: ",
          tail(df |> filter(!is.na(!!mortality_col)), n = 1)$date,
          "; Source: www.mortality.watch"
        ),
        y = "Deaths/100k",
        x = "Month of Year"
      ) +
      geom_line(color = "#5383EC", linewidth = 1) +
      twitter_theme() +
      scale_x_yearquarter(date_breaks = "1 year", date_labels = "%Y") +
      theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
    save_chart(
      chart3,
      paste("mortality", type, country, "quarterly_line", sep = "/"),
      upload = TRUE
    )

    print("4) Yearly")
    df <- data_yearly |>
      filter(iso3c == country, !is.na(!!mortality_col)) |>
      mutate(date = ymd(date, truncated = 2L)) |>
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
      twitter_theme() +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
    save_chart(
      chart4,
      paste("mortality", type, country, "yearly_line", sep = "/"),
      upload = TRUE
    )

    print("5) Flu Season")
    df <- data_fluseason |>
      filter(iso3c == country, !is.na(!!mortality_col))
    df <- df |>
      mutate(index = seq_along(df$date)) |>
      mutate(date = paste0(mid(date, 3, 2), "/", right(date, 2)))
    chart5 <-
      ggplot(df, aes(x = index, y = !!mortality_col)) +
      labs(
        title = paste0("Flu Season ", mortality_title, " [", country, "]"),
        subtitle = "Oct 1 - Sep 30; Source: www.mortality.watch",
        y = "Deaths/100k",
        x = "Flu Season"
      ) +
      geom_line(color = "#5383EC", linewidth = 1) +
      scale_x_continuous(breaks = seq_along(df$date), labels = df$date) +
      twitter_theme() +
      theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
    save_chart(
      chart5,
      paste("mortality", type, country, "fluseason_line", sep = "/"),
      upload = TRUE
    )

    print("6) STL Decomposition")
    df <- data_weekly |>
      filter(iso3c == country, !is.na(!!mortality_col)) |>
      filter(!is.na(!!mortality_col)) |>
      mutate(yearweek = yearweek(date)) |>
      mutate(date = date(yearweek)) |>
      as_tsibble(index = date) |>
      group_by_key() |>
      fill_gaps() |>
      fill(!!mortality_col, .direction = "down") |>
      model(STL(!!mortality_col)) |>
      components()
    chart6 <-
      autoplot(df, .vars = !!mortality_col) +
      labs(
        title = paste0(
          "Weekly ", mortality_title, " - STL Decomp. [", country, "]"
        ),
        subtitle = paste0(
          "Data until: ",
          tail(df |> filter(!is.na(!!mortality_col)), n = 1)$date,
          "; Source: www.mortality.watch"
        ),
        y = "Deaths/100k",
        x = "Week of Year"
      ) +
      scale_x_date(date_labels = "%Y", breaks = "1 year") +
      twitter_theme() +
      theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
    save_chart(
      chart6,
      paste("mortality", type, country, "stl_line", sep = "/"),
      upload = TRUE
    )

    print("7) Yearly (Bar)")
    df <- data_yearly |>
      filter(iso3c == country, !is.na(!!mortality_col)) |>
      as_tsibble(index = date)
    chart7 <-
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
        size = 3, vjust = 2.5, colour = "#ffffff"
      ) +
      scale_x_continuous(breaks = df$date) +
      theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
    save_chart(
      chart7,
      paste("mortality", type, country, "yearly_bar", sep = "/"),
      upload = TRUE
    )

    print("8) Flu Season (Bar)")
    df <- data_fluseason |>
      filter(iso3c == country, !is.na(!!mortality_col))
    df <- df |>
      mutate(index = seq(seq_along(df$date))) |>
      mutate(date = paste0(mid(date, 3, 2), "/", right(date, 2)))
    chart8 <-
      ggplot(df, aes(x = index, y = !!mortality_col)) +
      labs(
        title = paste0("Flu Season ", mortality_title, " [", country, "]"),
        subtitle = "Oct 1 - Sep 30; Source: www.mortality.watch",
        y = "Deaths/100k",
        x = "Flu Season"
      ) +
      twitter_theme() +
      geom_col(fill = "#5383EC") +
      geom_text(
        aes(label = round(!!mortality_col)),
        size = 3, vjust = 2.5, colour = "#ffffff"
      ) +
      scale_x_continuous(breaks = seq_along(df$date), labels = df$date) +
      theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
    save_chart(
      chart8,
      paste("mortality", type, country, "fluseason_bar", sep = "/"),
      upload = TRUE
    )

    print("9) Weekly (52W SMA)")
    df <- data_weekly |>
      filter(iso3c == country, !is.na(!!mortality_col)) |>
      filter(!is.na(!!mortality_col)) |>
      mutate(yearweek = yearweek(date)) |>
      mutate(date = date(yearweek)) |>
      as_tsibble(index = date)
    df <- df |>
      mutate(sma = SMA(!!mortality_col, n = min(nrow(df), 52))) |>
      filter(!is.na(sma))
    chart9 <-
      ggplot(df, aes(x = date, y = sma)) +
      labs(
        title = paste0(
          "Weekly ", mortality_title, " (52W SMA) [", country, "]"
        ),
        subtitle = paste0(
          "Data until: ", df$yearweek, "; Source: www.mortality.watch"
        ),
        y = "Deaths/100k",
        x = "Week of Year"
      ) +
      geom_line(color = "#5383EC", linewidth = 1) +
      twitter_theme() +
      scale_x_date(date_labels = "%Y", breaks = "1 year") +
      theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
    save_chart(
      chart9,
      paste("mortality", type, country, "weekly_52w_sma_line", sep = "/"),
      upload = TRUE
    )

    save_collage(
      chart9, chart1, chart2, chart3, chart4, chart5, chart6, chart7, chart8,
      path = paste("mortality", type, country, "collage", sep = "/"),
      ncol = 3, nrow = 4, scale = 5
    )
  }
}
