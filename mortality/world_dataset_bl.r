source("lib/common.r")

data_yearly <- read_remote("mortality/world_yearly.csv")
data_baseline <- read_remote("mortality/world_baseline.csv")

types <- c("cmr", "asmr")
asmr_data <- data_yearly |> filter(!is.na(asmr))

calculate_baseline <- function(table, forecast_interval = 1) {
  r <- table |>
    filter(!!mortality_col > 0) |>
    filter(!is.na(!!mortality_col)) |>
    model(TSLM(!!mortality_col ~ trend())) |>
    forecast(h = forecast_interval)
  hl <- hilo(r, 99.99) |> unpack_hilo(cols = `99.99%`)
  data.frame(
    date = r$date,
    mean = round(r$.mean, digits = 1),
    lower = round(hl$`99.99%_lower`, digits = 1),
    upper = round(hl$`99.99%_upper`, digits = 1)
  ) |> as_tibble()
}

getData <- function(df, bl_size, mortality_col) {
  training_data <- df |>
    filter(date < 2020) |>
    as_tsibble(index = date) |>
    slide_tsibble(.size = bl_size) |>
    nest(data = !.id)

  last_n_years <- training_data |>
    mutate(data = lapply(data, calculate_baseline)) |>
    unnest(cols = c(data)) |>
    select(!.id)
  last_n_years$fc_type <- rep("1 year", length(last_n_years$date))

  # First n years
  first_n_years <- training_data$data[[1]] |>
    filter(!is.na(!!mortality_col)) |>
    model(TSLM(!!mortality_col ~ trend())) |>
    augment() |>
    select(2, 4) |>
    setNames(c("date", "mean")) |>
    as_tibble()

  fc <- calculate_baseline(training_data$data[[length(training_data$data)]], 3)
  fc$fc_type <- rep("3 year", 3)

  bind_rows(first_n_years, last_n_years, fc) |>
    setNames(c(
      "date", "baseline", "baseline_lower", "baseline_upper", "fc_type"
    )) |>
    mutate(
      iso3c = df$iso3c[1],
      jurisdiction = df$jurisdiction[1],
      baseline = round(baseline, digits = 1)
    ) |>
    relocate(iso3c, jurisdiction)
}

calculateExcess <- function(data) {
  data |>
    mutate(
      excess = round(!!mortality_col - baseline, digits = 1),
      excess_p = round((!!mortality_col - baseline) / baseline, digits = 3),
      sign_excess = round(!!mortality_col - baseline_upper, digits = 1),
      sign_excess_p = round((!!mortality_col - baseline_upper) / baseline, digits = 3),
    ) |>
    mutate(
      sign_excess = ifelse(sign_excess >= 0, sign_excess, NA),
      sign_excess_p = ifelse(sign_excess_p >= 0, sign_excess_p, NA),
    )
}

country <- "Germany"
df <- data_yearly |> filter(jurisdiction == country)
mortality_type <- "cmr"
mortality_col <- sym(mortality_type)
bl_size <- 4
data <- df |>
  select(iso3c, jurisdiction, date, !!mortality_col) |>
  right_join(
    getData(df, bl_size, mortality_col),
    by = c("iso3c", "jurisdiction", "date")
  )

for (mortality_type in types) {
  countries <- getCountriesForType(mortality_type, data_yearly, asmr_data)
  mortality_col <- sym(mortality_type)
  mortality_title <- ifelse(
    mortality_type == "cmr",
    "Crude Mortality Rate",
    "Age Std. Mortality Rate"
  )
  result <- setNames(
    data.frame(matrix(ncol = 12, nrow = 0)),
    c(
      "iso3c", "name", "date", mortality_type, "baseline", "baseline_lower",
      "baseline_upper", "fc_type", "excess", "excess_p", "sign_excess",
      "sign_excess_p"
    )
  )

  print(paste("-----", mortality_type, "-----"))
  for (country in countries) {
    print(country)

    bl_size <- (data_baseline |>
      filter(name == country) |>
      filter(type == mortality_type))$window
    df <- data_yearly |> filter(name == country)
    if (nrow(df |> filter(date < 2020)) < 3) next
    data <- df |>
      select(iso3c, name, date, !!mortality_col) |>
      right_join(
        getData(df, bl_size, mortality_col),
        by = c("iso3c", "name", "date")
      )

    chart <-
      ggplot(data, aes(x = date, y = !!mortality_col, color = fc_type)) +
      labs(
        title = paste0("Weekly ", mortality_title, " [", country, "]"),
        subtitle = paste0(
          "99.99% PI; <2020: 1y forecast;",
          " >=2020: 3y forecast; Source: www.mortality.watch"
        ),
        y = "Deaths/100k",
        x = "Year",
        color = "99% Forecast Interval"
      ) +
      geom_line(color = "#5383EC", linewidth = 1) +
      geom_line(
        aes(y = baseline),
        color = "#000000",
        linetype = "dashed",
        linewidth = 1
      ) +
      geom_ribbon(
        data = data |> filter(fc_type == "1 year"),
        aes(
          ymin = baseline_lower,
          ymax = baseline_upper,
          x = date
        ),
        alpha = 0.2,
        fill = "#AFED3B",
        color = "#5ED62B"
      ) +
      geom_ribbon(
        data = data |> filter(fc_type != "1 year"),
        aes(
          ymin = baseline_lower,
          ymax = baseline_upper,
          x = date
        ),
        alpha = 0.2,
        fill = "#AFED3B",
        color = "#C33BED"
      ) +
      twitter_theme() +
      theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
      theme(legend.position = "bottom") +
      scale_x_continuous(breaks = unique(data$date), labels = unique(data$date))
    save_chart(chart, paste0(
      "mortality_bl/", mortality_type, "/", country
    ), upload = FALSE)
    result <- rbind(result, calculateExcess(data))
  }

  save_csv(
    result, paste0("mortality/world_yearly_", mortality_type, "_bl"),
    upload = FALSE
  )
}
