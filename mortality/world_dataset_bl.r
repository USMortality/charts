source("lib/common.r")

# Define default functions
select <- dplyr::select
filter <- dplyr::filter
mutate <- dplyr::mutate
group_by <- dplyr::group_by
ungroup <- dplyr::ungroup
summarise <- dplyr::summarise
inner_join <- dplyr::inner_join
relocate <- dplyr::relocate
year <- lubridate::year
month <- lubridate::month
week <- lubridate::week
days <- lubridate::days
days_in_month <- lubridate::days_in_month
as_tibble <- tibble::as_tibble
tibble <- tibble::tibble
as_tsibble <- tsibble::as_tsibble
str_replace <- stringr::str_replace
uncount <- tidyr::uncount
sym <- rlang::sym
model <- fabletools::model
date <- lubridate::date
forecast <- fabletools::forecast
select <- dplyr::select
all_of <- dplyr::all_of
nest <- tidyr::nest
unnest <- tidyr::unnest
.data <- dplyr::.data
yearmonth <- tsibble::yearmonth
yearweek <- tsibble::yearweek
ggplot <- ggplot2::ggplot
make_yearmonth <- tsibble::make_yearmonth
arrange <- dplyr::arrange
distinct <- dplyr::distinct
complete <- tidyr::complete
case_when <- dplyr::case_when

data_yearly <- read_remote("mortality/world_yearly.csv")
data_baseline <- read_remote("mortality/world_baseline.csv")

types <- c("cmr", "asmr")
asmr_data <- data_yearly |> filter(!is.na(asmr))

calculate_baseline <- function(table, mortality_col, forecast_interval = 1) {
  r <- table |>
    filter(!!mortality_col > 0) |>
    filter(!is.na(!!mortality_col)) |>
    model(fable::TSLM(!!mortality_col ~ trend())) |>
    forecast(h = forecast_interval)
  hl <- fabletools::hilo(r, 95) |> fabletools::unpack_hilo(cols = `95%`) # nolint
  data.frame(
    date = r$date,
    mean = round(r$.mean, digits = 1),
    lower = round(hl$`95%_lower`, digits = 1),
    upper = round(hl$`95%_upper`, digits = 1)
  ) |> as_tibble()
}

get_data <- function(df, bl_size, mortality_col) {
  training_data <- df |>
    filter(date < 2020) |>
    as_tsibble(index = date) |>
    tsibble::slide_tsibble(.size = bl_size) |>
    nest(data = !.data$.id)

  last_n_years <- training_data |>
    mutate(data = lapply(data, calculate_baseline, mortality_col)) |>
    unnest(cols = c(data)) |>
    select(!.data$.id)
  last_n_years$fc_type <- rep("1 year", length(last_n_years$date))

  # First n years
  first_n_years <- training_data$data[[1]] |>
    filter(!is.na(!!mortality_col)) |>
    model(fable::TSLM(!!mortality_col ~ trend())) |>
    fabletools::augment() |>
    select(2, 4) |>
    setNames(c("date", "mean")) |>
    as_tibble()

  fc <- calculate_baseline(
    training_data$data[[length(training_data$data)]],
    mortality_col,
    3
  )
  fc$fc_type <- rep("3 year", 3)

  iso <- df$iso3c[1]
  jur <- df$jurisdiction[1]
  dplyr::bind_rows(first_n_years, last_n_years, fc) |>
    setNames(c(
      "date", "baseline", "baseline_lower", "baseline_upper", "fc_type"
    )) |>
    mutate(
      iso3c = iso, jurisdiction = jur,
      baseline = round(.data$baseline, digits = 1)
    ) |>
    relocate("iso3c", "jurisdiction")
}

calculate_excess <- function(data, mortality_col) {
  data |>
    mutate(
      excess = round(!!mortality_col - baseline, digits = 1), # nolint
      excess_p = round((!!mortality_col - baseline) / baseline, digits = 3),
      sign_excess = round(!!mortality_col - baseline_upper, digits = 1), # nolint
      sign_excess_p = round(
        (!!mortality_col - baseline_upper) / baseline,
        digits = 3
      ),
    ) |>
    mutate(
      sign_excess = ifelse(sign_excess >= 0, sign_excess, NA), # nolint
      sign_excess_p = ifelse(sign_excess_p >= 0, sign_excess_p, NA), # nolint
    )
}

for (mortality_type in types) {
  countries <- get_countries_for_type(mortality_type, data_yearly, asmr_data)
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
        get_data(df, bl_size, mortality_col),
        by = c("iso3c", "name", "date")
      )

    chart <-
      ggplot(data, aes(x = date, y = !!mortality_col, color = fc_type)) +
      labs(
        title = paste0("Weekly ", mortality_title, " [", country, "]"),
        subtitle = paste0(
          "95% PI; <2020: 1y forecast;",
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
    result <- rbind(result, calculate_excess(data, mortality_col))
  }

  save_csv(
    result, paste0("mortality/world_yearly_", mortality_type, "_bl"),
    upload = FALSE
  )
}
