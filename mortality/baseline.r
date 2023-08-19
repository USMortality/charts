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

forecast_len <- 3
get_optimal_size <- function(df, type) {
  min <- Inf
  optimal_size <- nrow(df)
  type <- sym(type)
  if (nrow(na.omit(df)) < 3) {
    return(optimal_size)
  }

  optimal_size <- nrow(df)
  for (size in 5:min(optimal_size - forecast_len, 15)) {
    acc <- df |>
      head(-forecast_len) |>
      tsibble::slide_tsibble(.size = size) |>
      model(fable::TSLM(!!type ~ trend())) |>
      forecast(h = forecast_len) |>
      fabletools::accuracy(df)
    if (!is.nan(acc$RMSE) && acc$RMSE < min) {
      min <- acc$RMSE
      print(paste("New min:", min, "Size:", size))
      optimal_size <- size
    }
  }

  return(optimal_size)
}

get_baseline_size <- function(data) {
  result <- setNames(
    data.frame(matrix(ncol = 3, nrow = 0)),
    c("iso3c", "type", "window")
  )
  asmr_types <- c("asmr_who", "asmr_esp", "asmr_usa", "asmr_country")
  types <- c("deaths", "cmr", asmr_types)
  for (type in types) {
    for (iso in unique(data$iso3c)) {
      print(paste(iso, type))
      df <- data |>
        filter(.data$iso3c == iso, date < 2020) |>
        as_tsibble(index = date)

      optimal_size <- ifelse(nrow(df) > 5,
        get_optimal_size(df, type),
        nrow(df)
      )

      iso <- head(data |> filter(.data$iso3c == iso), 1)$iso3c
      result[nrow(result) + 1, ] <- c(
        iso, type, optimal_size
      )
      print(paste0("Optimal window size: ", optimal_size))
    }
  }
  result
}

data <- read_remote("mortality/world_yearly.csv")
yearly <- data |>
  get_baseline_size() |>
  mutate(chart_type = "yearly", .after = "iso3c")

data <- read_remote("mortality/world_fluseason.csv")
fluseason <- data |>
  mutate(date = as.integer(left(date, 4))) |>
  get_baseline_size() |>
  mutate(chart_type = "fluseason", .after = "iso3c")

data <- read_remote("mortality/world_midyear.csv")
midyear <- data |>
  mutate(date = as.integer(left(date, 4))) |>
  get_baseline_size() |>
  mutate(chart_type = "midyear", .after = "iso3c")

save_csv(rbind(yearly, fluseason, midyear), "mortality/world_baseline")
