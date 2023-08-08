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

get_optimal_size <- function(df, col_name) {
  min <- Inf
  optimal_size <- nrow(df)
  col_name <- sym(col_name)
  if (nrow(na.omit(df)) < 3) {
    return(optimal_size)
  }
  for (size in 5:ceiling(nrow(df) / 2)) {
    acc <- df |>
      tsibble::slide_tsibble(.size = size) |>
      model(fable::TSLM(!!col_name ~ trend())) |>
      forecast(h = 3) |>
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
    data.frame(matrix(ncol = 4, nrow = 0)),
    c("iso3c", "jurisdiction", "type", "window")
  )
  asmr_types <- c("asmr_who", "asmr_esp", "asmr_usa", "asmr_country")
  types <- c("deaths", "cmr", asmr_types)
  for (mortality_type in types) {
    for (country in unique(data$jurisdiction)) {
      print(paste(country, mortality_type))
      df <- data |>
        filter(
          .data$jurisdiction == country,
          date < 2020
        ) |>
        as_tsibble(index = date)

      optimal_size <- ifelse(ceiling(nrow(df) / 2) > 5,
        get_optimal_size(df, mortality_type),
        nrow(df)
      )

      iso <- head(data |> filter(.data$jurisdiction == country), 1)$iso3c
      result[nrow(result) + 1, ] <- c(
        iso, country, mortality_type, optimal_size
      )
      print(paste0("Optimal window size: ", optimal_size))
    }
  }
  result
}

data <- read_remote("mortality/world_yearly.csv")
yearly <- data |>
  get_baseline_size() |>
  mutate(chart_type = "yearly", .after = "jurisdiction")

data <- read_remote("mortality/world_fluseason.csv")
fluseason <- data |>
  mutate(date = as.integer(left(date, 4))) |>
  get_baseline_size() |>
  mutate(chart_type = "fluseason", .after = "jurisdiction")

data <- read_remote("mortality/world_midyear.csv")
midyear <- data |>
  mutate(date = as.integer(left(date, 4))) |>
  get_baseline_size() |>
  mutate(chart_type = "midyear", .after = "jurisdiction")

save_csv(rbind(yearly, fluseason, midyear), "mortality/world_baseline")
