source("lib/common.r")
source("lib/parallel.r")

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

forecast_len <- 4
get_optimal_size <- function(df, type) {
  min <- Inf
  optimal_size <- min(10, nrow(df))
  type <- sym(type)
  if (nrow(na.omit(df[type])) < 5 + forecast_len) {
    return(NA)
  }

  for (size in 5:min(optimal_size - forecast_len, 10)) {
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

filter_complete_latest <- function(df) {
  gaps <- df |> tsibble::scan_gaps()
  if (nrow(gaps) == 0) {
    return(df |> select(-iso3c))
  }
  df |>
    filter(date > max(gaps)) |>
    select(-iso3c)
}

get_baseline_size <- function(data, iso) {
  result <- setNames(
    data.frame(matrix(ncol = 3, nrow = 0)),
    c("iso3c", "type", "window")
  )
  asmr_types <- c("asmr_who", "asmr_esp", "asmr_usa", "asmr_country")
  types <- c("deaths", "cmr", asmr_types)
  for (type in types) {
    if (!type %in% colnames(data)) next

    print(paste(type))
    df <- data |>
      filter(date < 2020, !is.na(!!sym(type))) |>
      as_tsibble(index = date) |>
      filter_complete_latest()

    optimal_size <- ifelse(nrow(na.omit(df[type])) > 5,
      get_optimal_size(df, type),
      NA
    )

    result[nrow(result) + 1, ] <- c(iso, type, optimal_size)
    print(paste0("Optimal window size: ", optimal_size))
  }
  result
}

process_country <- function(iso3c) {
  result <- tibble()
  data <- read_remote(paste0("mortality/", iso3c, "/yearly.csv"))
  yearly <- data |>
    get_baseline_size(iso3c) |>
    mutate(chart_type = "yearly", .after = "iso3c")

  data <- read_remote(paste0("mortality/", iso3c, "/fluseason.csv"))
  fluseason <- data |>
    mutate(date = as.integer(left(date, 4))) |>
    get_baseline_size(iso3c) |>
    mutate(chart_type = "fluseason", .after = "iso3c")

  data <- read_remote(paste0("mortality/", iso3c, "/midyear.csv"))
  midyear <- data |>
    mutate(date = as.integer(left(date, 4))) |>
    get_baseline_size(iso3c) |>
    mutate(chart_type = "midyear", .after = "iso3c")

  rbind(result, rbind(yearly, fluseason, midyear))
}

meta <- read_remote("mortality/world_meta.csv")
codes <- unique(meta$iso3c)
with_progress({
  p <- progressor(steps = length(codes))
  result <- codes |>
    future_map(~ {
      p()
      process_country(.x)
    }) |>
    bind_rows()
})

save_csv(result, "mortality/world_baseline")

# source("mortality/baseline.r")
