source("lib/common.r")
source("lib/parallel.r")

min_bl_len <- 3
max_bl_len <- 10
forecast_len <- 4
min_data_len <- min_bl_len + forecast_len

get_optimal_size <- function(df, type) {
  min <- Inf
  optimal_size <- min(max_bl_len, nrow(df))
  type <- sym(type)
  if (nrow(na.omit(df[type])) < min_bl_len + forecast_len) {
    return(NA)
  }

  for (size in min_bl_len:min(nrow(df) - forecast_len, max_bl_len)) {
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

    optimal_size <- ifelse(nrow(na.omit(df[type])) >= min_data_len,
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
  print(iso3c)
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
