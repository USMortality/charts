source("lib/common.r")

get_optimal_size <- function(df, col_name) {
  min <- Inf
  optimal_size <- nrow(df)
  col_name <- sym(col_name)
  if (nrow(na.omit(df)) < 3) {
    return(optimal_size)
  }
  for (size in 5:ceiling(nrow(df) / 2)) {
    acc <- df |>
      slide_tsibble(.size = size) |>
      model(TSLM(!!col_name ~ trend())) |>
      forecast(h = 3) |>
      accuracy(df)
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
  types <- c("deaths", "cmr", "asmr")
  for (mortality_type in types) {
    mortality_col <- sym(mortality_type)
    for (country in unique(data$jurisdiction)) {
      print(paste(country, mortality_type))
      df <- data |>
        filter(
          jurisdiction == country,
          date < 2020
        ) |>
        as_tsibble(index = date)

      optimal_size <- ifelse(ceiling(nrow(df) / 2) > 5,
        get_optimal_size(df, mortality_type),
        nrow(df)
      )

      iso <- head(data |> filter(jurisdiction == country), 1)$iso3c
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
