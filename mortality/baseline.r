source("lib/common.r")

data <- read_remote("mortality/world_yearly.csv")

result <- setNames(
  data.frame(matrix(ncol = 3, nrow = 0)),
  c("iso3c", "type", "window")
)

types <- c("cmr", "asmr")
asmr_data <- data %>% filter(!is.na(asmr))
for (type in types) {
  countries <- if (type == "cmr") {
    unique(data$iso3c)
  } else {
    unique(asmr_data$iso3c)
  }
  mortality_col <- sym(type)
  for (iso in countries) {
    print(iso)
    df <- data %>%
      filter(iso3c == iso) %>%
      as_tsibble(index = date)

    training_data <- df %>% filter(date < 2020)
    min <- Inf
    optimal_size <- 3
    if (nrow(training_data) >= 3 && nrow(df) >= nrow(training_data) + 1) {
      for (size in 3:min(length(training_data$date), 10)) {
        acc <- training_data %>%
          slide_tsibble(.size = size) %>%
          model(TSLM(!!mortality_col ~ trend())) %>%
          forecast(h = 3) %>%
          accuracy(df)
        if (acc$RMSE < min) {
          min <- acc$RMSE
          optimal_size <- size
        }
      }
    }
    result[nrow(result) + 1, ] <- c(iso, type, optimal_size)
    print(
      paste0("Optimal window size: ", optimal_size, ", RMSE: ", round(min, 1))
    )
  }
}

save_csv(result, "mortality/world_baseline")
