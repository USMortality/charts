source("lib/common.r")

data <- read_remote("mortality/world_yearly.csv")

result <- setNames(
  data.frame(matrix(ncol = 4, nrow = 0)),
  c("iso3c", "name", "type", "window")
)

types <- c("cmr", "asmr")
asmr_data <- data %>% filter(!is.na(asmr))
for (mortality_type in types) {
  countries <- getCountriesForType(mortality_type, data, asmr_data)
  mortality_col <- sym(mortality_type)
  for (country in countries) {
    print(country)
    df <- data %>%
      filter(name == country) %>%
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
    iso <- head(df$iso3c, 1)
    result[nrow(result) + 1, ] <- c(iso, country, mortality_type, optimal_size)
    print(
      paste0("Optimal window size: ", optimal_size, ", RMSE: ", round(min, 1))
    )
  }
}

save_csv(result, "mortality/world_baseline")
