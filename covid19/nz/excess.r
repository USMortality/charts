source("./lib/common.r")

# Yearly NZ data
df <- read.csv("https://s3.mortality.watch/data/mortality/world_fluseason.csv")
nz <- df |>
    filter(iso3c == "NZL") |>
    as_tibble() |>
    select(date, deaths) |>
    mutate(year = as.integer(left(date, 4))) |>
    filter(year >= 2009)

# Split data into training and test
df_train <- nz |> filter(year <= 2019)
df_test <- nz |> filter(year > 2019)

# Model
model <- lm(deaths ~ year, data = df_train)

# Forecast
oo <- lm_predict(model, df_test, FALSE)
fc_sum_mean <- sum(oo$fit)
fc_sum_variance <- sum(oo$var.fit)
alpha <- 0.999
Qt <- c(-1, 1) * qt((1 - alpha) / 2, model$df.residual, lower.tail = FALSE)
ci <- fc_sum_mean + Qt * sqrt(fc_sum_variance)

print(paste0(
    "Actual: ", sum(df_test$deaths),
    ", Expected: ", round(fc_sum_mean),
    ", 99.9%CI[", round(ci[1]), ",", round(ci[2]), "]"
))
print(paste0(
    "Excess: ", round(sum(df_test$deaths) - fc_sum_mean),
    ", 99.9%CI[", sum(df_test$deaths) - round(ci[1]), ",",
    sum(df_test$deaths) - round(ci[2]), "]"
))
