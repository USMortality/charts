source("./lib/common.r")

# Yearly NZ data
df <- read.csv("https://s3.mortality.watch/data/mortality/NZL/fluseason.csv")
nz <- df |>
    filter(iso3c == "NZL") |>
    as_tibble() |>
    select(date, deaths) |>
    mutate(year = as.integer(left(date, 4))) |>
    filter(year >= 2010)

# Split data into training and test
df_train <- nz |> filter(year <= 2019)
df_test <- nz |> filter(year > 2019)

# Model
model <- lm(deaths ~ year, data = df_train)

# Forecast
oo <- lm_predict(model, df_test, FALSE)
fc_sum_mean <- sum(oo$fit)
fc_sum_variance <- sum(oo$var.fit)

n <- ncol(lengths(oo$var.fit))
res <- agg_pred(rep.int(x = 1, length(oo$fit)), oo, alpha = .95)

print(paste0(
    "Actual deaths: ", sum(df_test$deaths),
    ", Expected: ", round(fc_sum_mean),
    ", 95%PI[", round(res$PI[1]), ",", round(res$PI[2]), "]"
))
print(paste0(
    "Excess deaths: ", round(sum(df_test$deaths) - fc_sum_mean),
    ", 95%CI[", sum(df_test$deaths) - round(res$PI[2]), ",",
    sum(df_test$deaths) - round(res$PI[1]), "]"
))
