source("lib/common.r")
source("lib/asmr.r")
source("mortality/world_dataset_functions.r")
source("population/std_pop.r")

# Create sample dataframe
df <- tibble(
  age_group = c("0-24", "25-44", "45-64", "65-74", "75-84", "85+"),
  deaths = c(2, 4, 10, 21, 49, 30),
  population = c(100000, 200000, 150000, 100000, 80000, 50000)
) |> adorn_totals("row", name = "total")

# Omit <10
df_na <- df |> mutate(deaths = ifelse(deaths < 10, NA, deaths))

# Count NAs
n <- sum(is.na(df_na$deaths))

# Find sum target
target <- df_na$deaths[df_na$age_group == "total"] -
  sum(df_na$deaths[df_na$age_group != "total"], na.rm = TRUE)

# Generate permutations of n values that sum to target
res <- permuteGeneral(
  0:min(9, target), # Ensure all values are less than 10
  n,
  repetition = TRUE,
  constraintFun = "sum",
  comparisonFun = "==",
  limitConstraints = target
)

result <- tibble()
for (i in seq_len(nrow(res))) {
  new_df <- tibble(df_na)
  new_df$deaths[is.na(new_df$deaths)] <- res[i, ]
  new_df$iso3c <- rep("USA", nrow(df))
  new_df$date <- rep(make_yearmonth(year = 2020, month = 1), nrow(df))
  new_df$seed <- i
  result <- rbind(result, new_df)
}

result <- result |> mutate(cmr = deaths / population * 100000)
result <- result |> nest(data = !seed)
std_pop <- get_esp2013_bins((df_na |> filter(age_group != "total"))$age_group)

estimates <- result |>
  mutate(data = lapply(data, calculate_asmr, std_pop, "asmr")) |>
  unnest(cols = c(data))

mean <- mean(estimates$asmr)
sd <- sd(estimates$asmr)
lo <- qnorm(0.05 / 2, mean = mean, sd = sd) # lower CI bound
hi <- qnorm(1 - 0.05 / 2, mean = mean, sd = sd) # upper CI bound
print(paste0(
  "ASMR: ", round(mean, 1),
  " (95% CI: ", round(lo, 1), "-",
  round(hi, 1), ")"
))

plot(estimates$asmr, y, main = "Normal Distribution", col = "blue")

actual <- (df |>
  mutate(
    iso3c = rep("USA", nrow(df)),
    date = rep(make_yearmonth(year = 2020, month = 1), nrow(df))
  ) |>
  mutate(cmr = deaths / population * 100000) |>
  calculate_asmr(std_pop, "asmr"))$asmr

print(paste0((actual / estimate - 1) * 100, "%"))
