
imputeNAPermutations <- function(df) {
  # Count NAs
  n <- sum(is.na(df$deaths))

  # No need for imputation
  if (n == 0) {
    return(df)
  }

  # Find sum target
  target <- df$deaths[df$age_group == "all"] -
    sum(df$deaths[df$age_group != "all"], na.rm = TRUE)

  # Only one possibility
  if (n == 1) {
    df$deaths[is.na(df$deaths)] <- target
    return(df)
  }

  # Multiple possibilities: Generate permutations of n values that sum to target
  res <- permuteGeneral(
    0:min(9, target), # Ensure all values are less than 10
    n,
    repetition = TRUE,
    constraintFun = "sum",
    comparisonFun = "==",
    limitConstraints = target
  )

  result <- tibble(df[0, ])
  for (i in seq_len(nrow(res))) {
    new_df <- tibble(df)
    new_df$deaths[is.na(new_df$deaths)] <- res[i, ]
    bind_rows(result, new_df)
    result <- result |> add_row(new_df)
  }
  return(result)
}

# Nest by iso3c, and date for imputation.
df_nested <- result |>
  nest(data = !c(iso3c)) |>
  mutate(data = map(data, ~ .x |> nest(data = !c(date))))

result_imputed <- df_nested |>
  mutate(
    data = map(data, "data") |> map_depth(2, ~ imputeNAPermutations(.x))
  ) |>
  unnest(cols = c(data)) |>
  unnest(cols = c(data))
