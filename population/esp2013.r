source("lib/common.r")

esp2013_5y <- read.csv("data_static/ESP2013.csv") |> as_tibble()

split_age_group <- function(age_group) {
  ages <- strsplit(age_group, "-")
  c(as.integer(ages[[1]][1]), as.integer(ages[[1]][2]))
}

get_weights_2 <- function(df) {
  ag <- df$age_group
  if (!is.na(as.numeric(ag))) {
    data.frame(age_group = as.integer(ag), weight = df$weight)
  } else if (grepl("-", ag)) {
    ages <- split_age_group(ag)
    data.frame(
      age_group = ages[1]:ages[2],
      weight = df$weight / (ages[2] - ages[1] + 1)
    )
  } else if (grepl("+", ag)) {
    start <- as.integer(substr(ag, 0, nchar(ag) - 1))
    data.frame(age_group = start:99, weight = df$weight / (99 - start + 1))
  }
}

esp2013_yearly <- esp2013_5y |>
  mutate(key = age_group) |>
  nest(data = c(age_group, weight)) |>
  mutate(data = lapply(data, get_weights_2)) |>
  unnest(cols = c(data)) |>
  select(2, 3) |>
  setNames(c("age", "weight"))

get_weights <- function(age_groups, std_pop) {
  result <- NULL
  ag <- "90+"
  for (ag in age_groups) {
    if (!is.na(as.numeric(ag))) {
      result <- rbind(result, data.frame(
        age_group = ag,
        weight = (std_pop |> filter(age == ag))$weight
      ))
    } else if (grepl("-", ag)) {
      parts <- split_age_group(ag)
      weight <- std_pop |>
        filter(age %in% parts[1]:parts[2]) |>
        summarise(weight = sum(weight))
      result <- rbind(result, data.frame(age_group = ag, weight = weight))
    } else if (grepl("+", ag)) {
      start <- as.integer(substr(ag, 0, nchar(ag) - 1))
      weight <- std_pop |>
        filter(age %in% start:99) |>
        summarise(weight = sum(weight))
      result <- rbind(result, data.frame(age_group = ag, weight = weight))
    }
  }
  result
}
