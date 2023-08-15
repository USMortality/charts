source("lib/common.r")

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

# Helper Functions:
## Splits an age group string by its start and end year.
split_age_group <- function(age_group) {
  ages <- strsplit(age_group, "-")
  c(as.integer(ages[[1]][1]), as.integer(ages[[1]][2]))
}

## Split weights for an age group uniformly.
get_weights <- function(df) {
  ag <- df$age_group
  if (grepl("^[0-9]+$", ag)) {
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

## Split weights by age years
get_std_pop_weights <- function(age_groups, std_pop) {
  result <- NULL
  for (ag in age_groups) {
    if (grepl("^[0-9]+$", ag)) {
      result <- rbind(result, data.frame(
        age_group = as.integer(ag),
        weight = (std_pop |> filter(.data$age == as.integer(ag)))$weight
      ))
    } else if (grepl("-", ag)) {
      parts <- split_age_group(ag)
      weight <- std_pop |>
        filter(.data$age %in% parts[1]:parts[2]) |>
        summarise(weight = sum(weight))
      result <- rbind(result, data.frame(age_group = ag, weight = weight))
    } else if (grepl("+", ag)) {
      start <- as.integer(substr(ag, 0, nchar(ag) - 1))
      weight <- std_pop |>
        filter(.data$age %in% start:99) |>
        summarise(weight = sum(weight))
      result <- rbind(result, data.frame(age_group = ag, weight = weight))
    }
  }
  result
}

parse_seer_std_table <- function(table_n, column_name) {
  df_2 <- rvest::html_nodes(tables, "table")
  df_3 <- rvest::html_table(df_2[table_n], fill = TRUE)
  df_4 <- df_3[[1]] |>
    select(all_of(c("Age", column_name))) |>
    setNames(c("age_group", "weight")) |>
    mutate(dplyr::across("age_group", \(x) str_replace(x, " years", "")))
  n_last <- nrow(df_4)
  total <- df_4[n_last, ]$weight |>
    stringr::str_replace_all(",", "") |>
    as.numeric()
  df_4[1:n_last - 1, ] |>
    filter(.data$age_group != "Total") |>
    mutate(
      key = .data$age_group,
      weight = as.numeric(
        stringr::str_replace_all(.data$weight, ",", "")
      ) / total
    ) |>
    nest(data = c("age_group", "weight")) |>
    mutate(data = lapply(data, get_weights)) |>
    unnest(cols = c(data)) |>
    select(all_of(c(2, 3))) |>
    setNames(c("age", "weight")) |>
    filter(!is.infinite("weight"))
}

tables <- read_html("https://seer.cancer.gov/stdpopulations/stdpop.19ages.html")
who2015 <- parse_seer_std_table(2, "World (WHO 2000-2025) Standard2")
usa2000 <- parse_seer_std_table(1, "2000 U.S. Standard Million")
esp2013 <- parse_seer_std_table(
  2,
  "European (EU-27 plus EFTA 2011-2030) Std Million"
)

get_esp2013_bins <- function(age_groups) {
  get_std_pop_weights(age_groups, esp2013)
}

get_usa2000_bins <- function(age_groups) {
  get_std_pop_weights(age_groups, usa2000)
}

get_who2015_bins <- function(age_groups) {
  get_std_pop_weights(age_groups, who2015)
}

get_country2020_bins <- function(df) {
  data1 <- df |>
    ungroup() |>
    filter(date == as.Date("2020-01-01"))
  if (nrow(data1) == 0) data1 <- df |> filter(date == 2020)
  if (nrow(data1) == 0) stop("No data for 2020 available.")
  data <- data1 |>
    select("age_group", "population") |>
    mutate(
      key = .data$age_group,
      weight = .data$population / sum(.data$population)
    ) |>
    select(-population) |>
    nest(data = c(age_group, weight)) |>
    mutate(data = lapply(data, get_weights)) |>
    unnest(cols = c(data)) |>
    ungroup() |>
    select("age_group", "weight") |>
    setNames(c("age", "weight")) |>
    filter(!is.infinite(weight))

  if (sum(data$weight) < 0.999) {
    stop("Weights do not sum up to 1.")
  }
  get_std_pop_weights(unique(df$age_group), data)
}
