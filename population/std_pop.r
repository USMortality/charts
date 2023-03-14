source("lib/common.r")

# Helper Functions:

## Splits an age group string by its start and end year.
split_age_group <- function(age_group) {
  ages <- strsplit(age_group, "-")
  c(as.integer(ages[[1]][1]), as.integer(ages[[1]][2]))
}

## Split weights for an age group uniformly.
get_weights <- function(df) {
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

##
get_std_pop_weights <- function(age_groups, std_pop) {
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


# ESP 2013 Std. Pop
esp2013_5y <- read.csv("data_static/ESP2013.csv") |> as_tibble()
esp2013 <- esp2013_5y |>
  mutate(key = age_group) |>
  nest(data = c(age_group, weight)) |>
  mutate(data = lapply(data, get_weights)) |>
  unnest(cols = c(data)) |>
  select(2, 3) |>
  setNames(c("age", "weight"))

ggplot(esp2013, aes(x = age, y = weight)) +
  geom_line()

get_esp2013_bins <- function(age_groups) {
  get_std_pop_weights(age_groups, esp2013)
}


# WHO 2015 Std. Pop

who2015_1 <- read_html("https://seer.cancer.gov/stdpopulations/world.who.html")
who2015_2 <- html_nodes(who2015_1, "table")
who2015 <- html_table(who2015_2[1], fill = TRUE)

get_who2015_bins <- function(age_groups) {
  get_std_pop_weights(age_groups, who2015)
}

# pop_5
get_esp2013_bins(c("0-14", "15-64", "65-74", "75-84", "85+"))

# pop_6
get_esp2013_bins(c("0-24", "25-44", "45-64", "65-74", "75-84", "85+"))

# pop_4
get_esp2013_bins(c("0-64", "65-74", "75-84", "85+"))

# pop_9
get_esp2013_bins(c(
  "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"
))
