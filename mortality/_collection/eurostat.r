source("lib/common.r")
source("population/std_pop.r")

.data <- dplyr::.data

# Functions
custom_match <- c(EL = "GRC", UK = "GBR")

get_population_by_age_group <- function(df, age_groups) {
    result <- NULL
    for (ag in age_groups) {
        if (grepl("^[0-9]+$", ag)) {
            result <- rbind(result, data.frame(
                age_group = as.integer(ag),
                population = (
                    df |> filter(.data$age_group == as.integer(ag))
                )$population
            ))
        } else if (grepl("\\-", ag)) {
            parts <- split_age_group(ag)
            df2 <- df |> filter(.data$age_group %in% parts[1]:parts[2])
            result <- rbind(
                result,
                data.frame(age_group = ag, population = sum(df2$population))
            )
        } else if (grepl("\\+", ag)) {
            start <- as.integer(substr(ag, 0, nchar(ag) - 1))
            df2 <- df |> filter(.data$age_group %in% start:99)
            result <- rbind(
                result,
                data.frame(age_group = ag, population = sum(df2$population))
            )
        }
    }
    result
}

# Deaths
deaths_raw <- as_tibble(read.csv(
    gzfile("./data/eurostat_weekly.tsv.gz"),
    sep = "\t"
))

deaths_daily <- deaths_raw |>
    pivot_longer(
        cols = 2:ncol(deaths_raw),
        names_to = "date",
        values_to = "deaths"
    ) |>
    mutate(
        year = as.integer(right(left(date, 5), 4)),
        week = as.integer(right(date, 2))
    ) |>
    mutate(
        date = date_parse(paste(year, week, 1), format = "%G %V %u"),
        deaths = suppress_warnings(
            as.integer(str_replace_all(deaths, c(" p" = "", ": " = ""))),
            "NAs introduced by coercion"
        )
    ) |>
    filter(!is.na(deaths)) |>
    separate_wider_delim(
        age.sex.unit.geo.time,
        delim = ",",
        names = c("age_group", "sex", "unit", "iso3c")
    ) |>
    filter(sex == "T", age_group != "Y_GE80") |>
    mutate(
        age_group = case_when(
            age_group == "TOTAL" ~ "all",
            age_group == "UNK" ~ "NS",
            age_group == "Y80-89" ~ "80+",
            age_group == "Y_GE80" ~ "80+",
            age_group == "Y_GE90" ~ "80+",
            age_group == "Y_LT10" ~ "0-9",
            .default = suppress_warnings(
                str_replace_all(age_group, c("Y" = "")),
                "NAs introduced by coercion"
            )
        )
    ) |>
    select(iso3c, age_group, date, deaths) |>
    group_by(iso3c, age_group, date) |>
    summarize(deaths = sum(deaths)) |>
    get_daily_from_weekly(c("deaths"))

# Population
population_raw <- as_tibble(read.csv(
    gzfile("./data/eurostat_population.tsv.gz"),
    sep = "\t"
))

population <- population_raw |>
    pivot_longer(
        cols = 2:ncol(population_raw),
        names_to = "date",
        values_to = "population"
    ) |>
    mutate(date = as.integer(right(date, 4))) |>
    mutate(
        population = suppress_warnings(
            as.integer(str_replace_all(population, c(": " = ""))),
            "NAs introduced by coercion"
        )
    ) |>
    filter(!is.na(population)) |>
    separate_wider_delim(
        unit.age.sex.geo.time,
        delim = ",",
        names = c("unit", "age_group", "sex", "iso3c")
    ) |>
    filter(sex == "T") |>
    mutate(
        age_group = case_when(
            age_group == "TOTAL" ~ "all",
            age_group == "UNK" ~ "NS",
            age_group %in% c("Y_LT1", "Y_OPEN") ~ NA,
            .default = suppress_warnings(
                str_replace_all(age_group, c("Y" = "")),
                "NAs introduced by coercion"
            )
        )
    ) |>
    select(iso3c, age_group, date, population)

population_all <- population |> filter(age_group == "all")

# Summarize single ages to age groups.
age_groups <- unique(
    (deaths_daily |> filter(!age_group %in% c("all", "NS")))$age_group
)
population_age <- population |>
    filter(age_group != "all") |>
    rowwise() |>
    mutate(age_group = as_integer(age_group)) |>
    filter(!is.na(age_group)) |>
    nest(data = !c("iso3c", "date")) |>
    mutate(data = lapply(data, get_population_by_age_group, age_groups)) |>
    unnest(cols = c(data))

# Interpolate sub yearly population.
population_daily <- rbind(population_all, population_age) |>
    mutate(date = date(sprintf("%d-01-01", date))) |>
    nest(data = !c("iso3c")) |>
    mutate(
        data = map(data, ~ . |>
            group_by(age_group) |>
            nest() |>
            mutate(data = lapply(data, interpolate_population)) |>
            unnest(cols = "data"))
    ) |>
    unnest(cols = "data")

eurostat <- deaths_daily |>
    inner_join(population_daily, by = c("iso3c", "age_group", "date")) |>
    mutate(iso3c = countrycode(
        iso3c,
        origin = "iso2c",
        destination = "iso3c",
        custom_match = custom_match
    )) |>
    arrange(iso3c, date, age_group) |>
    distinct(iso3c, date, age_group, .keep_all = TRUE)

eurostat$type <- 3
eurostat$n_age_groups <- 9
eurostat$source <- "eurostat"
