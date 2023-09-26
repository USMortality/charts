source("lib/common.r")

filter_complete_latest <- function(df) {
    gaps <- df |>
        as_tsibble(index = year) |>
        scan_gaps()
    if (nrow(gaps) == 0) {
        return(df |> select(-iso3c))
    }
    df |>
        filter(year > max(gaps)) |>
        select(-iso3c)
}

world <- read_excel(
    "./data_static/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx",
    sheet = "Estimates",
    range = "F17:AE30000",
    col_types = "text"
)

un <- world |>
    select(1, 6, 7, 26) |>
    setNames(c("iso3c", "year", "population", "deaths")) |>
    filter(!is.na(iso3c), !is.na(year)) |>
    arrange(iso3c, year) |>
    rowwise() |>
    mutate(
        year = as_integer(year),
        population = round(as_double(population) * 1000),
        deaths = round(as_double(deaths) * 1000)
    ) |>
    mutate(date = date(sprintf("%d-01-01", year))) |>
    filter(!is.na(deaths)) |>
    group_by(iso3c) |>
    group_modify(~ filter_complete_latest(.x), .keep = TRUE) |>
    ungroup() |>
    select(-year)

un$source <- "un"
un$n_age_groups <- 1
un$type <- 1
un$age_group <- "all"
