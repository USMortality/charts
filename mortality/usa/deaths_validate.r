source("lib/common.r")

year_max <- 2022

usa_state_age <- read_csv("./out/deaths/usa/yearly_10y_completed.csv")
# USA/All was manually validated and is used as reference.

for (ag in unique(usa_state_age$age_group)) {
    usa_all <- usa_state_age |>
        filter(age_group == ag, iso3c == "USA") |>
        filter(date <= year_max)

    usa_states_summed <- usa_state_age |>
        filter(age_group == ag, iso3c != "USA") |>
        filter(date <= year_max) |>
        group_by(date) |>
        summarize(deaths = sum(deaths))

    stopifnot(identical(usa_all$deaths, usa_states_summed$deaths))
}

usa_state_age <- read_csv("./out/deaths/usa/yearly_5y_completed.csv")
# USA/All was manually validated and is used as reference.

for (ag in unique(usa_state_age$age_group)) {
    usa_all <- usa_state_age |>
        filter(age_group == ag, iso3c == "USA") |>
        filter(date <= year_max)

    usa_states_summed <- usa_state_age |>
        filter(age_group == ag, iso3c != "USA") |>
        filter(date <= year_max) |>
        group_by(date) |>
        summarize(deaths = sum(deaths))

    stopifnot(identical(usa_all$deaths, usa_states_summed$deaths))
}

usa_state_age <- read_csv("./out/deaths/usa/monthly_10y_imputed.csv")
# USA/All was manually validated and is used as reference.

for (ag in unique(usa_state_age$age_group)) {
    usa_all <- usa_state_age |>
        mutate(year = as.integer(left(date, 4))) |>
        filter(age_group == ag, iso3c == "USA") |>
        filter(year <= year_max) |>
        group_by(year) |>
        summarize(deaths = sum(deaths))

    usa_states_summed <- usa_state_age |>
        mutate(year = as.integer(left(date, 4))) |>
        filter(age_group == ag, iso3c != "USA") |>
        filter(year <= year_max) |>
        group_by(year) |>
        summarize(deaths = sum(deaths))

    stopifnot(identical(usa_all$deaths, usa_states_summed$deaths))
}

usa_state_age <- read_csv("./out/deaths/usa/yearly_5y_completed.csv")
# USA/All was manually validated and is used as reference.

for (ag in unique(usa_state_age$age_group)) {
    usa_all <- usa_state_age |>
        mutate(year = as.integer(left(date, 4))) |>
        filter(age_group == ag, iso3c == "USA") |>
        filter(year <= year_max) |>
        group_by(year) |>
        summarize(deaths = sum(deaths))

    usa_states_summed <- usa_state_age |>
        mutate(year = as.integer(left(date, 4))) |>
        filter(age_group == ag, iso3c != "USA") |>
        filter(year <= year_max) |>
        group_by(year) |>
        summarize(deaths = sum(deaths))

    stopifnot(identical(usa_all$deaths, usa_states_summed$deaths))
}

print("All sums match!")
