source("lib/common.r")

data <- read.csv("./data/owid.csv") |> as_tibble()

colnames(data)

df <- data |>
    select(
        iso_code,
        location,
        date,
        people_vaccinated_per_hundred,
        total_vaccinations_per_hundred,
        people_fully_vaccinated_per_hundred,
        total_boosters_per_hundred,
        population,
        human_development_index
    ) |>
    mutate(date = date(date)) |>
    filter(
        population > 1000000,
        human_development_index > 0.8,
        !is.na(people_vaccinated_per_hundred)
    )

vaccinated <- df |>
    group_by(iso_code, location) |>
    summarise(
        people_vaccinated_per_hundred = max(
            people_vaccinated_per_hundred,
            .na.rm = TRUE
        )
    ) |>
    arrange(desc(people_vaccinated_per_hundred))

vaccinated |> head(20)
vaccinated |> tail(20)
