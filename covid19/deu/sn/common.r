source("lib/common.r")

SEP <- " · "

# Genesis 12613-020Z: Gestorbene (je 1000 EW) auf Gemeinden
cmr <- as_tibble(
  head(
    read.csv("./data_static/12613-020Z.csv",
      sep = ";",
      skip = 5,
      colClasses = c("character"),
      fileEncoding = "latin1"
    ),
    -4
  )
) |>
  setNames(c("date", "id", "jurisdiction", "cmr")) |>
  rowwise() |>
  mutate(
    jurisdiction = trimws(jurisdiction),
    level = nchar(id),
    date = as.integer(date),
    cmr = ifelse(cmr == "-", NA, as.double(gsub(",", ".", cmr)))
  )

# Genesis 12613-010Z: Gestorbene (absolut) nach Geschlecht auf Gemeinden
deaths <- as_tibble(
  head(
    read.csv("./data_static/12613-010Z.csv",
      sep = ";",
      skip = 5,
      colClasses = c("character"),
      fileEncoding = "latin1"
    ),
    -4
  )
) |>
  setNames(c(
    "date", "id", "jurisdiction", "deaths_all", "deaths_m", "deaths_f"
  )) |>
  pivot_longer(
    cols = 4:6,
    names_to = "sex",
    names_prefix = "deaths_",
    values_to = "deaths"
  ) |>
  filter(date != "", id != "", sex != "", deaths != "") |>
  rowwise() |>
  mutate(
    date = as.integer(date),
    jurisdiction = trimws(jurisdiction),
    level = nchar(id),
    deaths = as_integer(deaths)
  )

# Genesis 12410-010Z: Bevölkerung Fortschreibung zum 30.06. nach Geschlecht
# auf Gemeinden
population <- as_tibble(
  head(
    read.csv("./data_static/12410-010Z.csv",
      sep = ";",
      skip = 5,
      colClasses = c("character"),
      fileEncoding = "latin1"
    ),
    -4
  )
) |>
  setNames(c(
    "date", "id", "jurisdiction", "population_all", "population_m",
    "population_f"
  )) |>
  pivot_longer(
    cols = 4:6,
    names_to = "sex",
    names_prefix = "population_",
    values_to = "population"
  ) |>
  filter(date != "", id != "", sex != "", population != "") |>
  rowwise() |>
  mutate(
    date = year(as.Date(date, format = "%d.%m.%Y")),
    jurisdiction = trimws(jurisdiction),
    level = nchar(id),
    population = as_integer(population)
  )

# Calculate Zschepplin data, since cmr/population data is missing.
# Use 2021 population for 2022.
z_d <- deaths |>
  filter(jurisdiction == "Zschepplin", date == 2022, sex == "all")
z_p <- population |>
  filter(jurisdiction == "Zschepplin", date == 2021, sex == "all")
cmr <- cmr |> add_row(tibble(
  date = 2022L,
  id = "14730360",
  jurisdiction = "Zschepplin",
  cmr = z_d$deaths / z_p$population * 1000,
  level = 8L
))

df <- cmr |>
  left_join(
    population |> filter(sex == "all") |> select(date, id, population),
    by = join_by(date, id)
  )
