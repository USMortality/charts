source("lib/common.r")

# Load Data
deaths <- as_tibble(read.csv("./data_static/euromomo.csv", sep = ";"))

death_nested <- deaths |> nest(!group)

data <- death_nested$data[[2]] |>
  select(1, 6) |>
  setNames(c("date", "deaths")) |>
  mutate(
    date = make_yearweek(
      year = as.integer(left(date, 4)),
      week = as.integer(right(date, 2))
    ),
    deaths = as.integer(deaths)
  )

data |>
  as_tsibble(index = date) |>
  autoplot()


data |>
  as_tsibble(index = date) |>
  model(STL(deaths)) %>%
  components() |>
  autoplot() +
  labs(
    title = "Weekly Deaths (15-44)",
    subtitle = "Source: euromomo.eu",
    y = "Deaths",
    x = "Week of Year"
  ) +
  scale_x_yearweek(date_labels = "%Y", breaks = "1 year")


data$deaths <- round(SMA(data$deaths, n = 52), 0)

data |>
  as_tsibble(index = date) |>
  autoplot()
