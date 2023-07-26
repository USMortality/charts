source("lib/common.r")

data1 <- as_tibble(
  read.csv("./data_static/population_usa_county_2010-2020.csv")
) |> filter(SUMLEV == 50)
pop1 <- data1 |>
  mutate(
    fips = paste0(
      str_pad(STATE, 2, "left", "0"),
      str_pad(COUNTY, 3, "left", "0")
    ),
    .before = CTYNAME
  ) |>
  select(7, 8, 11:ncol(data1)) |>
  pivot_longer(
    cols = 3:13,
    names_to = "year",
    values_to = "population",
    names_prefix = "POPESTIMATE"
  )

data2 <- as_tibble(
  read.csv("./data_static/population_usa_county_2020-2021.csv")
) |> filter(SUMLEV == 50)
pop2 <- data2 |>
  mutate(
    fips = paste0(
      str_pad(STATE, 2, "left", "0"),
      str_pad(COUNTY, 3, "left", "0")
    ),
    .before = CTYNAME
  ) |>
  select(7, 8, 10:12) |>
  pivot_longer(
    cols = 3:5,
    names_to = "year",
    values_to = "population",
    names_prefix = "POPESTIMATE"
  )

pop <- rbind(pop1, pop2) |>
  setNames(c("fips", "county", "year", "population")) |>
  filter(year %in% 2010:2021) |>
  mutate(year = as.integer(year)) |>
  arrange(fips, year)

deaths <- as_tibble(read.csv("./data_static/us-county-deaths.csv")) |>
  select(County, County.Code, Month.Code, Deaths) |>
  setNames(c("county", "fips", "date", "deaths")) |>
  mutate(
    fips = str_pad(fips, 5, "left", "0"),
    year = as.integer(left(date, 4))
  ) |>
  mutate(date = make_yearmonth(year = year, month = right(date, 2)))

cmr <- deaths |>
  inner_join(pop, by = c("fips", "year")) |>
  select(fips, county.x, year, date, deaths, population) |>
  setNames(c("fips", "county", "year", "date", "deaths", "population")) |>
  mutate(cmr = round(deaths / population * 100000, digits = 1))

find_gaps <- function(df) {
  ts <- df |> as_tsibble(index = date)
  ts$has_gaps <- nrow(scan_gaps(ts))
  ts |>
    as_tibble() |>
    select(-fips)
}

calculate_excess <- function(df) {
  data <- df |>
    as_tsibble(index = date) |>
    select(-fips)

  fit <- subset(data, year < 2020) |>
    model(TSLM(cmr ~ trend() + season())) |>
    forecast()
  bl <- data.frame(date = fit$date, baseline = round(fit$.mean, digits = 1))

  data |>
    inner_join(bl, by = c("date")) |>
    mutate(eCMR = round(cmr - baseline, digits = 1)) |>
    mutate(eCMR_p = round(eCMR / baseline, digits = 3)) |>
    as_tibble()
}

df <- cmr |>
  group_by(fips) |>
  filter(n() >= 24) |>
  group_modify(~ find_gaps(.x), .keep = TRUE) |>
  filter(has_gaps == 0) |>
  group_modify(~ calculate_excess(.x), .keep = TRUE) |>
  ungroup()

plot_chart <- function(df, d, state) {
  df2 <- df |> filter(as.Date(date) == d)
  chart <- usmap::plot_usmap(
    "counties",
    data = df2,
    values = "eCMR_p",
    # include = c(state),
    color = NA
  ) +
    scale_fill_viridis_c(
      option = "A",
      name = "excess CMR (%)",
      limits = c(min(df$eCMR_p), max(df$eCMR_p)),
      label = scales::percent,
      na.value = "#000000"
    ) +
    easy_move_legend(to = c("right")) +
    labs(title = "All-Cause Excess Mortality (%)", subtitle = paste(state, yearmonth(as.Date(d)))) +
    theme(panel.background = element_rect(colour = "white"))

  save_chart(chart, paste("cmr", state, as.Date(d), sep = "_"), upload = FALSE)
}

for (d in unique(df$date)) {
  plot_chart(df, d, "USA")
}
