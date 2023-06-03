source("lib/common.r")

# Load Data
deaths <- as_tibble(read.csv("./data/world_mortality.csv"))
population <- read_remote("population/world.csv")

population <- population |>
  mutate(date = date(sprintf("%d-01-01", year)), .after = year) |>
  select(-year, -jurisdiction) |>
  nest(data = !iso3c) |>
  mutate(data = lapply(data, interpolate_population)) |>
  unnest(cols = c(data)) |>
  select(-is_projection)

# Deaths
wd <- deaths |>
  filter(time_unit == "weekly") |>
  mutate(
    date = make_yearweek(year = year, week = time),
    age_group = "all"
  ) |>
  select(iso3c, date, age_group, deaths) |>
  getDailyFromWeekly(c("deaths"))
wd$type <- "weekly"

md <- deaths |>
  filter(time_unit == "monthly") |>
  mutate(
    date = make_yearmonth(year = year, month = time),
    age_group = "all"
  ) |>
  select(iso3c, date, age_group, deaths) |>
  getDailyFromMonthly(c("deaths"))
md$type <- "monthly"

world_mortality <- rbind(wd, md) |>
  inner_join(population, by = c("iso3c", "date")) |>
  arrange(iso3c, date, age_group, type) |>
  distinct(iso3c, date, age_group, type, .keep_all = TRUE)
world_mortality$source <- "world_mortality"
