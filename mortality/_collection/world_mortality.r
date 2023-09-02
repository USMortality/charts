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
    date = date_parse(paste(year, time, 1), format = "%G %V %u"),
    age_group = "all"
  ) |>
  select(iso3c, date, age_group, deaths) |>
  get_daily_from_weekly(c("deaths"))
wd$type <- 3

md <- deaths |>
  filter(time_unit == "monthly") |>
  mutate(
    date = date_parse(paste(year, time, 1), format = "%Y %m %d"),
    age_group = "all"
  ) |>
  select(iso3c, date, age_group, deaths) |>
  get_daily_from_monthly(c("deaths"))
md$type <- 2

world_mortality <- rbind(wd, md) |>
  inner_join(population, by = c("iso3c", "date")) |>
  arrange(iso3c, date, age_group, type) |>
  distinct(iso3c, date, age_group, type, .keep_all = TRUE)
world_mortality$source <- "world_mortality"
world_mortality$n_age_groups <- 1
