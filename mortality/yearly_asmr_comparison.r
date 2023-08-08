source("lib/common.r")
source("lib/asmr.r")

deaths <- read_remote("deaths/usa/yearly_5y_complete.csv") |>
  mutate(age_group = ifelse(
    age_group %in% c("85-89", "90-94", "95+"), "85+", age_group
  )) |>
  group_by(iso3c, date, age_group) |>
  summarise(deaths = sum(deaths)) |>
  ungroup()
population <- read_remote("population/usa/5y.csv")

df <- deaths |>
  inner_join(population, by = join_by(iso3c, date == year, age_group)) |>
  select(iso3c, date, age_group, deaths, population)

df2 <- df |>
  mutate(
    # Create categories
    age_group = case_when(
      age_group %in% c("0-4", "5-9") ~ "0-9",
      age_group %in% c("10-14", "15-19") ~ "10-19",
      age_group %in% c("20-24", "25-29") ~ "20-29",
      age_group %in% c("30-34", "35-39") ~ "30-39",
      age_group %in% c("40-44", "45-49") ~ "40-49",
      age_group %in% c("50-54", "55-59") ~ "50-59",
      age_group %in% c("60-64", "65-69") ~ "60-69",
      age_group %in% c("70-74", "75-79") ~ "70-79",
      age_group %in% c("80-84", "85+") ~ "80+",
      age_group %in% c("all") ~ "all"
    )
  ) |>
  group_by(iso3c, date, age_group) |>
  summarise(deaths = sum(deaths), population = sum(population)) |>
  ungroup()

df3 <- df |>
  mutate(
    # Create categories
    age_group = case_when(
      age_group %in% c("0-4", "5-9", "10-14", "15-19") ~ "0-19",
      age_group %in% c("20-24", "25-29", "30-34", "35-39") ~ "20-39",
      age_group %in% c("40-44", "45-49", "50-54", "55-59") ~ "40-59",
      age_group %in% c("60-64", "65-69", "70-74", "75-79") ~ "60-79",
      age_group %in% c("80-84", "85+") ~ "80+",
      age_group %in% c("all") ~ "all"
    )
  ) |>
  group_by(iso3c, date, age_group) |>
  summarise(deaths = sum(deaths), population = sum(population)) |>
  ungroup()

cmr <- df |> mutate(cmr = deaths / population * 100000)
asmr <- cmr |>
  filter(age_group != "all") |>
  group_by(iso3c) |>
  group_modify(~ calculate_asmr_variants(.x), .keep = TRUE) |>
  ungroup()
result <- cmr |>
  filter(age_group == "all") |>
  inner_join(asmr, by = c("iso3c", "date")) |>
  select(-any_of(age_group))

cmr2 <- df2 |> mutate(cmr = deaths / population * 100000)
asmr2 <- cmr2 |>
  filter(age_group != "all") |>
  group_by(iso3c) |>
  group_modify(~ calculate_asmr_variants(.x), .keep = TRUE) |>
  ungroup()
result2 <- cmr2 |>
  filter(age_group == "all") |>
  inner_join(asmr2, by = c("iso3c", "date")) |>
  select(-any_of(age_group)

cmr3 <- df3 |> mutate(cmr = deaths / population * 100000)
asmr3 <- cmr3 |>
  filter(age_group != "all") |>
  group_by(iso3c) |>
  group_modify(~ calculate_asmr_variants(.x), .keep = TRUE) |>
  ungroup()
result3 <- cmr3 |>
  filter(age_group == "all") |>
  inner_join(asmr3, by = c("iso3c", "date")) |>
  select(-any_of(age_group)

ts_diff <- result |>
  select(iso3c, date, cmr, asmr_country) |>
  inner_join(result2 |>
    select(iso3c, date, asmr_country), by = c("iso3c", "date")) |>
  inner_join(result3 |>
    select(iso3c, date, asmr_country), by = c("iso3c", "date"))

# Chart
jurisdiction <- "USA"
# chart1 <-
ggplot(
  ts_diff |>
    filter(iso3c == jurisdiction, date < 2023) |>
    as_tsibble(index = date),
  aes(x = date)
) +
  labs(
    title = paste0("Yearly Mortality [", jurisdiction, "]"),
    subtitle = paste0(
      "Source: CDC | Mortality.watch"
    ),
    y = "Deaths/100k",
    x = "Year"
  ) +
  # geom_line(aes(y = cmr, colour = "cmr"), linewidth = 1) +
  geom_line(
    aes(y = asmr_country.x, colour = "asmr_country.5y"),
    linewidth = 1
  ) +
  geom_line(
    aes(y = asmr_country.y, colour = "asmr_country.10y"),
    linewidth = 1
  ) +
  geom_line(
    aes(y = asmr_country, colour = "asmr_country.20y"),
    linewidth = 1
  ) +
  twitter_theme() +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
