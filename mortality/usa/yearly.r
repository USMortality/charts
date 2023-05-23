source("lib/common.r")
source("lib/asmr.r")

deaths <- read_remote("deaths/usa/yearly_5y_imputed.csv") |>
  mutate(age_group = ifelse(
    age_group %in% c("85-89", "90-94", "95+"), "85+", age_group
  )) |>
  group_by(iso3c, date, age_group) |>
  summarise(deaths = sum(deaths)) |>
  ungroup()
population <- read_remote("population/usa/5y.csv")

cmr <- deaths |>
  inner_join(population, by = join_by(iso3c, date == year, age_group)) |>
  select(iso3c, date, age_group, deaths, population) |>
  mutate(cmr = deaths / population * 100000)

asmr <- cmr |>
  filter(age_group != "all") |>
  group_by(iso3c) |>
  group_modify(~ calculate_asmr_variants(.x), .keep = TRUE) |>
  ungroup()

result <- cmr |>
  filter(age_group == "all") |>
  inner_join(asmr, by = c("iso3c", "date")) |>
  select(-age_group)

save_csv(result, "mortality/usa/yearly")

# Chart
jurisdiction <- "USA-VT"
ts <- result |>
  filter(iso3c == jurisdiction) |>
  as_tsibble(index = date)
# chart1 <-
ggplot(ts, aes(x = date)) +
  labs(
    title = paste0("Yearly Mortality [", jurisdiction, "]"),
    subtitle = paste0(
      "Source: CDC | Mortality.watch"
    ),
    y = "Deaths/100k",
    x = "Year"
  ) +
  geom_line(aes(y = cmr, colour = "cmr"), linewidth = 1) +
  geom_line(aes(y = asmr_who, colour = "asmr_who"), linewidth = 1) +
  geom_line(aes(y = asmr_esp, colour = "asmr_esp"), linewidth = 1) +
  geom_line(aes(y = asmr_usa, colour = "asmr_usa"), linewidth = 1) +
  geom_line(aes(y = asmr_country, colour = "asmr_country"), linewidth = 1) +
  twitter_theme() +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
