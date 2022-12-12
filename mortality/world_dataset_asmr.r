source("lib/common.r")

deaths <- as_tibble(read.csv("./data/mortality_org.csv", skip = 2))
std_pop <- as_tibble(
  read.csv("https://s3.mortality.watch/data/population/who_std_pop.csv")
)

wd <- deaths %>%
  filter(Sex == "b") %>%
  select(CountryCode, Year, Week, R0_14, R15_64, R65_74, R75_84, R85p) %>%
  mutate(date = make_yearweek(
    year = Year, week = Week
  )) %>%
  setNames(c(
    "iso3c", "year", "week", "0-14", "15-64", "65-74", "75-84", "85+", "date"
  )) %>%
  pivot_longer(
    cols = 4:8,
    names_to = "age_group",
    values_to = "mortality"
  ) %>%
  inner_join(std_pop, by = "age_group") %>%
  mutate(asmr = mortality / 52 * percentage) %>%
  select(-5, -7) %>%
  group_by(iso3c, year, week, date) %>%
  summarise(asmr = sum(asmr)) %>%
  mutate(asmr = asmr * 100000) %>%
  ungroup()

dd_asmr <- wd %>%
  uncount(7, .id = "day") %>%
  mutate(date = date_parse(
    paste0(year, "-W", week, "-1"),
    format = "%G-W%V-%u"
  )) %>%
  mutate(date = date + days(day - 1)) %>%
  mutate(asmr = asmr / 7) %>%
  select(iso3c, date, asmr) %>%
  distinct(iso3c, date, .keep_all = TRUE)

dd_asmr$iso3c[dd_asmr$iso3c == "DEUTNP"] <- "DEU"
dd_asmr$iso3c[dd_asmr$iso3c == "FRATNP"] <- "FRA"
dd_asmr$iso3c[dd_asmr$iso3c == "NZL_NP"] <- "NZL"
dd_asmr$iso3c[dd_asmr$iso3c == "GBR_NP"] <- "GBR"
