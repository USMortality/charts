source("lib/common.r")
source("lib/asmr.r")
source("population/std_pop.r")

# Genesis 12411-0005: Bevölkerung: Deutschland, Stichtag, Altersjahre
population <- as_tibble(
  head(
    read.csv("./data_static/12411-0005_$F.csv",
      sep = ";",
      skip = 6,
      colClasses = c("character")
    ),
    -4
  )
)

# Genesis 12613-0003: Gestorbene: Deutschland, Jahre, Geschlecht, Altersjahre
deaths <- as_tibble(
  head(
    read.csv("./data_static/12613-0003_$F.csv",
      sep = ";",
      skip = 5,
      colClasses = c("character")
    ),
    -4
  )
)

# Parse raw data
population <- population |>
  pivot_longer(
    cols = 2:ncol(population),
    names_to = "year",
    values_to = "population"
  ) |>
  setNames(c("age_group", "year", "population")) |>
  mutate(
    age_group = case_when(
      age_group == "unter 1 Jahr" ~ "0",
      age_group == "85 Jahre und mehr" ~ "85+",
      age_group == "Alter unbekannt" ~ "NS",
      age_group == "Insgesamt" ~ "all",
      .default = str_replace(age_group, "-J�hrige", "")
    ),
    year = as.integer(right(year, 4)),
    population = as.integer(population),
    sex = "all"
  ) |>
  relocate(year, sex, age_group, population)

deaths <- deaths |>
  pivot_longer(
    cols = 3:ncol(deaths),
    names_to = "year",
    values_to = "deaths"
  ) |>
  setNames(c("sex", "age_group", "year", "deaths")) |>
  mutate(
    sex = left(sex, 1),
    age_group = case_when(
      age_group == "unter 1 Jahr" ~ "0",
      age_group == "100 Jahre und mehr" ~ "100+",
      age_group == "Alter unbekannt" ~ "NS",
      age_group == "Insgesamt" ~ "all",
      .default = str_replace(age_group, "-J�hrige", "")
    ),
    year = as.integer(right(year, 4)),
    deaths = as.integer(deaths)
  ) |>
  relocate(year, sex, age_group, deaths)
deaths$sex[deaths$sex == "I"] <- "all"
deaths <- deaths |>
  group_by(sex, year) |>
  group_modify(~ imputeSingleNA(.x)) |>
  ungroup()

df <- deaths |>
  mutate(age_group = ifelse(
    as.integer(age_group) >= 85 | age_group == "100+",
    "85+",
    age_group
  )) |>
  group_by(sex, year, age_group) |>
  summarise(deaths = sum(deaths)) |>
  ungroup() |>
  inner_join(population, by = c("year", "sex", "age_group")) |>
  select(-sex) |>
  mutate(cmr = deaths / population * 100000)

ts <- df |>
  mutate(age_group = as.integer(ifelse(age_group == "85+", "85", age_group))) |>
  arrange(year, age_group) |>
  filter(year > 2000)

sf <- 5
options(vsc.dev.args = list(width = 600 * sf, height = 335 * sf, res = 72))

# Deaths
ggplot(ts, aes(x = year, y = deaths)) +
  labs(
    title = "Yearly All-Cause Deaths by Single Age Groups [Germany]",
    subtitle = "Source: destatis.de",
    x = "Year",
    y = "Deaths"
  ) +
  geom_smooth(
    data = subset(ts, date >= 2010 & date <= 2019),
    fullrange = TRUE,
    color = "black",
    linetype = 5,
    size = 0.8,
    method = "lm"
  ) +
  watermark() +
  geom_line(color = "#5383EC", linewidth = 1) +
  twitter_theme() +
  scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3)) +
  facet_wrap(vars(age_group), scales = "free") +
  theme(
    plot.title = element_text(
      size = 18 * sf,
      face = "bold",
      color = "#333333"
    ),
    plot.subtitle = element_text(size = 14 * sf, color = "#999999")
  )

# CMR
ggplot(ts, aes(x = year, y = cmr)) +
  labs(
    title = "Yearly All-Cause CMR by Single Age Groups [Germany]",
    subtitle = "Source: destatis.de",
    x = "Year",
    y = "Deaths/100k"
  ) +
  geom_smooth(
    data = subset(ts, date >= 2010 & date <= 2019),
    fullrange = TRUE,
    color = "black",
    linetype = 5,
    size = 0.8,
    method = "lm"
  ) +
  watermark() +
  geom_line(color = "#5383EC", linewidth = 1) +
  twitter_theme() +
  facet_wrap(vars(age_group), scales = "free") +
  theme(
    plot.title = element_text(
      size = 18 * sf,
      face = "bold",
      color = "#333333"
    ),
    plot.subtitle = element_text(size = 14 * sf, color = "#999999")
  )

sf <- 2
options(vsc.dev.args = list(width = 600 * sf, height = 335 * sf, res = 72 * sf))

# ASMR
asmr_esp <- df |>
  mutate(iso3c = "DEU", date = year) |>
  calculate_asmr(get_esp2013_bins(unique(df$age_group)), "asmr_esp")

asmr_who <- df |>
  mutate(iso3c = "DEU", date = year) |>
  calculate_asmr(get_who2015_bins(unique(df$age_group)), "asmr_who")

asmr <- asmr_esp
asmr$asmr_who <- asmr_who$asmr_who

START_YEAR <- 1970

ggplot(asmr |> filter(date >= START_YEAR), aes(x = date)) +
  labs(
    title = "Yearly All-Cause ASMR (Single Age Groups) [Germany]",
    subtitle = paste0(
      c(
        "Baseline: 2010-2019",
        "99.9% CI",
        "Std. Population: WHO2015",
        "Source: destatis.de"
      ),
      collapse = " | "
    ),
    x = "Year",
    y = "Deaths/100k"
  ) +
  geom_smooth(
    mapping = aes(y = asmr_who),
    data = subset(asmr, date >= 2010 & date <= 2019),
    fullrange = TRUE,
    color = "black",
    linetype = 5,
    size = 0.8,
    method = "lm",
    level = 0.999
  ) +
  watermark() +
  geom_line(aes(y = asmr_who), color = "#5383EC", linewidth = 1) +
  twitter_theme()

ggplot(asmr |> filter(date >= START_YEAR), aes(x = date)) +
  labs(
    title = "Yearly All-Cause ASMR (Single Age Groups) [Germany]",
    subtitle = paste0(
      c(
        "Baseline: 2010-2019",
        "95% CI",
        "Std. Population: WHO2015",
        "Source: destatis.de"
      ),
      collapse = " | "
    ),
    x = "Year",
    y = "Deaths/100k"
  ) +
  geom_smooth(
    mapping = aes(y = asmr_who),
    data = subset(asmr, date >= 2010 & date <= 2019),
    fullrange = TRUE,
    color = "black",
    linetype = 5,
    size = 0.8,
    method = "lm",
    level = 0.95
  ) +
  watermark() +
  geom_line(aes(y = asmr_who), color = "#5383EC", linewidth = 1) +
  twitter_theme()

ggplot(asmr |> filter(date >= START_YEAR), aes(x = date)) +
  labs(
    title = "Yearly All-Cause ASMR (Single Age Groups) [Germany]",
    subtitle = paste0(
      c(
        "Baseline: 2010-2019",
        "99.9% CI",
        "Std. Population: ESP2013",
        "Source: destatis.de"
      ),
      collapse = " | "
    ),
    x = "Year",
    y = "Deaths/100k"
  ) +
  geom_smooth(
    mapping = aes(y = asmr_esp),
    data = subset(asmr, date >= 2010 & date <= 2019),
    fullrange = TRUE,
    color = "black",
    linetype = 5,
    size = 0.8,
    method = "lm",
    level = 0.999
  ) +
  watermark() +
  geom_line(aes(y = asmr_esp), color = "#5383EC", linewidth = 1) +
  twitter_theme()

ggplot(asmr |> filter(date >= START_YEAR), aes(x = date)) +
  labs(
    title = "Yearly All-Cause ASMR (Single Age Groups) [Germany]",
    subtitle = paste0(
      c(
        "Baseline: 2010-2019",
        "95% CI",
        "Std. Population: ESP2013",
        "Source: destatis.de"
      ),
      collapse = " | "
    ),
    x = "Year",
    y = "Deaths/100k"
  ) +
  geom_smooth(
    mapping = aes(y = asmr_esp),
    data = subset(asmr, date >= 2010 & date <= 2019),
    fullrange = TRUE,
    color = "black",
    linetype = 5,
    size = 0.8,
    method = "lm",
    level = 0.95
  ) +
  watermark() +
  geom_line(aes(y = asmr_esp), color = "#5383EC", linewidth = 1) +
  twitter_theme()
