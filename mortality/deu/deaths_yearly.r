source("lib/common.r")
source("lib/asmr.r")
source("population/std_pop.r")

# Define default functions
select <- dplyr::select
filter <- dplyr::filter
mutate <- dplyr::mutate
group_by <- dplyr::group_by
ungroup <- dplyr::ungroup
summarise <- dplyr::summarise
inner_join <- dplyr::inner_join
relocate <- dplyr::relocate
year <- lubridate::year
month <- lubridate::month
week <- lubridate::week
days <- lubridate::days
days_in_month <- lubridate::days_in_month
as_tibble <- tibble::as_tibble
tibble <- tibble::tibble
as_tsibble <- tsibble::as_tsibble
str_replace <- stringr::str_replace
uncount <- tidyr::uncount
sym <- rlang::sym
model <- fabletools::model
date <- lubridate::date
forecast <- fabletools::forecast
select <- dplyr::select
all_of <- dplyr::all_of
nest <- tidyr::nest
unnest <- tidyr::unnest
.data <- dplyr::.data
yearmonth <- tsibble::yearmonth
yearweek <- tsibble::yearweek
ggplot <- ggplot2::ggplot

# Genesis 12411-0005: Bevölkerung: Deutschland, Stichtag, Altersjahre
population_raw <- as_tibble(
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
deaths_raw <- as_tibble(
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
population <- population_raw |>
  pivot_longer(
    cols = 2:ncol(population_raw),
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

deaths <- deaths_raw |>
  pivot_longer(
    cols = 3:ncol(deaths_raw),
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
    year = sapply(right(year, 4), as_integer),
    deaths = sapply(deaths, as_integer)
  ) |>
  relocate(year, sex, age_group, deaths)
deaths$sex[deaths$sex == "I"] <- "all"
deaths <- deaths |>
  group_by(sex, year) |>
  group_modify(~ impute_single_na(.x)) |>
  ungroup()

df <- deaths |>
  rowwise() |>
  mutate(age_group = ifelse(
    as_integer(age_group) >= 85 | age_group == "100+",
    "85+",
    age_group
  )) |>
  group_by(sex, year, age_group) |>
  summarise(deaths = sum(deaths)) |>
  ungroup() |>
  inner_join(population, by = c("year", "sex", "age_group")) |>
  select(-any_of("sex")) |>
  mutate(cmr = deaths / population * 100000)

ts <- df |>
  rowwise() |>
  mutate(age_group = as_integer(ifelse(age_group == "85+", "85", age_group))) |>
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
    data = subset(ts, year >= 2010 & year <= 2019),
    fullrange = TRUE,
    color = "black",
    linetype = 5,
    size = 0.8,
    method = "lm"
  ) +
  watermark(max(ts$year)) +
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
    data = subset(ts, year >= 2010 & year <= 2019),
    fullrange = TRUE,
    color = "black",
    linetype = 5,
    size = 0.8,
    method = "lm"
  ) +
  watermark(max(ts$year)) +
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
asmr <- asmr |> mutate(year = date, date = as.Date(paste0(date, "-01-01")))

start_year <- 2010

ggplot(asmr |> filter(year >= start_year), aes(x = date)) +
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
    data = subset(asmr, year >= 2010 & year <= 2019),
    fullrange = TRUE,
    color = "black",
    linetype = 5,
    size = 0.8,
    method = "lm",
    level = 0.95
  ) +
  watermark(max(asmr$date)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  geom_line(aes(y = asmr_esp), color = "#5383EC", linewidth = 1) +
  twitter_theme()

ggplot(asmr |> filter(date >= start_year), aes(x = date)) +
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
  watermark(max(asmr$date)) +
  geom_line(aes(y = asmr_who), color = "#5383EC", linewidth = 1) +
  twitter_theme()

ggplot(asmr |> filter(date >= start_year), aes(x = date)) +
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
  watermark(max(asmr$date)) +
  geom_line(aes(y = asmr_esp), color = "#5383EC", linewidth = 1) +
  twitter_theme()

ggplot(asmr |> filter(date >= start_year), aes(x = date)) +
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
  watermark(max(asmr$date)) +
  geom_line(aes(y = asmr_esp), color = "#5383EC", linewidth = 1) +
  twitter_theme()

# 1990 log scale
ggplot(asmr |> filter(date >= 1990), aes(x = date)) +
  labs(
    title = "Yearly All-Cause ASMR (Single Age Groups) [Germany] {LOG}",
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
  watermark(max(asmr$date)) +
  scale_y_continuous(trans = "log2") +
  geom_line(aes(y = asmr_esp), color = "#5383EC", linewidth = 1) +
  twitter_theme()

bl_len <- 10
h_step_fc <- 2

calc_excess <- function(df) {
  fc <- head(df, bl_len) |>
    as_tsibble(index = year) |>
    model(fable::RW(asmr_esp ~ drift())) |>
    forecast(h = h_step_fc)

  result <- tail(df, h_step_fc)
  result$expected <- fc$.mean
  result$excess <- result$asmr_esp - result$expected
  result$excess_p <- (result$asmr_esp - result$expected) / result$asmr_esp
  result
}

data_excess <- asmr |>
  select(-any_of(c("iso3c", "asmr_who"))) |>
  as_tsibble(index = date) |>
  slide_tsibble(.size = (bl_len + h_step_fc)) |>
  group_by(.id) |>
  group_modify(~ calc_excess(.x)) |>
  mutate(id = row_number()) |>
  ungroup()

ggplot(
  data_excess |> filter(id == h_step_fc) |> as_tsibble(index = date),
  aes(x = date)
) +
  labs(
    title = "Yearly All-Cause excess ASMR [Germany]",
    subtitle = paste0(
      c(
        paste0(h_step_fc, " year forecast"),
        "Single Age Groups",
        "Std. Population: ESP2013",
        "Source: destatis.de"
      ),
      collapse = " | "
    ),
    x = "Forecast Year",
    y = "Excess Deaths/100k"
  ) +
  watermark() +
  geom_col(aes(y = excess), fill = "#5383EC") +
  twitter_theme() +
  scale_y_continuous(limits = c(-100, 100))

ggplot(
  data_excess |> filter(id == h_step_fc) |> as_tsibble(index = date),
  aes(x = date)
) +
  labs(
    title = "Yearly All-Cause excess ASMR [Germany]",
    subtitle = paste0(
      c(
        paste0(h_step_fc, " year forecast"),
        "Single Age Groups",
        "Std. Population: ESP2013",
        "Source: destatis.de"
      ),
      collapse = " | "
    ),
    x = "Forecast Year",
    y = "Excess ASMR (%)"
  ) +
  watermark() +
  geom_col(aes(y = excess_p), fill = "#5383EC") +
  twitter_theme() +
  scale_y_continuous(labels = scales::percent, limits = c(-0.1, .10))

# Animate
make_chart <- function(df) {
  chart <- ggplot(df, ggplot2::aes(x = date)) +
    ggplot2::labs(
      title = "Yearly All-Cause ASMR (Single Age Groups) [Germany]",
      subtitle = paste0(
        c(
          "95% CI",
          "Std. Population: ESP2013",
          "Source: destatis.de"
        ),
        collapse = " | "
      ),
      x = "Year",
      y = "Deaths/100k"
    ) +
    ggplot2::geom_smooth(
      mapping = aes(y = asmr_esp),
      data = head(df, bl_len),
      fullrange = TRUE,
      color = "black",
      linetype = 5,
      size = 0.8,
      method = "lm",
      level = 0.95
    ) +
    watermark(max(df$date)) +
    ggplot2::geom_line(aes(y = asmr_esp), color = "#5383EC", linewidth = 1) +
    ggplot2::scale_x_continuous(breaks = df$date) +
    twitter_theme()
  save_chart(
    chart,
    paste("mortality", "deu", paste0(
      "forecast_", stringr::str_pad(unique(df$.id), 2, pad = "0")
    ), sep = "/"),
    upload = FALSE
  )
}

asmr |>
  select(-any_of(c("iso3c", "asmr_who"))) |>
  as_tsibble(index = year) |>
  slide_tsibble(.size = (bl_len + h_step_fc)) |>
  group_by(.id) |>
  group_map(~ make_chart(.x), .keep = TRUE)

# ffmpeg -hide_banner -loglevel error -r 1 -pattern_type glob -i '*.png' -c:v
# libx264 -vf "fps=1,format=yuv420p,scale=1200x670" _movie.mp4ffmpeg
