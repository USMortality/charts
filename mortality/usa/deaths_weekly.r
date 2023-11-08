source("lib/common.r")

data1 <- as_tibble(read.csv("./data_static/usa_states_excess_weekly.csv"))
data2 <- as_tibble(read.csv("./data_static/usa_states_age_weekly.csv"))
us_states_iso3c <- as_tibble(read.csv("./data_static/usa_states_iso3c.csv"))

df1 <- data1 |>
  filter(Type == "Predicted (weighted)", Outcome == "All causes") |>
  select("State", "Week.Ending.Date", "Year", "Observed.Number") |>
  setNames(c("jurisdiction", "date", "year", "deaths")) |>
  mutate(
    date = yearweek(ymd(date)),
    age_group = "all",
    deaths = as.integer(str_replace(deaths, ",", ""))
  ) |>
  select(-"year")

# Combine NY/NYC
ny <- df1 |>
  filter(jurisdiction %in% c("New York", "New York City")) |>
  group_by(date, age_group) |>
  summarise(deaths = sum(deaths)) |>
  ungroup()
ny$jurisdiction <- "New York"
df1 <- rbind(df1 |> filter(jurisdiction != "New York"), ny)

df2 <- data2 |>
  filter(Type == "Unweighted") |>
  select(Jurisdiction, Week.Ending.Date, Age.Group, Number.of.Deaths) |>
  setNames(c("jurisdiction", "date", "age_group", "deaths")) |>
  mutate(
    date = yearweek(mdy(date)),
    # Create categories
    age_group = case_when(
      age_group == "Under 25 years" ~ "0-24",
      age_group == "25-44 years" ~ "25-44",
      age_group == "45-64 years" ~ "45-64",
      age_group == "65-74 years" ~ "65-74",
      age_group == "75-84 years" ~ "75-84",
      age_group == "85 years and older" ~ "85+"
    )
  )

# Combine NY/NYC
ny <- df2 |>
  filter(jurisdiction %in% c("New York", "New York City")) |>
  group_by(date, age_group) |>
  summarise(deaths = sum(deaths)) |>
  ungroup()
ny$jurisdiction <- "New York"
df2 <- rbind(df2 |> filter(jurisdiction != "New York"), ny)

df3 <- df2 |> filter(jurisdiction == "United States", age_group == "85+")
df3$age_group <- "NS"
df3$deaths <- NA

df <- rbind(df1, df2, df3)

result_1 <- df |>
  left_join(us_states_iso3c, by = "jurisdiction") |>
  select(-"jurisdiction") |>
  mutate(year = year(date), week = isoweek(date)) |>
  arrange(iso3c, date, age_group)

# Continue dataset after 10/2023 via CDC Wonder
parse_data <- function(df, jurisdiction_column, age_group) {
  df <- df |>
    mutate(
      year = as.numeric(left(`MMWR Week Code`, 4)),
      week = as.numeric(right(`MMWR Week Code`, 2))
    ) |>
    filter(!is.na(year), !is.na(week)) |>
    mutate(date = make_yearweek(year, week))
  if (nchar(jurisdiction_column) == 0) {
    df <- df |>
      select("date", "year", "week", "Deaths") |>
      setNames(c("date", "year", "week", "deaths"))
    df$iso3c <- "USA"
    df |>
      filter(!is.na(date)) |>
      select("iso3c", "date", "year", "week", "deaths") |>
      mutate(age_group = gsub("_", "-", age_group), .after = date)
  } else {
    df |>
      select(!!jurisdiction_column, "date", "year", "week", "Deaths") |>
      setNames(c("jurisdiction", "date", "year", "week", "deaths")) |>
      left_join(us_states_iso3c, by = "jurisdiction") |>
      filter(!is.na(iso3c), !is.na(date)) |>
      select("iso3c", "date", "year", "week", "deaths") |>
      mutate(age_group = gsub("_", "-", age_group), .after = date)
  }
}

data1 <- read_csv("../wonder_dl/out/usa_week_0_24_2022_2023.csv",
  col_types = "cccccc"
)
data2 <- read_csv("../wonder_dl/out/usa_week_25_44_2022_2023.csv",
  col_types = "cccccc"
)
data3 <- read_csv("../wonder_dl/out/usa_week_45_64_2022_2023.csv",
  col_types = "cccccc"
)
data4 <- read_csv("../wonder_dl/out/usa_week_65_74_2022_2023.csv",
  col_types = "cccccc"
)
data5 <- read_csv("../wonder_dl/out/usa_week_75_84_2022_2023.csv",
  col_types = "cccccc"
)
data6 <- read_csv("../wonder_dl/out/usa_week_85_100_2022_2023.csv",
  col_types = "cccccc"
)
data7 <- read_csv("../wonder_dl/out/usa_week_NS_2022_2023.csv",
  col_types = "cccccc"
)
data8 <- read_csv("../wonder_dl/out/usa_state_week_0_24_2022_2023.csv",
  col_types = "cccccc"
)
data9 <- read_csv("../wonder_dl/out/usa_state_week_25_44_2022_2023.csv",
  col_types = "cccccc"
)
data10 <- read_csv("../wonder_dl/out/usa_state_week_45_64_2022_2023.csv",
  col_types = "cccccc"
)
data11 <- read_csv("../wonder_dl/out/usa_state_week_65_74_2022_2023.csv",
  col_types = "cccccc"
)
data12 <- read_csv("../wonder_dl/out/usa_state_week_75_84_2022_2023.csv",
  col_types = "cccccc"
)
data13 <- read_csv("../wonder_dl/out/usa_state_week_85_100_2022_2023.csv",
  col_types = "cccccc"
)
data14 <- read_csv("../wonder_dl/out/usa_state_week_NS_2022_2023.csv",
  col_types = "cccccc"
)
data15 <- read_csv("../wonder_dl/out/usa_week_all_2022_2023.csv",
  col_types = "cccccc"
)
data16 <- read_csv("../wonder_dl/out/usa_state_week_all_2022_2023.csv",
  col_types = "cccccc"
)

result_2 <- rbind(
  parse_data(data1,
    jurisdiction_column = "",
    age_group = "0-24"
  ),
  parse_data(data2,
    jurisdiction_column = "",
    age_group = "25-44"
  ),
  parse_data(data3,
    jurisdiction_column = "",
    age_group = "45-64"
  ),
  parse_data(data4,
    jurisdiction_column = "",
    age_group = "65-74"
  ),
  parse_data(data5,
    jurisdiction_column = "",
    age_group = "75-84"
  ),
  parse_data(data6,
    jurisdiction_column = "",
    age_group = "85-100"
  ),
  parse_data(data7,
    jurisdiction_column = "",
    age_group = "NS"
  ),
  parse_data(
    df = data8,
    jurisdiction_column = "Residence State",
    age_group = "0-24"
  ),
  parse_data(data9,
    jurisdiction_column = "Residence State",
    age_group = "25-44"
  ),
  parse_data(data10,
    jurisdiction_column = "Residence State",
    age_group = "45-64"
  ),
  parse_data(data11,
    jurisdiction_column = "Residence State",
    age_group = "65-74"
  ),
  parse_data(data12,
    jurisdiction_column = "Residence State",
    age_group = "75-84"
  ),
  parse_data(data13,
    jurisdiction_column = "Residence State",
    age_group = "85-100"
  ),
  parse_data(data14,
    jurisdiction_column = "Residence State",
    age_group = "NS"
  ),
  parse_data(data15,
    jurisdiction_column = "",
    age_group = "all"
  ),
  parse_data(data16,
    jurisdiction_column = "Residence State",
    age_group = "all"
  )
) |>
  mutate(
    year = as.integer(year),
    week = as.integer(week),
    age_group = ifelse(age_group == "85-100", "85+", age_group),
    deaths = as.integer(ifelse(deaths == "Suppressed",
      NA, deaths
    ))
  ) |>
  arrange(iso3c, date, age_group) |>
  distinct(iso3c, date, age_group, .keep_all = TRUE)

# Join Result & Save
date_2017 <- make_yearweek(year = 2017, week = 1)
date_2023 <- make_yearweek(year = 2023, week = 1)

result_1_all <- result_1 |>
  filter(date < date_2017) |>
  group_by(across(c(-age_group, -deaths))) |>
  summarise(deaths = sum(deaths, na.rm = TRUE)) |>
  ungroup()
result_1_all$age_group <- "all"

result <- rbind(
  result_1 |> filter(date < date_2023),
  result_1_all,
  result_2 |> filter(date >= date_2023, week != 99)
) |>
  relocate(iso3c, date, year, week, age_group, deaths) |>
  arrange(iso3c, date, age_group) |>
  complete(iso3c, date, age_group) |>
  filter(!is.na(year), !is.na(week), !is.na(age_group))

save_csv(result, "deaths/usa/deaths_weekly")

# Data for USMortality.com
result_2 <- result |>
  inner_join(us_states_iso3c, by = "iso3c") |>
  mutate(year_week = paste0(year, "_", week)) |>
  select(jurisdiction, year, week, year_week, age_group, deaths) |>
  rename(state = jurisdiction)
result_2$deaths_covid <- NA
date <- now() %m-% weeks(2)
y <- year(date)
w <- week(date)
len <- nrow(result_2 |> filter(state == "United States", year == y, week == w))
if (len < 1) stop(paste("latest data for week", w, "missing"))
save_csv(result_2, paste0("deaths/usa/deaths_weekly_", y, "_", w))

# source("mortality/usa/deaths_weekly.r")
