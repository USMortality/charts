source("lib/common.r")
source("lib/asmr.r")
source("mortality/world_dataset_functions.r")

source("mortality/_collection/mortality_org.r")
source("mortality/_collection/world_mortality.r")
source("mortality/usa/mortality_states.r")
source("mortality/deu/mortality_states.r")

# Country names are added back in the end.
source("mortality/world_iso.r")

# Load Data
baseline_size <- read_remote("mortality/world_baseline.csv")

asmr_types <- c("asmr_who", "asmr_esp", "asmr_usa", "asmr_country")

# For duplicates: first values take precedence.
dd <- rbind(
  deu_mortality_states,
  usa_mortality_states,
  world_mortality |> filter(!iso3c %in% c("DEU", "USA")),
  mortality_org |> filter(!iso3c %in% c("DEU", "USA"))
) |>
  distinct(iso3c, date, age_group, type, .keep_all = TRUE) |>
  arrange(iso3c, date, age_group, type) |>
  mutate(cmr = deaths / population * 100000) |>
  group_by(iso3c, age_group) |>
  group_modify(~ fill_gaps_na(.x)) |>
  ungroup()

save_info(dd)

dd_all <- dd |>
  filter(age_group == "all") |>
  inner_join(iso3c_jurisdiction, by = c("iso3c")) |>
  mutate(iso = iso3c)

dd_asmr <- dd |>
  filter(age_group != "all") |>
  group_by(iso3c, type) |>
  group_modify(~ calculate_asmr_variants(.x), .keep = TRUE) |>
  ungroup() |>
  inner_join(iso3c_jurisdiction, by = c("iso3c")) |>
  mutate(iso = iso3c)

# Weekly data
weekly_nested <- get_nested_data_by_time(dd_asmr, dd_all, "yearweek")

print('Calculating "Weekly" dataset')
weekly <- weekly_nested |>
  mutate(data = lapply(data, calculate_baseline_excess, "weekly")) |>
  unnest(cols = c(data)) |>
  select(-iso)
save_csv(weekly, "mortality/world_weekly", upload = TRUE)

print('Calculating "Weekly 104W SMA" dataset')
weekly104wsma <- weekly_nested |>
  filter_n_rows(104) |>
  mutate(data = lapply(data, calc_sma, 104)) |>
  mutate(data = lapply(data, calculate_baseline_excess, "weekly_104w_sma")) |>
  unnest(cols = c(data)) |>
  select(-iso) |>
  group_by(iso3c) |>
  filter(!is.na(deaths)) |>
  ungroup()
save_csv(weekly104wsma, "mortality/world_weekly_104w_sma", upload = TRUE)

print('Calculating "Weekly 52W SMA" dataset')
weekly52wsma <- weekly_nested |>
  filter_n_rows(52) |>
  mutate(data = lapply(data, calc_sma, 52)) |>
  mutate(data = lapply(data, calculate_baseline_excess, "weekly_52w_sma")) |>
  unnest(cols = c(data)) |>
  select(-iso) |>
  group_by(iso3c) |>
  filter(!is.na(deaths)) |>
  ungroup()
save_csv(weekly52wsma, "mortality/world_weekly_52w_sma", upload = TRUE)

print('Calculating "Weekly 26W SMA" dataset')
weekly26wsma <- weekly_nested |>
  mutate(data = lapply(data, calc_sma, 26)) |>
  filter_n_rows(26) |>
  mutate(data = lapply(data, calculate_baseline_excess, "weekly_26w_sma")) |>
  unnest(cols = c(data)) |>
  select(-iso) |>
  group_by(iso3c) |>
  filter(!is.na(deaths)) |>
  ungroup()
save_csv(weekly26wsma, "mortality/world_weekly_26w_sma", upload = TRUE)

print('Calculating "Weekly 13W SMA" dataset')
weekly13wsma <- weekly_nested |>
  mutate(data = lapply(data, calc_sma, 13)) |>
  filter_n_rows(13) |>
  mutate(data = lapply(data, calculate_baseline_excess, "weekly_13w_sma")) |>
  unnest(cols = c(data)) |>
  select(-iso) |>
  group_by(iso3c) |>
  filter(!is.na(deaths)) |>
  ungroup()
save_csv(weekly13wsma, "mortality/world_weekly_13w_sma", upload = TRUE)

print('Calculating "Monthly" dataset')
monthly <- get_nested_data_by_time(dd_asmr, dd_all, "yearmonth") |>
  mutate(data = lapply(data, calculate_baseline_excess, "monthly")) |>
  unnest(cols = c(data)) |>
  select(-iso)
save_csv(monthly, "mortality/world_monthly", upload = TRUE)

print('Calculating "Quarterly" dataset')
quarterly <- get_nested_data_by_time(dd_asmr, dd_all, "yearquarter") |>
  mutate(data = lapply(data, calculate_baseline_excess, "quarterly")) |>
  unnest(cols = c(data)) |>
  select(-iso)
save_csv(quarterly, "mortality/world_quarterly", upload = TRUE)

print('Calculating "Yearly" dataset')
yearly <- get_nested_data_by_time(dd_asmr, dd_all, "year") |>
  mutate(data = lapply(data, calculate_baseline_excess, "yearly")) |>
  unnest(cols = c(data)) |>
  select(-iso)
save_csv(yearly, "mortality/world_yearly", upload = TRUE)

print('Calculating "YTD" dataset')
daily_nested_ytd <- dd_all |>
  left_join(dd_asmr, by = c("iso3c", "date", "type")) |>
  select(-iso.y, -jurisdiction.y, -age_group) |>
  setNames(c(
    "iso3c", "date", "deaths", "population", "type", "source", "cmr",
    "jurisdiction", "iso", asmr_types
  )) |>
  mutate(year = year(date)) |>
  arrange(iso3c, date) |>
  distinct(iso3c, date, .keep_all = TRUE) |>
  nest(data = !iso) |>
  mutate(data = lapply(data, calc_ytd))
ytd <- daily_nested_ytd |>
  mutate(data = lapply(data, aggregate_data_ytd)) |>
  mutate(data = lapply(data, calculate_baseline_excess, "yearly")) |>
  unnest(cols = c(data)) |>
  select(-iso)
save_csv(ytd, "mortality/world_ytd", upload = TRUE)

print('Calculating "Fluseason" dataset')
fluseason <- get_nested_data_by_time(dd_asmr, dd_all, "fluseason") |>
  mutate(data = lapply(data, calculate_baseline_excess, "fluseason")) |>
  unnest(cols = c(data)) |>
  select(-iso)
save_csv(fluseason, "mortality/world_fluseason", upload = TRUE)

print('Calculating "Midyear" dataset')
midyear <- get_nested_data_by_time(dd_asmr, dd_all, "midyear") |>
  mutate(data = lapply(data, calculate_baseline_excess, "midyear")) |>
  unnest(cols = c(data)) |>
  select(-iso)
save_csv(midyear, "mortality/world_midyear", upload = TRUE)

print("Finished.")
