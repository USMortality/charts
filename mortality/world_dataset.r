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
  group_by(iso3c, age_group, type) |>
  group_modify(~ fill_gaps_na(.x)) |>
  ungroup()

# save_info(dd)

dd_all <- dd |>
  filter(age_group == "all") |>
  inner_join(iso3c_jurisdiction, by = c("iso3c")) |>
  mutate(iso = iso3c)

dd_age <- dd |>
  filter(age_group != "all") |>
  inner_join(iso3c_jurisdiction, by = c("iso3c")) |>
  mutate(iso = iso3c)
dd_asmr <- dd_age |>
  group_by(iso3c, type) |>
  group_modify(~ calculate_asmr_variants(.x), .keep = TRUE) |>
  ungroup() |>
  inner_join(iso3c_jurisdiction, by = c("iso3c")) |>
  mutate(iso = iso3c)

save_dataset <- function(
    weekly_nested,
    monthly_nested,
    quarterly_nested,
    yearly_nested,
    ytd_nested,
    fluseason_nested,
    midyear_nested,
    age_group) {
  postfix <- ifelse(age_group == "all", "", paste0("_", age_group))

  print('Calculating "Weekly" dataset')
  weekly <- weekly_nested |>
    filter(age_group == ag) |>
    mutate(data = lapply(data, calculate_baseline_excess, "weekly")) |>
    unnest(cols = c(data)) |>
    select(-iso)
  save_csv(
    weekly,
    paste0("mortality/world_weekly", postfix),
    upload = FALSE
  )

  print('Calculating "Weekly 104W SMA" dataset')
  weekly104wsma <- weekly_nested |>
    filter(age_group == ag) |>
    filter_n_rows(104) |>
    mutate(data = lapply(data, calc_sma, 104)) |>
    mutate(data = lapply(data, calculate_baseline_excess, "weekly_104w_sma")) |>
    unnest(cols = c(data)) |>
    select(-iso) |>
    group_by(iso3c) |>
    filter(!is.na(deaths)) |>
    ungroup()
  save_csv(
    weekly104wsma,
    paste0("mortality/world_weekly_104w_sma", postfix),
    upload = FALSE
  )

  print('Calculating "Weekly 52W SMA" dataset')
  weekly52wsma <- weekly_nested |>
    filter(age_group == ag) |>
    filter_n_rows(52) |>
    mutate(data = lapply(data, calc_sma, 52)) |>
    mutate(data = lapply(data, calculate_baseline_excess, "weekly_52w_sma")) |>
    unnest(cols = c(data)) |>
    select(-iso) |>
    group_by(iso3c) |>
    filter(!is.na(deaths)) |>
    ungroup()
  save_csv(
    weekly52wsma,
    paste0("mortality/world_weekly_52w_sma", postfix),
    upload = FALSE
  )

  print('Calculating "Weekly 26W SMA" dataset')
  weekly26wsma <- weekly_nested |>
    filter(age_group == ag) |>
    mutate(data = lapply(data, calc_sma, 26)) |>
    filter_n_rows(26) |>
    mutate(data = lapply(data, calculate_baseline_excess, "weekly_26w_sma")) |>
    unnest(cols = c(data)) |>
    select(-iso) |>
    group_by(iso3c) |>
    filter(!is.na(deaths)) |>
    ungroup()
  save_csv(
    weekly26wsma,
    paste0("mortality/world_weekly_26w_sma", postfix),
    upload = FALSE
  )

  print('Calculating "Weekly 13W SMA" dataset')
  weekly13wsma <- weekly_nested |>
    filter(age_group == ag) |>
    mutate(data = lapply(data, calc_sma, 13)) |>
    filter_n_rows(13) |>
    mutate(data = lapply(data, calculate_baseline_excess, "weekly_13w_sma")) |>
    unnest(cols = c(data)) |>
    select(-iso) |>
    group_by(iso3c) |>
    filter(!is.na(deaths)) |>
    ungroup()
  save_csv(
    weekly13wsma,
    paste0("mortality/world_weekly_13w_sma", postfix),
    upload = FALSE
  )

  print('Calculating "Monthly" dataset')
  monthly <- monthly_nested |>
    filter(age_group == ag) |>
    mutate(data = lapply(data, calculate_baseline_excess, "monthly")) |>
    unnest(cols = c(data)) |>
    select(-iso)
  save_csv(
    monthly,
    paste0("mortality/world_monthly", postfix),
    upload = FALSE
  )

  print('Calculating "Quarterly" dataset')
  quarterly <- quarterly_nested |>
    filter(age_group == ag) |>
    mutate(data = lapply(data, calculate_baseline_excess, "quarterly")) |>
    unnest(cols = c(data)) |>
    select(-iso)
  save_csv(
    quarterly,
    paste0("mortality/world_quarterly", postfix),
    upload = FALSE
  )

  print('Calculating "Yearly" dataset')
  yearly <- yearly_nested |>
    filter(age_group == ag) |>
    mutate(data = lapply(data, calculate_baseline_excess, "yearly")) |>
    unnest(cols = c(data)) |>
    select(-iso)
  save_csv(
    yearly,
    paste0("mortality/world_yearly", postfix),
    upload = FALSE
  )

  print('Calculating "YTD" dataset')
  ytd <- ytd_nested |>
    filter(age_group == ag) |>
    mutate(data = lapply(data, calculate_baseline_excess, "ytd")) |>
    unnest(cols = c(data)) |>
    select(-iso)
  save_csv(
    ytd,
    paste0("mortality/world_ytd", postfix),
    upload = FALSE
  )

  print('Calculating "YTD" dataset')
  ytd <- ytd_nested |>
    filter(age_group == ag) |>
    mutate(data = lapply(data, calculate_baseline_excess, "yearly")) |>
    unnest(cols = c(data)) |>
    select(-iso)
  save_csv(
    ytd,
    paste0("mortality/world_ytd", postfix),
    upload = FALSE
  )

  print('Calculating "Fluseason" dataset')
  fluseason <- fluseason_nested |>
    filter(age_group == ag) |>
    mutate(data = lapply(data, calculate_baseline_excess, "fluseason")) |>
    unnest(cols = c(data)) |>
    select(-iso)
  save_csv(
    fluseason,
    paste0("mortality/world_fluseason", postfix),
    upload = FALSE
  )

  print('Calculating "Midyear" dataset')
  midyear <- midyear_nested |>
    filter(age_group == ag) |>
    mutate(data = lapply(data, calculate_baseline_excess, "midyear")) |>
    unnest(cols = c(data)) |>
    select(-iso)
  save_csv(
    midyear,
    paste0("mortality/world_midyear", postfix),
    upload = FALSE
  )
}

# Weekly
weekly_nested_age <- get_nested_data_by_time_age(dd_age, "yearweek")
weekly_nested <- get_nested_data_by_time(dd_asmr, dd_all, "yearweek")
weekly_nested$age_group <- "all"
weekly_nested <- rbind(weekly_nested_age, weekly_nested)

# Monthly
monthly_nested_age <- get_nested_data_by_time_age(dd_age, "yearmonth")
monthly_nested <- get_nested_data_by_time(dd_asmr, dd_all, "yearmonth")
monthly_nested$age_group <- "all"
monthly_nested <- rbind(monthly_nested_age, monthly_nested)

# Quarterly
quarterly_nested_age <- get_nested_data_by_time_age(dd_age, "yearquarter")
quarterly_nested <- get_nested_data_by_time(dd_asmr, dd_all, "yearquarter")
quarterly_nested$age_group <- "all"
quarterly_nested <- rbind(quarterly_nested_age, quarterly_nested)

# Yearly
yearly_nested_age <- get_nested_data_by_time_age(dd_age, "year")
yearly_nested <- get_nested_data_by_time(dd_asmr, dd_all, "year")
yearly_nested$age_group <- "all"
yearly_nested <- rbind(yearly_nested_age, yearly_nested)

# YTD Nested
ytd_nested_age <- dd_age |>
  mutate(year = year(date)) |>
  arrange(iso3c, date) |>
  distinct(iso3c, date, age_group, .keep_all = TRUE) |>
  nest(data = !c(iso, age_group)) |>
  mutate(data = lapply(data, calc_ytd)) |>
  mutate(data = lapply(data, aggregate_data_ytd_age))

ytd_nested <- dd_all |>
  left_join(dd_asmr, by = c("iso3c", "date", "type")) |>
  select(-iso.y, -jurisdiction.y, -age_group) |>
  setNames(c(
    "iso3c", "type", "date", "deaths", "population", "source", "cmr",
    "jurisdiction", "iso", asmr_types
  )) |>
  mutate(year = year(date)) |>
  arrange(iso3c, date) |>
  distinct(iso3c, date, .keep_all = TRUE) |>
  nest(data = !iso) |>
  mutate(data = lapply(data, calc_ytd)) |>
  mutate(data = lapply(data, aggregate_data_ytd))
ytd_nested$age_group <- "all"
ytd_nested <- rbind(ytd_nested_age, ytd_nested)

# Fluseason
fluseason_nested_age <- get_nested_data_by_time_age(dd_age, "fluseason")
fluseason_nested <- get_nested_data_by_time(dd_asmr, dd_all, "fluseason")
fluseason_nested$age_group <- "all"
fluseason_nested <- rbind(fluseason_nested_age, fluseason_nested)

# Midyear
midyear_nested_age <- get_nested_data_by_time_age(dd_age, "midyear")
midyear_nested <- get_nested_data_by_time(dd_asmr, dd_all, "midyear")
midyear_nested$age_group <- "all"
midyear_nested <- rbind(midyear_nested_age, midyear_nested)

for (ag in unique(weekly_nested$age_group)) {
  print(paste0("Age Group: ", ag))
  save_dataset(
    weekly_nested,
    monthly_nested,
    quarterly_nested,
    yearly_nested,
    ytd_nested,
    fluseason_nested,
    midyear_nested,
    ag
  )
}

print("Finished.")
