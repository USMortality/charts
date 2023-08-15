source("lib/common.r")
source("lib/asmr.r")
source("mortality/world_dataset_functions.r")

source("mortality/_collection/mortality_org.r")
source("mortality/_collection/world_mortality.r")
source("mortality/usa/mortality_states.r")
source("mortality/deu/mortality_states.r")

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
make_yearmonth <- tsibble::make_yearmonth
arrange <- dplyr::arrange
distinct <- dplyr::distinct
complete <- tidyr::complete
case_when <- dplyr::case_when

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

save_info(dd |> inner_join(iso3c_jurisdiction, by = c("iso3c")))

dd_all <- dd |>
  filter(age_group == "all") |>
  mutate(iso = iso3c)

dd_age <- dd |>
  filter(age_group != "all") |>
  mutate(iso = iso3c)
dd_asmr <- dd_age |>
  group_by(iso3c, type) |>
  group_modify(~ calculate_asmr_variants(.x), .keep = TRUE) |>
  ungroup() |>
  mutate(iso = iso3c)

save_dataset <- function(
    weekly_nested,
    monthly_nested,
    quarterly_nested,
    yearly_nested,
    fluseason_nested,
    midyear_nested,
    ag) {
  ag <- "all"
  postfix <- ifelse(ag == "all", "", paste0("_", ag))

  print('Calculating "Weekly" dataset')
  weekly <- weekly_nested |>
    filter(age_group == ag) |>
    mutate(data = lapply(data, calculate_baseline_excess, "weekly")) |>
    unnest(cols = c(data)) |>
    select(-iso)
  save_csv(
    weekly,
    paste0("mortality/world_weekly", postfix),
    upload = TRUE
  )

  print('Calculating "Weekly 104W SMA" dataset')
  weekly104wsma <- weekly_nested |>
    filter(age_group == ag) |>
    filter_n_rows(104) |>
    mutate(data = lapply(data, calc_sma, 104)) |>
    mutate(data = lapply(data, calculate_baseline_excess, "weekly_104w_sma")) |>
    unnest(cols = c(data)) |>
    select(-iso) |>
    group_by(.data$iso3c) |>
    filter(!is.na(.data$deaths)) |>
    ungroup()
  save_csv(
    weekly104wsma,
    paste0("mortality/world_weekly_104w_sma", postfix),
    upload = TRUE
  )

  print('Calculating "Weekly 52W SMA" dataset')
  weekly52wsma <- weekly_nested |>
    filter(age_group == ag) |>
    filter_n_rows(52) |>
    mutate(data = lapply(data, calc_sma, 52)) |>
    mutate(data = lapply(data, calculate_baseline_excess, "weekly_52w_sma")) |>
    unnest(cols = c(data)) |>
    select(-"iso") |>
    group_by(.data$iso3c) |>
    filter(!is.na(.data$deaths)) |>
    ungroup()
  save_csv(
    weekly52wsma,
    paste0("mortality/world_weekly_52w_sma", postfix),
    upload = TRUE
  )

  print('Calculating "Weekly 26W SMA" dataset')
  weekly26wsma <- weekly_nested |>
    filter(age_group == ag) |>
    mutate(data = lapply(data, calc_sma, 26)) |>
    filter_n_rows(26) |>
    mutate(data = lapply(data, calculate_baseline_excess, "weekly_26w_sma")) |>
    unnest(cols = c(data)) |>
    select(-"iso") |>
    group_by(.data$iso3c) |>
    filter(!is.na(deaths)) |>
    ungroup()
  save_csv(
    weekly26wsma,
    paste0("mortality/world_weekly_26w_sma", postfix),
    upload = TRUE
  )

  print('Calculating "Weekly 13W SMA" dataset')
  weekly13wsma <- weekly_nested |>
    filter(age_group == ag) |>
    mutate(data = lapply(data, calc_sma, 13)) |>
    filter_n_rows(13) |>
    mutate(data = lapply(data, calculate_baseline_excess, "weekly_13w_sma")) |>
    unnest(cols = c(data)) |>
    select(-"iso") |>
    group_by(.data$iso3c) |>
    filter(!is.na(deaths)) |>
    ungroup()
  save_csv(
    weekly13wsma,
    paste0("mortality/world_weekly_13w_sma", postfix),
    upload = TRUE
  )

  print('Calculating "Monthly" dataset')
  monthly <- monthly_nested |>
    filter(age_group == ag) |>
    mutate(data = lapply(data, calculate_baseline_excess, "monthly")) |>
    unnest(cols = c(data)) |>
    select(-any_of("iso"))
  save_csv(
    monthly,
    paste0("mortality/world_monthly", postfix),
    upload = TRUE
  )

  print('Calculating "Quarterly" dataset')
  quarterly <- quarterly_nested |>
    filter(age_group == ag) |>
    mutate(data = lapply(data, calculate_baseline_excess, "quarterly")) |>
    unnest(cols = c(data)) |>
    select(-any_of("iso"))
  save_csv(
    quarterly,
    paste0("mortality/world_quarterly", postfix),
    upload = TRUE
  )

  print('Calculating "Yearly" dataset')
  yearly <- yearly_nested |>
    filter(age_group == ag) |>
    mutate(data = lapply(data, calculate_baseline_excess, "yearly")) |>
    unnest(cols = c(data)) |>
    select(-any_of("iso"))
  save_csv(
    yearly,
    paste0("mortality/world_yearly", postfix),
    upload = TRUE
  )

  print('Calculating "Fluseason" dataset')
  fluseason <- fluseason_nested |>
    filter(age_group == ag) |>
    mutate(data = lapply(data, calculate_baseline_excess, "fluseason")) |>
    unnest(cols = c(data)) |>
    select(-any_of("iso"))
  save_csv(
    fluseason,
    paste0("mortality/world_fluseason", postfix),
    upload = TRUE
  )

  print('Calculating "Midyear" dataset')
  midyear <- midyear_nested |>
    filter(age_group == ag) |>
    mutate(data = lapply(data, calculate_baseline_excess, "midyear")) |>
    unnest(cols = c(data)) |>
    select(-any_of("iso"))
  save_csv(
    midyear,
    paste0("mortality/world_midyear", postfix),
    upload = TRUE
  )
}

# Weekly
weekly_nested_age <- get_nested_data_by_time_age(dd_age, fun_name = "yearweek")
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
    fluseason_nested,
    midyear_nested,
    ag
  )
}

print("Finished.")
