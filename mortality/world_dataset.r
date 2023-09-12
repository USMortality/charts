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
any_of <- dplyr::any_of

source("lib/common.r")
source("lib/asmr.r")
source("mortality/world_dataset_functions.r")

source("mortality/_collection/mortality_org.r")
source("mortality/_collection/world_mortality.r")
source("mortality/_collection/eurostat.r")
source("mortality/usa/mortality_states.r")
source("mortality/deu/mortality_states.r")

# Load Data
baseline_size <- read_remote("mortality/world_baseline.csv")
asmr_types <- c("asmr_who", "asmr_esp", "asmr_usa", "asmr_country")

data <- rbind(
  deu_mortality_states,
  usa_mortality_states,
  eurostat |> filter(iso3c != "SWE"),
  world_mortality,
  mortality_org
) |> arrange(iso3c, type, source)

rm(
  deu_mortality_states,
  usa_mortality_states,
  eurostat,
  world_mortality,
  mortality_org
)

# Country names are saved in meta data.
source("mortality/world_iso.r")
save_info(df = data |> inner_join(iso3c_jurisdiction, by = c("iso3c")))
rm(iso3c_jurisdiction)

# start with ALB, since it has ASMR for csv headers.
for (code in unique(c("ALB", data$iso3c))) {
  print(paste0("Country: ", code))

  dd <- data |>
    filter(iso3c == code) |>
    mutate(cmr = deaths / population * 100000) |>
    expand_daily()
  dd_all <- dd |>
    filter(age_group == "all") |>
    filter(row_number() == n(), .by = c(iso3c, date)) |>
    arrange(date, type)
  dd_age <- dd |> filter(age_group != "all")
  dd_asmr <- dd_age
  if (nrow(dd_age)) {
    dd_asmr <- dd_age |>
      group_by(iso3c, type, n_age_groups, source) |>
      group_modify(~ calculate_asmr_variants(.x), .keep = TRUE) |>
      ungroup() |>
      filter(type == max(type), .by = c(iso3c, date)) |>
      filter(n_age_groups == max(n_age_groups), .by = c(iso3c, date)) |>
      distinct(iso3c, date, .keep_all = TRUE) |>
      arrange(date, type)
    dd_asmr$age_group <- "all"
  }

  for (ag in unique(dd$age_group)) {
    print(paste0("Age Group: ", ag))
    if (ag == "all") {
      append_dataset(
        weekly = summarize_data_all(dd_all, dd_asmr, "yearweek"),
        monthly = summarize_data_all(dd_all, dd_asmr, "yearmonth"),
        quarterly = summarize_data_all(dd_all, dd_asmr, "yearquarter"),
        yearly = summarize_data_all(dd_all, dd_asmr, "year"),
        by_fluseason = summarize_data_all(dd_all, dd_asmr, "fluseason"),
        by_midyear = summarize_data_all(dd_all, dd_asmr, "midyear"),
        ag
      )
    } else {
      dd_ag_f <- dd_age |> filter(age_group == ag)
      append_dataset(
        weekly = summarize_data_by_time(dd_ag_f, "yearweek"),
        monthly = summarize_data_by_time(dd_ag_f, "yearmonth"),
        quarterly = summarize_data_by_time(dd_ag_f, "yearquarter"),
        yearly = summarize_data_by_time(dd_ag_f, "year"),
        by_fluseason = summarize_data_by_time(dd_ag_f, "fluseason"),
        by_midyear = summarize_data_by_time(dd_ag_f, "midyear"),
        ag
      )
    }
  }
}

print("Finished.")
