source("lib/common.r")
source("lib/asmr.r")

options(progressr.enable = TRUE)

# Multithreading
n_cores <- detectCores()
if (n_cores > 2) {
  plan(multisession, workers = max(1, n_cores - 1))
} else {
  plan(sequential)
}

source("mortality/world_dataset_functions.r")

source("mortality/_collection/un.r")
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
  mortality_org,
  un
) |>
  arrange(iso3c, desc(type), source)

rm(
  deu_mortality_states,
  usa_mortality_states,
  eurostat,
  world_mortality,
  mortality_org,
  un
)

if (Sys.getenv("STAGE") != "") {
  data <- data |> filter(iso3c %in% c("USA", "SWE", "JPN", "DEU", "AFG"))
}
if (Sys.getenv("ISO3C") != "") {
  data <- data |> filter(iso3c == Sys.getenv("ISO3C"))
}

# Country names are saved in meta data.
source("mortality/world_iso.r")
save_info(
  df = data |> inner_join(iso3c_jurisdiction, by = c("iso3c")),
  upload = FALSE
)
rm(iso3c_jurisdiction)

process_country <- function(df) {
  iso3c <- df[1, ]$iso3c
  print(paste0("ISO: ", iso3c))
  # Make furr pull in these functions
  fluseason <- fluseason
  midyear <- midyear
  dd <- df |>
    mutate(cmr = deaths / population * 100000) |>
    expand_daily()
  dd_all <- dd |>
    filter(age_group == "all") |>
    arrange(date, desc(type)) |>
    distinct(iso3c, date, .keep_all = TRUE)
  dd_age <- dd |>
    filter(age_group != "all") |>
    arrange(date, desc(type), age_group)
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
      write_dataset(
        iso3c, ag,
        weekly = summarize_data_all(dd_all, dd_asmr, type = "yearweek"),
        monthly = summarize_data_all(dd_all, dd_asmr, type = "yearmonth"),
        quarterly = summarize_data_all(dd_all, dd_asmr, type = "yearquarter"),
        yearly = summarize_data_all(dd_all, dd_asmr, type = "year"),
        by_fluseason <- summarize_data_all(dd_all, dd_asmr, type = "fluseason"),
        by_midyear = summarize_data_all(dd_all, dd_asmr, type = "midyear")
      )
    } else {
      dd_ag_f <- dd_age |>
        filter(age_group == ag) |>
        distinct(iso3c, date, age_group, .keep_all = TRUE)
      write_dataset(
        iso3c, ag,
        weekly = summarize_data_by_time(dd_ag_f, type = "yearweek"),
        monthly = summarize_data_by_time(dd_ag_f, type = "yearmonth"),
        quarterly = summarize_data_by_time(dd_ag_f, type = "yearquarter"),
        yearly = summarize_data_by_time(dd_ag_f, type = "year"),
        by_fluseason = summarize_data_by_time(dd_ag_f, type = "fluseason"),
        by_midyear = summarize_data_by_time(dd_ag_f, type = "midyear")
      )
    }
  }
}

if (Sys.getenv("CI") != 1) {
  countries <- unique(data$iso3c)
  with_progress({
    p <- progressor(steps = length(countries))
    data |>
      group_split(iso3c) |>
      future_walk(~ {
        process_country(.x)
        p()
      })
  })
} else {
  df <- data |>
    group_split(iso3c) |>
    walk(process_country)
}

print("Finished.")

# source("mortality/world_dataset.r")
