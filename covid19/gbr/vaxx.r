source("lib/common.r")

data <- read.csv(
  "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=vaccinationsAgeDemographics&format=csv"
) |>
  as_tibble() |>
  select(4, 5, cumPeopleVaccinatedFirstDoseByVaccinationDate, cumVaccinationFirstDoseUptakeByVaccinationDatePercentage) |>
  set_names(c("date", "age_group", "vaxxed", "vaxxed_pct"))

population <- data |>
  filter(date == max(data$date)) |>
  mutate(
    age_group = str_replace(age_group, "_", "-"),
    vaxxed_pct = vaxxed_pct / 100
  ) |>
  mutate(
    population = vaxxed / vaxxed_pct,
    # Translate years
    age_group = case_when(
      age_group %in% c("05-11", "12-15", "16-17") ~ "0-17",
      age_group %in% c("18-24", "25-29", "30-34", "35-39") ~ "18-39",
      age_group %in% c("40-44", "45-49") ~ "40-49",
      age_group %in% c("50-54", "55-59") ~ "50-59",
      age_group %in% c("60-64", "65-69") ~ "60-69",
      age_group %in% c("70-74", "75-79") ~ "70-79",
      age_group %in% c("80-84", "85-89") ~ "80-89",
      age_group %in% c("90+") ~ "90+"
    )
  ) |>
  mutate(
    population = vaxxed / vaxxed_pct,
  ) |>
  filter(!is.na(age_group)) |>
  group_by(age_group) |>
  summarise(population = as.integer(sum(population)))

vaxxed <- data |>
  mutate(
    date = ymd(date),
    age_group = str_replace(age_group, "_", "-")
  ) |>
  mutate(
    # Translate years
    age_group = case_when(
      age_group %in% c("05-11", "12-15", "16-17") ~ "0-17",
      age_group %in% c("18-24", "25-29", "30-34", "35-39") ~ "18-39",
      age_group %in% c("40-44", "45-49") ~ "40-49",
      age_group %in% c("50-54", "55-59") ~ "50-59",
      age_group %in% c("60-64", "65-69") ~ "60-69",
      age_group %in% c("70-74", "75-79") ~ "70-79",
      age_group %in% c("80-84", "85-89") ~ "80-89",
      age_group %in% c("90+") ~ "90+"
    )
  ) |>
  filter(!is.na(age_group)) |>
  group_by(date, age_group) |>
  summarise(vaxxed = sum(vaxxed)) |>
  ungroup() |>
  mutate(date = yearmonth(date)) |>
  group_by(date, age_group) |>
  summarise(
    vaxxed_lower = round(min(vaxxed)),
    vaxxed_upper = round(max(vaxxed)),
    vaxxed = round(mean(vaxxed))
  ) |>
  relocate(vaxxed, .before = "vaxxed_lower")

vaxxed_population <- vaxxed |>
  inner_join(population, by = "age_group") |>
  dplyr::arrange(date, age_group) |>
  group_by(date) |>
  nest() |>
  mutate(data = map(data, ~ .x |> adorn_totals("row", name = "all"))) |>
  unnest(cols = c(data)) |>
  mutate(
    vaxxed_pct = round(vaxxed / population, 3),
    vaxxed_pct_lower = round(vaxxed_lower / population, 3),
    vaxxed_pct_upper = round(vaxxed_upper / population, 3)
  )
