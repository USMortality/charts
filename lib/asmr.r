source("lib/common.r")
source("population/std_pop.r")

calculate_asmr <- function(df, pop, col_name) {
  col <- rlang::sym(col_name)
  df |>
    dplyr::inner_join(pop, by = "age_group") |>
    dplyr::mutate(!!col_name := cmr * weight) |>
    dplyr::group_by(iso3c, date) |>
    dplyr::summarise(!!col_name := sum(!!col)) |>
    dplyr::ungroup()
}

calculate_asmr_variants <- function(df) {
  age_bins <- unique(df$age_group)
  std_pop_who <- get_who2015_bins(age_bins)
  std_pop_esp <- get_esp2013_bins(age_bins)
  std_pop_usa <- get_usa2000_bins(age_bins)
  std_pop_country <- get_country2020_bins(df)

  dd_asmr1 <- df |> calculate_asmr(std_pop_who, "asmr_who")
  dd_asmr2 <- df |> calculate_asmr(std_pop_esp, "asmr_esp")
  dd_asmr3 <- df |> calculate_asmr(std_pop_usa, "asmr_usa")
  dd_asmr4 <- df |> calculate_asmr(std_pop_country, "asmr_country")
  dd_asmr1 |>
    dplyr::inner_join(dd_asmr2, by = c("iso3c", "date")) |>
    dplyr::inner_join(dd_asmr3, by = c("iso3c", "date")) |>
    dplyr::inner_join(dd_asmr4, by = c("iso3c", "date"))
}
