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
across <- dplyr::across
left_join <- dplyr::left_join
group_modify <- dplyr::group_modify

filter_by_complete_temp_values <- function(data, fun_name, n) {
  start <- data |>
    filter(.data[[fun_name]] == head(data[[fun_name]], n = 1)) |>
    group_by(across(all_of(fun_name))) |>
    filter(n() >= n) |>
    ungroup()
  mid <- data |>
    filter(!.data[[fun_name]] %in% c(
      head(data, n = 1)[[fun_name]],
      tail(data, n = 1)[[fun_name]]
    ))
  end <- data |>
    filter(.data[[fun_name]] == tail(data, n = 1)[[fun_name]]) |>
    group_by(across(all_of(fun_name))) |>
    filter(n() >= n) |>
    ungroup()

  rbind(start, mid, end) |>
    group_by(across(all_of(c("iso3c", fun_name))))
}

aggregate_data <- function(data, type) {
  # Filter ends
  result <- switch(type,
    "yearweek" = filter_by_complete_temp_values(data, type, 7),
    "yearmonth" = filter_by_complete_temp_values(data, type, 28),
    "yearquarter" = filter_by_complete_temp_values(data, type, 90),
    "year" = filter_by_complete_temp_values(data, type, 365),
    "fluseason" = filter_by_complete_temp_values(data, type, 365),
    "midyear" = filter_by_complete_temp_values(data, type, 365)
  )
  if ("cmr" %in% names(data)) {
    result <- result |>
      summarise(
        deaths = round(sum_if_not_empty(deaths)),
        population = round(mean(.data$population)),
        cmr = round(sum_if_not_empty(.data$cmr), digits = 1)
      )
  }
  if ("asmr_who" %in% names(data)) {
    result <- result |>
      summarise(
        asmr_who = round(sum_if_not_empty(.data$asmr_who), digits = 1),
        asmr_esp = round(sum_if_not_empty(.data$asmr_esp), digits = 1),
        asmr_usa = round(sum_if_not_empty(.data$asmr_usa), digits = 1),
        asmr_country = round(sum_if_not_empty(.data$asmr_country), digits = 1)
      )
  }
  result |>
    ungroup() |>
    dplyr::rename("date" = all_of(type)) |>
    select(-"iso3c")
}

sma <- function(vec, n) {
  sma <- c()
  sma[1:(n - 1)] <- NA
  for (i in n:length(vec)) {
    sma[i] <- mean(vec[(i - n + 1):i], na.rm = TRUE)
  }
  xts::reclass(sma, vec)
}

calc_sma <- function(data, n) {
  if (nrow(data) < n) {
    return(data[c(), ])
  }

  data$deaths <- round(sma(data$deaths, n = n), 3)
  data$cmr <- round(sma(data$cmr, n = n), 3)
  if ("asmr_who" %in% colnames(data)) {
    data$asmr_who <- round(sma(data$asmr_who, n = n), 3)
    data$asmr_esp <- round(sma(data$asmr_esp, n = n), 3)
    data$asmr_usa <- round(sma(data$asmr_usa, n = n), 3)
    data$asmr_country <- round(sma(data$asmr_country, n = n), 3)
  }
  data
}

calculate_excess <- function(data, col_name) {
  col <- sym(col_name)
  data |> mutate(
    "{col_name}_excess" := !!col - !!sym(paste0(col_name, "_baseline")),
    "{col_name}_excess_lower" :=
      !!col - !!sym(paste0(col_name, "_baseline_lower")),
    "{col_name}_excess_upper" :=
      !!col - !!sym(paste0(col_name, "_baseline_upper")),
    .after = all_of(paste0(col_name, "_baseline_upper"))
  )
}

get_period_multiplier <- function(chart_type) {
  if (chart_type %in% c("yearly", "fluseason", "midyear")) {
    return(1)
  } else if (chart_type == "quarterly") {
    return(4)
  } else if (chart_type == "monthly") {
    return(12)
  } else if (chart_type == "weekly") {
    return(52.143)
  } else {
    return(52) # SMA
  }
}

apply_model <- function(data, col, chart_type) {
  if (chart_type %in% c(
    "weekly_26w_sma", "weekly_13w_sma", "quarterly", "monthly", "weekly"
  )) {
    data |> model(fable::TSLM(!!col ~ trend() + season()))
  } else {
    data |> model(fable::TSLM(!!col ~ trend()))
  }
}

get_baseline_length <- function(iso, ct, cn) {
  if (!ct %in% c("fluseason", "midyear")) ct <- "yearly"
  baseline <- baseline_size |>
    filter(.data$iso3c == iso & .data$chart_type == ct & .data$type == cn)
  # Use at least five years baseline, if no ideal baseline size is available.
  if (nrow(baseline) == 0) {
    return(5)
  }
  if (is.na(baseline$window)) {
    return(5)
  } else {
    return(baseline$window)
  }
}

calculate_baseline <- function(data, col_name, chart_type) {
  iso <- unique(data$iso3c)
  multiplier <- get_period_multiplier(chart_type)
  bl_size <- round(get_baseline_length(iso, chart_type, col_name) * multiplier)
  forecast_interval <- round(4 * multiplier)

  col <- sym(col_name)
  if (chart_type %in% c("yearly", "fluseason", "midyear")) {
    df <- data |> filter(date < 2020)
  } else {
    df <- data |> filter(year(date) < 2020)
  }

  # No rows, return
  if (nrow(tidyr::drop_na(df |> select(!!col))) < bl_size) {
    data <- data |> mutate(
      "{col_name}_baseline" := NA,
      "{col_name}_baseline_lower" := NA,
      "{col_name}_baseline_upper" := NA,
      "{col_name}_excess" := NA,
      "{col_name}_excess_lower" := NA,
      "{col_name}_excess_upper" := NA,
      .after = all_of(col)
    )

    return(data)
  } else {
    bl_data <- tail(df, bl_size)
    fc <- bl_data |>
      apply_model(col, chart_type) |>
      forecast(h = forecast_interval)
    fc_hl <- fabletools::hilo(fc, 95) |> unpack_hilo(cols = `95%`)

    bl <- bl_data |>
      apply_model(col, chart_type) |>
      forecast(new_data = bl_data)

    col <- sym(col_name)
    result <- data.frame(date = c(bl$date, fc$date)) |> mutate(
      "{col_name}_baseline" := c(bl$.mean, fc$.mean),
      "{col_name}_baseline_lower" := c(rep(NA, nrow(bl)), fc_hl$`95%_lower`),
      "{col_name}_baseline_upper" := c(rep(NA, nrow(bl)), fc_hl$`95%_upper`)
    )

    data |>
      left_join(result, by = "date") |>
      relocate(
        paste0(col_name, "_baseline"),
        paste0(col_name, "_baseline_lower"),
        paste0(col_name, "_baseline_upper"),
        .after = all_of(col_name)
      ) |>
      calculate_excess(col_name) |>
      round_x(col_name, ifelse(col_name == "deaths", 0, 2))
  }
}

round_x <- function(data, col_name, digits = 0) {
  data |>
    mutate(
      "{col_name}_baseline" :=
        round(!!sym(paste0(col_name, "_baseline")), digits),
      "{col_name}_baseline_lower" :=
        round(!!sym(paste0(col_name, "_baseline_lower")), digits),
      "{col_name}_baseline_upper" :=
        round(!!sym(paste0(col_name, "_baseline_upper")), digits),
      "{col_name}_excess" :=
        round(!!sym(paste0(col_name, "_excess")), digits),
      "{col_name}_excess_lower" :=
        round(!!sym(paste0(col_name, "_excess_lower")), digits),
      "{col_name}_excess_upper" :=
        round(!!sym(paste0(col_name, "_excess_upper")), digits)
    )
}

calculate_baseline_excess <- function(data, chart_type) {
  if (nrow(data) == 0) {
    return(data)
  }
  if (chart_type %in% c("fluseason", "midyear")) {
    ts <- data |>
      mutate(date = as.integer(right(date, 4))) |>
      as_tsibble(index = date)
  } else {
    ts <- data |> as_tsibble(index = date)
  }

  result <- ts |>
    calculate_baseline(col_name = "deaths", chart_type) |>
    calculate_baseline(col_name = "cmr", chart_type)
  if ("asmr_who" %in% colnames(ts) && sum(!is.na(ts$asmr_who)) > 0) {
    result <- result |>
      calculate_baseline(col_name = "asmr_who", chart_type) |>
      calculate_baseline(col_name = "asmr_esp", chart_type) |>
      calculate_baseline(col_name = "asmr_usa", chart_type) |>
      calculate_baseline(col_name = "asmr_country", chart_type)
  }

  if (chart_type %in% c("fluseason", "midyear")) {
    # Restore Flu Season Notation
    result <- result |>
      as_tibble() |>
      mutate(date = paste0(date - 1, "-", date))
  } else {
    result |> as_tibble()
  }
}

summarize_data_all <- function(dd_all, dd_asmr, type) {
  a <- summarize_data_by_time(dd_all, type)
  if (nrow(dd_asmr) == 0) {
    return(a)
  }
  b <- summarize_data_by_time(dd_asmr, type)
  a |> left_join(b, by = c("iso3c", "age_group", "date"))
}

summarize_data_by_time <- function(df, type) {
  fun <- get(type)
  df |>
    mutate(!!type := fun(date), .after = date) |>
    group_by(.data$iso3c, .data$age_group) |>
    group_modify(~ aggregate_data(.x, type), .keep = TRUE) |>
    ungroup()
}

fill_gaps_na <- function(df) {
  ts <- df |> as_tsibble(index = date)
  if (!tsibble::has_gaps(ts)) {
    return(ts)
  }
  ts |>
    tsibble::fill_gaps() |>
    tidyr::fill(population, .direction = "down") |>
    fill(source, .direction = "down")
}

save_info <- function(df, upload) {
  result <- tibble()
  for (code in unique(df$iso3c)) {
    df_country <- df |> filter(.data$iso3c == code)
    for (t in unique(df_country$type)) {
      df_country_type <- df_country |> filter(.data$type == t)
      for (s in unique(df_country_type$source)) {
        df_country_type_source <- df_country_type |> filter(.data$source == s)
        result <- rbind(
          result,
          tibble(
            iso3c = code,
            jurisdiction = head(df_country_type_source$jurisdiction, n = 1),
            type = t,
            source = s,
            min_date = min(df_country_type_source$date),
            max_date = max(df_country_type_source$date),
            age_groups = paste(
              unique(df_country_type_source$age_group),
              collapse = ", "
            )
          )
        )
      }
    }
  }
  save_csv(result, "mortality/world_meta", upload)
}

expand_daily <- function(df) {
  cols <- c("deaths", "cmr")
  yearly <- df |>
    filter(.data$type == 1) |>
    get_daily_from_yearly(cols)
  monthly <- df |>
    filter(.data$type == 2) |>
    get_daily_from_monthly(cols)
  weekly <- df |>
    filter(.data$type == 3) |>
    get_daily_from_weekly(cols)

  rbind(yearly, monthly, weekly)
}

append_dataset <- function(
    weekly,
    monthly,
    quarterly,
    yearly,
    by_fluseason,
    by_midyear,
    ag) {
  postfix <- ifelse(ag == "all", "", paste0("_", ag))

  append_csv(
    df = weekly |>
      calculate_baseline_excess("weekly") |>
      select(-all_of("age_group")),
    name = paste0("mortality/world_weekly", postfix)
  )

  weekly104wsma <- weekly |>
    calc_sma(104) |>
    calculate_baseline_excess("weekly_104w_sma") |>
    select(-all_of("age_group")) |>
    filter(!is.na(.data$deaths))
  append_csv(
    df = weekly104wsma,
    name = paste0("mortality/world_weekly_104w_sma", postfix)
  )

  weekly52wsma <- weekly |>
    calc_sma(52) |>
    calculate_baseline_excess("weekly_52w_sma") |>
    select(-all_of("age_group")) |>
    filter(!is.na(.data$deaths))
  append_csv(
    df = weekly52wsma,
    name = paste0("mortality/world_weekly_52w_sma", postfix)
  )

  weekly26wsma <- weekly |>
    calc_sma(26) |>
    calculate_baseline_excess("weekly_26w_sma") |>
    select(-all_of("age_group")) |>
    filter(!is.na(.data$deaths))
  append_csv(
    df = weekly26wsma,
    name = paste0("mortality/world_weekly_26w_sma", postfix)
  )

  weekly13wsma <- weekly |>
    calc_sma(14) |>
    calculate_baseline_excess("weekly_14w_sma") |>
    select(-all_of("age_group")) |>
    filter(!is.na(.data$deaths))
  append_csv(
    df = weekly13wsma,
    name = paste0("mortality/world_weekly_13w_sma", postfix)
  )

  monthly <- monthly |>
    calculate_baseline_excess("monthly") |>
    select(-all_of("age_group"))
  append_csv(
    df = monthly,
    name = paste0("mortality/world_monthly", postfix)
  )

  quarterly <- quarterly |>
    calculate_baseline_excess("quarterly") |>
    select(-all_of("age_group"))
  append_csv(
    df = quarterly,
    name = paste0("mortality/world_quarterly", postfix)
  )

  yearly <- yearly |>
    calculate_baseline_excess("yearly") |>
    select(-all_of("age_group"))
  append_csv(
    df = yearly,
    name = paste0("mortality/world_yearly", postfix)
  )

  by_fluseason <- by_fluseason |>
    calculate_baseline_excess("fluseason") |>
    select(-all_of("age_group"))
  append_csv(
    df = by_fluseason,
    name = paste0("mortality/world_fluseason", postfix)
  )

  by_midyear <- by_midyear |>
    calculate_baseline_excess("midyear") |>
    select(-all_of("age_group"))
  append_csv(
    df = by_midyear,
    name = paste0("mortality/world_midyear", postfix)
  )
}
