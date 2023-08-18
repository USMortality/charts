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

filter_by_complete_temp_values <- function(data, col, n) {
  start <- data |>
    filter(.data[[col]] == head(data[[col]], n = 1)) |>
    group_by(across(all_of(col))) |>
    filter(n() >= n) |>
    ungroup()
  mid <- data |>
    filter(!.data[[col]] %in% c(
      head(data, n = 1)[[col]],
      tail(data, n = 1)[[col]]
    ))
  end <- data |>
    filter(.data[[col]] == tail(data, n = 1)[[col]]) |>
    group_by(across(all_of(col))) |>
    filter(n() >= n) |>
    ungroup()

  rbind(start, mid, end) |>
    group_by(across(all_of(c("iso3c", col))))
}

aggregate_data <- function(data, fun_name) {
  # Filter ends
  result <- switch(fun_name,
    yearweek = filter_by_complete_temp_values(data, fun_name, 7),
    yearmonth = filter_by_complete_temp_values(data, fun_name, 28),
    yearquarter = filter_by_complete_temp_values(data, fun_name, 90),
    year = filter_by_complete_temp_values(data, fun_name, 365),
    fluseason = filter_by_complete_temp_values(data, fun_name, 365),
    midyear = filter_by_complete_temp_values(data, fun_name, 365)
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
    as_tibble() |>
    dplyr::rename("date" = all_of(fun_name))
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
  if (nrow(data) < n) stop("Not enough rows for SMA")

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
      !!col - !!sym(paste0(col_name, "_baseline_upper"))
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
  ifelse(nrow(baseline) < 3, 3, baseline$window)
}

# TODO: Remove when next version of fabeletools (>0.3.2) is published.
unpack_hilo <- function(
    data,
    cols,
    names_sep = "_",
    names_repair = "check_unique") {
  orig <- data
  cols <- tidyselect::eval_select(dplyr::enexpr(cols), data)
  if (any(bad_col <- !purrr::map_lgl(data[cols], inherits, "hilo"))) {
    rlang::abort(sprintf(
      paste(
        "Not all unpacking columns are hilo objects (%s).",
        "All unpacking columns of unpack_hilo() must be hilo vectors."
      ),
      paste(names(bad_col)[bad_col], collapse = ", ")
    ))
  }
  data[cols] <- purrr::map(
    data[cols], function(x) {
      vctrs::vec_proxy(x)[c("lower", "upper")]
    }
  )
  data <- tidyr::unpack(
    data,
    names(cols),
    names_sep = names_sep, names_repair = names_repair
  )
  vctrs::vec_restore(data, orig)
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
    data[paste0(col, "_baseline")] <- NA
    data[paste0(col, "_baseline_lower")] <- NA
    data[paste0(col, "_baseline_upper")] <- NA
    data[paste0(col, "_excess")] <- NA
    data[paste0(col, "_excess_lower")] <- NA
    data[paste0(col, "_excess_upper")] <- NA

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
    bl_hl <- fabletools::hilo(bl, 95) |> unpack_hilo(cols = `95%`)

    result <- data.frame(date = c(bl$date, fc$date))
    result[paste0(col, "_baseline")] <- c(bl$.mean, fc$.mean)
    result[paste0(col, "_baseline_lower")] <- c(
      bl_hl$`95%_lower`,
      fc_hl$`95%_lower`
    )
    result[paste0(col, "_baseline_upper")] <- c(
      bl_hl$`95%_upper`,
      fc_hl$`95%_upper`
    )

    data |>
      left_join(result, by = "date") |>
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
  if (chart_type %in% c("fluseason", "midyear")) {
    ts <- data |>
      mutate(date = as.integer(right(date, 4))) |>
      as_tsibble(index = date)
  } else {
    ts <- data |> as_tsibble(index = date)
  }

  print(paste("calculate_baseline_excess:", unique(ts$iso3c)))
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

get_nested_data_by_time <- function(dd_asmr, dd_all, fun_name) {
  fun <- get(fun_name)
  df_asmr <- dd_asmr |>
    mutate(!!fun_name := fun(date), .after = date) |>
    nest(data = !c("iso", "type")) |>
    mutate(data = lapply(data, aggregate_data, fun_name)) |>
    unnest(cols = c(data))
  df_all <- dd_all |>
    mutate(!!fun_name := fun(date), .after = date) |>
    nest(data = !c("iso", "type")) |>
    mutate(data = lapply(data, aggregate_data, fun_name)) |>
    unnest(cols = c(data))

  asmr <- rbind(
    df_asmr |> filter(.data$type == "weekly"),
    df_asmr |> filter(.data$type == "monthly"),
    df_asmr |> filter(.data$type == "yearly")
  ) |>
    arrange(.data$iso3c, .data$date) |>
    distinct(.data$iso3c, .data$date, .keep_all = TRUE) |>
    select(-all_of("type"))

  all <- rbind(
    df_all |> filter(.data$type == "weekly"),
    df_all |> filter(.data$type == "monthly"),
    df_all |> filter(.data$type == "yearly")
  ) |>
    arrange(iso3c, date) |>
    distinct(iso3c, date, .keep_all = TRUE) |>
    select(-all_of("type"))

  all |>
    left_join(asmr, by = c("iso3c", "date")) |>
    select(!all_of("iso.y")) |>
    setNames(c(
      "iso", "iso3c", "date", "deaths", "population", "cmr",
      asmr_types
    )) |>
    nest(data = !c("iso"))
}

get_nested_data_by_time_age <- function(dd_age, fun_name) {
  fun <- get(fun_name)
  df_all <- dd_age |>
    mutate(!!fun_name := fun(date), .after = date) |>
    nest(data = !c("iso", "type", "age_group")) |>
    mutate(data = lapply(data, aggregate_data, fun_name)) |>
    unnest(cols = c(data))

  all <- rbind(
    df_all |> filter(.data$type == "weekly"),
    df_all |> filter(.data$type == "monthly"),
    df_all |> filter(.data$type == "yearly")
  ) |>
    arrange(.data$iso3c, date, .data$age_group) |>
    distinct(iso3c, date, age_group, .keep_all = TRUE) |>
    select(-all_of("type"))

  all |> nest(data = !c("iso", "age_group"))
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

save_info <- function(df) {
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
  save_csv(result, "mortality/world_meta", upload = TRUE)
}
