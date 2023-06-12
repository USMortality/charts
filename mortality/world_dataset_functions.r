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
    group_by(across(all_of(c("iso3c", "jurisdiction", col))))
}

aggregate_data <- function(data, fun) {
  # Filter ends
  result <- switch(fun,
    yearweek = filter_by_complete_temp_values(data, fun, 7),
    yearmonth = filter_by_complete_temp_values(data, fun, 28),
    yearquarter = filter_by_complete_temp_values(data, fun, 90),
    year = filter_by_complete_temp_values(data, fun, 365),
    fluseason = filter_by_complete_temp_values(data, fun, 365),
    midyear = filter_by_complete_temp_values(data, fun, 365)
  )
  if ("cmr" %in% names(data)) {
    result <- result |>
      summarise(
        deaths = round(sumIfNotEmpty(deaths)),
        population = round(mean(population)),
        cmr = round(sumIfNotEmpty(cmr), digits = 1)
      )
  }
  if ("asmr_who" %in% names(data)) {
    result <- result |>
      summarise(
        asmr_who = round(sumIfNotEmpty(asmr_who), digits = 1),
        asmr_esp = round(sumIfNotEmpty(asmr_esp), digits = 1),
        asmr_usa = round(sumIfNotEmpty(asmr_usa), digits = 1),
        asmr_country = round(sumIfNotEmpty(asmr_country), digits = 1)
      )
  }
  result |>
    ungroup() |>
    as_tibble() |>
    rename("date" = fun)
}

filter_ytd <- function(data, max_date_cmr, max_date_asmr) {
  year(max_date_cmr) <- year(data$date[1])
  year(max_date_asmr) <- year(data$date[1])

  data <- data |>
    filter(date <= max_date_cmr) |>
    mutate(asmr_who = ifelse(date <= max_date_asmr, asmr_who, NA)) |>
    mutate(asmr_esp = ifelse(date <= max_date_asmr, asmr_esp, NA)) |>
    mutate(asmr_usa = ifelse(date <= max_date_asmr, asmr_usa, NA)) |>
    mutate(asmr_country = ifelse(date <= max_date_asmr, asmr_country, NA)) |>
    mutate(max_date_cmr = max_date_cmr) |>
    mutate(max_date_asmr = max_date_asmr)
}

calc_ytd <- function(data) {
  nested <- data |> nest(data = !year)

  cmr_data <- nested[[2]][[length(nested[[2]])]] |> filter(!is.na(cmr))
  asmr_data <- nested[[2]][[length(nested[[2]])]] |> filter(!is.na(asmr_who))
  max_date_cmr <- max(cmr_data$date)
  max_date_asmr <- if (length(asmr_data) > 0) max(asmr_data$date) else NA
  nested |>
    mutate(data = lapply(data, filter_ytd, max_date_cmr, max_date_asmr)) |>
    unnest(cols = c(data))
}

aggregate_data_ytd <- function(data) {
  data |>
    group_by(iso3c, jurisdiction, year, max_date_cmr, max_date_asmr) |>
    summarise(
      deaths = round(sumIfNotEmpty(deaths)),
      cmr = round(sumIfNotEmpty(cmr), digits = 1),
      asmr_who = round(sumIfNotEmpty(asmr_who), digits = 1),
      asmr_esp = round(sumIfNotEmpty(asmr_esp), digits = 1),
      asmr_usa = round(sumIfNotEmpty(asmr_usa), digits = 1),
      asmr_country = round(sumIfNotEmpty(asmr_country), digits = 1)
    ) |>
    ungroup() |>
    rename("date" = "year") |>
    as_tibble()
}

SMA <- function(vec, n) {
  sma <- c()
  sma[1:(n - 1)] <- NA
  for (i in n:length(vec)) {
    sma[i] <- mean(vec[(i - n + 1):i], na.rm = TRUE)
  }
  reclass(sma, vec)
}

calc_sma <- function(data, n) {
  if (nrow(data) < n) stop("Not enough rows for SMA")

  data$deaths <- round(SMA(data$deaths, n = n), 0)
  data$cmr <- round(SMA(data$cmr, n = n), 2)
  data$asmr_who <- round(SMA(data$asmr_who, n = n), 2)
  data$asmr_esp <- round(SMA(data$asmr_esp, n = n), 2)
  data$asmr_usa <- round(SMA(data$asmr_usa, n = n), 2)
  data$asmr_country <- round(SMA(data$asmr_country, n = n), 2)
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
  if (chart_type %in% c("yearly", "fluseason", "ytd", "midyear")) {
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
    data |> model(TSLM(!!col ~ trend() + season()))
  } else {
    data |> model(TSLM(!!col ~ trend()))
  }
}

get_baseline_length <- function(iso, ct, cn) {
  if (!ct %in% c("fluseason", "midyear")) ct <- "yearly"
  baseline <- baseline_size |>
    filter(iso3c == iso & chart_type == ct & type == cn)
  ifelse(nrow(baseline) == 0, 5, baseline$window)
}

# TODO: Remove when next version of fabeletools (>0.3.2) is published.
unpack_hilo <- function(data, cols, names_sep = "_", names_repair = "check_unique") {
  orig <- data
  cols <- tidyselect::eval_select(enexpr(cols), data)
  if (any(bad_col <- !map_lgl(data[cols], inherits, "hilo"))) {
    abort(sprintf(
      "Not all unpacking columns are hilo objects (%s). All unpacking columns of unpack_hilo() must be hilo vectors.",
      paste(names(bad_col)[bad_col], collapse = ", ")
    ))
  }
  data[cols] <- map(data[cols], function(x) vctrs::vec_proxy(x)[c("lower", "upper")])
  data <- tidyr::unpack(data, names(cols), names_sep = names_sep, names_repair = names_repair)
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
  if (nrow(drop_na(df |> select(!!col))) == 0) {
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
    fc_hl <- hilo(fc, 95) |> unpack_hilo(cols = `95%`)

    bl <- bl_data |>
      apply_model(col, chart_type) |>
      forecast(new_data = bl_data)
    bl_hl <- hilo(bl, 95) |> unpack_hilo(cols = `95%`)

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
  if (nrow(data) < 2) {
    return(data)
  }
  if (chart_type %in% c("fluseason", "midyear")) {
    ts <- data |>
      mutate(date = as.integer(right(date, 4))) |>
      as_tsibble(index = date)
  } else {
    ts <- data |> as_tsibble(index = date)
  }

  print(paste("calculate_baseline_excess:", unique(ts$iso3c)))
  result <- ts |>
    calculate_baseline("deaths", chart_type) |>
    calculate_baseline("cmr", chart_type) |>
    calculate_baseline("asmr_who", chart_type) |>
    calculate_baseline("asmr_esp", chart_type) |>
    calculate_baseline("asmr_usa", chart_type) |>
    calculate_baseline("asmr_country", chart_type) |>
    as_tibble()

  if (chart_type %in% c("fluseason", "midyear")) {
    # Restore Flu Season Notation
    result |> mutate(date = paste0(date - 1, "-", date))
  } else {
    result
  }
}

filter_n_rows <- function(df, n) {
  df |>
    unnest(cols = c(data)) |>
    group_by(iso3c) |>
    filter(n() >= n) |>
    ungroup() |>
    nest(data = !iso)
}

get_nested_data_by_time <- function(dd_asmr, dd_all, fun_name) {
  fun <- get(fun_name)
  weekly_asmr <- dd_asmr |>
    mutate(!!fun_name := fun(date), .after = date) |>
    nest(data = !c("iso", "type")) |>
    mutate(data = lapply(data, aggregate_data, fun_name)) |>
    unnest(cols = c(data))
  weekly_all <- dd_all |>
    mutate(!!fun_name := fun(date), .after = date) |>
    nest(data = !c("iso", "type")) |>
    mutate(data = lapply(data, aggregate_data, fun_name)) |>
    unnest(cols = c(data))

  asmr <- rbind(
    weekly_asmr |> filter(type == "weekly"),
    weekly_asmr |> filter(type == "monthly"),
    weekly_asmr |> filter(type == "yearly")
  ) |>
    arrange(iso3c, date) |>
    distinct(iso3c, date, .keep_all = TRUE) |>
    select(-type)

  all <- rbind(
    weekly_all |> filter(type == "weekly"),
    weekly_all |> filter(type == "monthly"),
    weekly_all |> filter(type == "yearly")
  ) |>
    arrange(iso3c, date) |>
    distinct(iso3c, date, .keep_all = TRUE) |>
    select(-type)

  all |>
    left_join(asmr, by = c("iso3c", "date")) |>
    select(-iso.y, -jurisdiction.y) |>
    setNames(c(
      "iso", "iso3c", "jurisdiction", "date", "deaths", "population", "cmr",
      asmr_types
    )) |>
    nest(data = !c("iso"))
}
