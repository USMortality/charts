source("lib/common.r")

# CONFIG
SEP <- " · "

# FUNCTIONS

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

calculate_baseline_excess <- function(
    df,
    metric_column,
    period_type,
    bl_years,
    bl_model,
    fc_years,
    year_period_multiplier = 1) {
  ts <- df |> as_tsibble(index = date)
  forecast_interval <- round(fc_years * year_period_multiplier)

  if (period_type %in% c("yearly", "fluseason", "midyear")) {
    bl_data <- ts |> filter(date %in% bl_years)
  } else {
    bl_data <- ts |> filter(lubridate::year(date) %in% bl_years)
  }

  fc <- bl_data |>
    fabletools::model(bl_model) |>
    fabletools::forecast(h = forecast_interval)
  fc_hl <- fabletools::hilo(fc, 95) |> fabletools::unpack_hilo(cols = "95%")

  bl <- bl_data |>
    fabletools::model(bl_model) |>
    fabletools::forecast(new_data = bl_data)

  result <- tibble(date = c(bl$date, fc$date)) |> dplyr::mutate(
    "{metric_column}_baseline" := c(bl$.mean, fc$.mean),
    "{metric_column}_baseline_lower" := c(rep(NA, nrow(bl)), fc_hl$`95%_lower`),
    "{metric_column}_baseline_upper" := c(rep(NA, nrow(bl)), fc_hl$`95%_upper`)
  )

  ts |>
    left_join(result, by = "date") |>
    relocate(
      paste0(metric_column, "_baseline"),
      paste0(metric_column, "_baseline_lower"),
      paste0(metric_column, "_baseline_upper"),
      .after = all_of(metric_column)
    ) |>
    calculate_excess(metric_column) |>
    mutate(
      "{metric_column}_excess_p" := !!sym(paste0(metric_column, "_excess")) /
        !!sym(paste0(metric_column, "_baseline")),
      "{metric_column}_excess_p_lower" :=
        !!sym(paste0(metric_column, "_excess_upper")) /
          !!sym(paste0(metric_column, "_baseline_lower")),
      "{metric_column}_excess_p_upper" :=
        !!sym(paste0(metric_column, "_excess_lower")) /
          !!sym(paste0(metric_column, "_baseline_upper")),
      "{metric_column}_excess_sign" :=
        !!sym(paste0(metric_column, "_excess_p_lower")) > 0
    ) |>
    as_tibble()
}

make_line_plot <- function(ts_plot) {
  ggplot(ts_plot, aes(date)) +
    geom_line(aes(y = cmr), color = "#4366ad") +
    geom_point(aes(y = cmr), color = "#4366ad") +
    geom_line(aes(y = cmr_baseline), color = "#44781d", linetype = "dashed") +
    geom_ribbon(
      aes(
        x = date,
        ymin = cmr_baseline_lower,
        ymax = cmr_baseline_upper
      ),
      alpha = 0.2,
      fill = "#44781d",
      color = "#44781d"
    ) +
    labs(
      title = paste0(
        "Crude Mortality Rate [",
        unique(ts_plot$jurisdiction), ", Germany]"
      ),
      subtitle = paste(
        paste0(
          "Population 2020: ",
          format((ts_plot |> filter(date == 2020))$population, big.mark = ",")
        ),
        "Source: statistik.sachsen.de/genonline",
        "95% PI",
        sep = SEP
      ),
      x = "Calendar Year",
      y = "Deaths/1,000 population"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
    theme(legend.position = "top", legend.title = element_blank()) +
    scale_x_continuous(breaks = unique(ts$date)) +
    scale_y_continuous(labels = comma)
}

make_excess_bar_plot <- function(ts_plot) {
  ggplot(ts_plot, aes(date)) +
    geom_col(aes(y = cmr_excess_p), fill = "#bb4664") +
    geom_text(aes(
      y = cmr_excess_p,
      label = scales::percent(cmr_excess_p, accuracy = 0.1)
    ), vjust = -0.5) +
    geom_errorbar(
      aes(
        ymin = cmr_excess_p_lower,
        ymax = cmr_excess_p_upper
      ),
      width = 0.4,
      color = "black"
    ) +
    labs(
      title = paste0(
        "Relative Excess Crude Mortality Rate [",
        unique(ts_plot$jurisdiction), ", Germany]"
      ),
      subtitle = "Source: statistik.sachsen.de/genonline · 95% PI",
      x = "Calendar Year",
      y = "Excess Deaths/1,000 population"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
    theme(legend.position = "top", legend.title = element_blank()) +
    scale_x_continuous(breaks = unique(ts$date)) +
    scale_y_continuous(labels = scales::percent_format())
}

make_scatter_plot <- function(df_plot, extra_subtitle = NULL) {
  st <- if (is.null(extra_subtitle)) {
    "Source: statistik.sachsen.de/genonline"
  } else {
    paste("Source: statistik.sachsen.de/genonline", extra_subtitle, sep = " · ")
  }
  ggplot(df_plot, aes(x = date, y = cmr_excess_p)) +
    geom_point() +
    facet_wrap(~ reorder(level_name, level), scales = "free") +
    labs(
      title = paste0(
        "Relative Excess Crude Mortality Rate [Saxony, Germany]"
      ),
      subtitle = st,
      x = "Calendar Year",
      y = "Excess Deaths/100k population"
    ) +
    theme_bw() +
    scale_x_continuous(breaks = unique(ts$date)) +
    scale_y_continuous(labels = scales::percent_format()) +
    theme(legend.position = "top") +
    theme(legend.title = element_blank()) +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5)) +
    geom_hline(yintercept = 0, linewidth = 0.5)
}

get_limits <- function(ts) {
  c(
    floor(min(ts$cmr_excess_p) * 100) / 100,
    ceiling(max(ts$cmr_excess_p) * 100) / 100
  )
}

as_pct <- function(x) {
  sprintf("%.1f%%", as.numeric(x) * 100)
}

# DATA

# Genesis 12613-020Z: Gestorbene (je 1000 EW) auf Gemeinden
cmr <- as_tibble(
  head(
    read.csv("./data_static/12613-020Z.csv",
      sep = ";",
      skip = 5,
      colClasses = c("character"),
      fileEncoding = "latin1"
    ),
    -4
  )
) |>
  setNames(c("date", "id", "jurisdiction", "cmr")) |>
  rowwise() |>
  mutate(
    jurisdiction = trimws(jurisdiction),
    level = nchar(id),
    date = as.integer(date),
    cmr = ifelse(cmr == "-", NA, as.double(gsub(",", ".", cmr)))
  )

# Genesis 12613-010Z: Gestorbene (absolut) nach Geschlecht auf Gemeinden
deaths <- as_tibble(
  head(
    read.csv("./data_static/12613-010Z.csv",
      sep = ";",
      skip = 5,
      colClasses = c("character"),
      fileEncoding = "latin1"
    ),
    -4
  )
) |>
  setNames(c(
    "date", "id", "jurisdiction", "deaths_all", "deaths_m", "deaths_f"
  )) |>
  pivot_longer(
    cols = 4:6,
    names_to = "sex",
    names_prefix = "deaths_",
    values_to = "deaths"
  ) |>
  filter(date != "", id != "", sex != "", deaths != "") |>
  rowwise() |>
  mutate(
    date = as.integer(date),
    jurisdiction = trimws(jurisdiction),
    level = nchar(id),
    deaths = as_integer(deaths)
  )

# Genesis 12410-010Z: Bevölkerung Fortschreibung zum 30.06. nach Geschlecht
# auf Gemeinden
population <- as_tibble(
  head(
    read.csv("./data_static/12410-010Z.csv",
      sep = ";",
      skip = 5,
      colClasses = c("character"),
      fileEncoding = "latin1"
    ),
    -4
  )
) |>
  setNames(c(
    "date", "id", "jurisdiction", "population_all", "population_m",
    "population_f"
  )) |>
  pivot_longer(
    cols = 4:6,
    names_to = "sex",
    names_prefix = "population_",
    values_to = "population"
  ) |>
  filter(date != "", id != "", sex != "", population != "") |>
  rowwise() |>
  mutate(
    date = year(as.Date(date, format = "%d.%m.%Y")),
    jurisdiction = trimws(jurisdiction),
    level = nchar(id),
    population = as_integer(population)
  )

# Calculate Zschepplin data, since cmr/population data is missing.
# Use 2021 population for 2022.
z_d <- deaths |>
  filter(jurisdiction == "Zschepplin", date == 2022, sex == "all")
z_p <- population |>
  filter(jurisdiction == "Zschepplin", date == 2021, sex == "all")
cmr <- cmr |> add_row(tibble(
  date = 2022L,
  id = "14730360",
  jurisdiction = "Zschepplin",
  cmr = z_d$deaths / z_p$population * 1000,
  level = 8L
))

df <- cmr |>
  left_join(
    population |> filter(sex == "all") |> select(date, id, population),
    by = join_by(date, id)
  )

# Calculate Baseline & Excess
ts <- df |>
  filter(date >= 2010) |>
  group_by(id) |>
  mutate(level_name = case_when(
    level == 2 ~ "State",
    level == 3 ~ "NUTS-2 Region",
    level == 5 ~ "County",
    level == 8 ~ "Municipality",
    .default = NA
  )) |>
  group_modify(~ calculate_baseline_excess(
    .x,
    metric_column = "cmr",
    period_type = "yearly",
    bl_years = 2010:2019,
    bl_model = fable::TSLM(cmr ~ trend()),
    fc_years = 3
  ))

# Calculate cumulative
max_year <- max(ts$date)
cum <- ts |>
  filter(date >= 2020) |>
  group_by(id, jurisdiction, level, level_name) |>
  summarize(
    cmr = mean(cmr),
    cmr_baseline = mean(cmr_baseline),
    population = mean(population),
    cmr_excess_sign_all = all(cmr_excess_sign),
    cmr_excess_sign = any(cmr_excess_sign)
  ) |>
  mutate(
    cmr_excess = cmr - cmr_baseline,
    cmr_excess_p = cmr_excess / cmr_baseline
  )
cum$is_cumulative <- TRUE

ts <- bind_rows(ts, cum)
