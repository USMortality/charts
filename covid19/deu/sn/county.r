source("covid19/deu/sn/common.r")

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
    watermark() +
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
    watermark() +
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
    watermark() +
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

df2 <- df |>
  filter(id == "14521") |>
  mutate(level_name = case_when(
    level == 2 ~ "State",
    level == 3 ~ "NUTS-2 Region",
    level == 5 ~ "County",
    level == 8 ~ "Municipality",
    .default = NA
  ))

# Munis with any year of sign. excess
munis <- ts |> filter(date %in% 2020:2022, level == 8)
munis_no_sign_excess <- munis |> filter(any(cmr_excess_sign) == FALSE)
munis_no_sign_excess |> arrange(desc(population))
munis_sign_excess <- munis |> filter(any(cmr_excess_sign) == TRUE)
munis_sign_excess |> arrange(desc(population))

n_munis <- length(unique(munis$id))
n_munis_sign_excess <- length(unique(munis_sign_excess$id))
n_munis_sign_excess / n_munis # -> 43.8%

# Plot individual id
jurisdictions <- list(
  state = unique((ts |> filter(level == 2))$id),
  nuts2 = unique((ts |> filter(level == 3))$id),
  county = unique((ts |> filter(level == 5))$id),
  muni = unique((ts |> filter(level == 8))$id)
)

for (key in names(jurisdictions)) {
  ids <- jurisdictions[[key]]
  for (i in ids) {
    ts_plot <- ts |> filter(id == i)
    # CMR
    chart <- make_line_plot(ts_plot)
    save_chart(chart, paste0("deu/sn/", key, "/cmr_", i),
      upload = FALSE
    )
    # relative eCMR
    chart <- make_excess_bar_plot(ts_plot)
    save_chart(chart, paste0("deu/sn/", key, "/rel_ecmr_", i),
      upload = FALSE
    )
  }
}

# Scatter Plots
## All munis
make_scatter_plot(ts |> filter(date >= 2010))

## All munis w/ sign. excess
ts_plot <- ts |> filter(
  level == 8,
  date >= 2010,
  any(cmr_excess_sign, na.rm = TRUE)
)
make_scatter_plot(
  ts_plot,
  paste(
    "Jurisdictions with any sign. excess ('20-'22)",
    paste0("n=", length(unique(ts_plot$id))),
    sep = SEP
  )
)

## All munis w/o sign. excess
ts_plot <- ts |> filter(
  level == 8,
  date >= 2010,
  !any(cmr_excess_sign, na.rm = TRUE)
)
make_scatter_plot(
  ts_plot,
  paste(
    "Jurisdictions without any sign. excess ('20-'22)",
    paste0("n=", length(unique(ts_plot$id))),
    sep = SEP
  )
)

## All munis w sign. excess in all years
ts_plot <- ts |> filter(
  level == 8,
  date >= 2010,
  all(cmr_excess_sign, na.rm = TRUE)
)
make_scatter_plot(
  ts_plot,
  paste(
    "Jurisdictions with all years sign. excess ('20-'22)",
    paste0("n=", length(unique(ts_plot$id))),
    sep = SEP
  )
)

# Plot County Map
st_layers("./data_static/gadm41_DEU.gpkg")
germany <- st_read(dsn = "./data_static/gadm41_DEU.gpkg", layer = "ADM_ADM_2")

y <- "2020"
lims <- get_limits(ts |> filter(level == 5, date %in% 2020:2023))
ts_plot <- germany |>
  filter(NAME_1 == "Sachsen") |>
  select(CC_2, geom) |>
  inner_join(ts |> filter(date == y), by = join_by(CC_2 == id)) |>
  as_tibble()

# Mean Excess
ggplot(ts_plot) +
  geom_sf(aes(geometry = geom, fill = cmr_excess_p)) +
  scale_fill_viridis_c(
    option = "D",
    na.value = "#bbbbbb",
    name = "eCMR",
    label = scales::percent,
    limits = lims
  ) +
  labs(
    title = paste0(
      "Relative Excess Crude Mortality Rate (eCMR) - ",
      "Counties [Sachsen, Germany]"
    ),
    subtitle = paste0(
      "Source: statistik.sachsen.de/genonline ·",
      " Year: ", y, " · 2010-2019 linear trend baseline"
    ),
    x = "",
    y = ""
  ) +
  theme(plot.background = element_rect(fill = "white")) +
  theme_bw() +
  watermark()

# Significant Excess
ggplot(ts_plot |> mutate(cmr_excess_sign = as.integer(cmr_excess_sign))) +
  geom_sf(aes(geometry = geom, fill = cmr_excess_sign)) +
  scale_fill_viridis_c(
    option = "D",
    na.value = "#bbbbbb",
    name = "eCMR",
    label = scales::percent,
    limits = c(0, 1)
  ) +
  labs(
    title = paste0(
      "Jurisdictions with Stat. Sign. Excess Mortality (eCMR) - ",
      "Counties [Sachsen, Germany]"
    ),
    subtitle = paste0(
      "Source: statistik.sachsen.de/genonline ·",
      " Year: ", y, " · 2010-2019 linear trend baseline · ",
      "yellow = TRUE"
    ),
    x = "",
    y = ""
  ) +
  theme(plot.background = element_rect(fill = "white")) +
  theme_bw() +
  watermark() +
  theme(legend.position = "none")

# Plot Muni Map
st_layers("./data_static/gadm41_DEU.gpkg")
germany_2 <- st_read(
  dsn = "./data_static/gadm41_DEU.gpkg", layer = "ADM_ADM_2"
) |> filter(NAME_1 == "Sachsen")
germany_3 <- st_read(
  dsn = "./data_static/gadm41_DEU.gpkg", layer = "ADM_ADM_3"
) |>
  filter(NAME_1 == "Sachsen") |>
  mutate(id = CC_3)
germany_4 <- st_read(
  dsn = "./data_static/gadm41_DEU.gpkg", layer = "ADM_ADM_4"
) |>
  filter(NAME_1 == "Sachsen") |>
  mutate(id = paste0(left(CC_4, 5), right(CC_4, 3)))

# Update "holes" in 4th layer geo, using geos from 3rd layer geo.
# Leipzig
germany_4$geom[germany_4$id == "14729370"] <-
  germany_3$geom[germany_3$id == "147295310"]
# Meißen
germany_4$geom[germany_4$id == "14627290"] <-
  germany_3$geom[germany_3$id == "146275241"]
# Bautzen
germany_4$geom[germany_4$id == "14625250"] <-
  germany_3$geom[germany_3$id == "146255216"]
germany_4$geom[germany_4$id == "14625200"] <-
  germany_3$geom[germany_3$id == "146255213"]
# Mittelsachsen
germany_4$id[germany_4$id == "14522450"] <- "14522275"
germany_4$geom[germany_4$id == "14522275"] <-
  germany_3$geom[germany_3$id == "145225123"]
germany_4$geom[germany_4$id == "14522275"] <-
  germany_3$geom[germany_3$id == "145225123"]
to_merge <- germany_3 |> filter(id %in% c("145220370", "145220080"))
germany_4$geom[germany_4$id == "14522080"] <- st_union(to_merge$geom)
# Erzgebirgkreis
to_merge <- germany_4 |> filter(id %in% c("14521460", "14521470"))
germany_4$geom[germany_4$id == "14521460"] <- st_union(to_merge$geom)

germany_4$id[germany_4$id == "14521030"] <- "14521035"
to_merge <- germany_4 |> filter(id %in% c("14521035", "14521050"))
germany_4$geom[germany_4$id == "14521035"] <- st_union(to_merge$geom)

# ggplot(
#   germany_4 |> filter(id %in% c("14521035"))
# ) +
#   geom_sf(aes(geometry = geom))

# ts |> filter(jurisdiction == "Zschepplin") # 14521035
# germany_3 |> filter(NAME_3 == "Aue") # 145210030, 145210050
# germany_4 |> filter(NAME_4 == "Aue") # 14521035, 14521050

y <- "2022"
ts2 <- ts |>
  filter(
    level == 8,
    date >= 2020,
    any(cmr_excess_sign, na.rm = TRUE)
  ) |>
  select(id, date, cmr_excess_sign) |>
  group_by(id) |>
  summarize(cmr_excess_sign = sum(cmr_excess_sign))
ts_plot <- germany_4 |>
  inner_join(ts2c("id")) |>
  as_tibble() |>
  mutate(cmr_excess_p = pmax(0, pmin(1, cmr_excess_p)))
lims <- get_limits(ts_plot |> filter(level == 8, date %in% 2020:2023))

# Mean Excess
ggplot(
  ts_plot
  # |> filter(NAME_2 %in% c("Nordsachsen"))
) +
  geom_sf(aes(geometry = geom, fill = cmr_excess_p)) +
  scale_fill_viridis_c(
    option = "D",
    na.value = "#bbbbbb",
    name = "eCMR",
    label = scales::percent,
    limits = lims
  ) +
  labs(
    title = paste0(
      "Relative Excess Crude Mortality Rate (eCMR) - ",
      "Counties [Sachsen, Germany]"
    ),
    subtitle = paste0(
      "Source: statistik.sachsen.de/genonline ·",
      " Year: ", y, " · 2010-2019 linear trend baseline"
    ),
    x = "",
    y = ""
  ) +
  theme(plot.background = element_rect(fill = "white")) +
  theme_bw() +
  watermark()

# Significant Excess
ggplot(ts_plot |> mutate(cmr_excess_sign = as.integer(cmr_excess_sign))) +
  geom_sf(aes(geometry = geom, fill = cmr_excess_sign)) +
  scale_fill_viridis_c(
    option = "D",
    na.value = "#bbbbbb",
    name = "eCMR",
    label = scales::percent,
    limits = c(0, 1)
  ) +
  labs(
    title = paste0(
      "Jurisdictions with Stat. Sign. Excess Mortality (eCMR) - ",
      "Counties [Sachsen, Germany]"
    ),
    subtitle = paste0(
      "Source: statistik.sachsen.de/genonline ·",
      " Year: ", y, " · 2010-2019 linear trend baseline · ",
      "yellow = TRUE"
    ),
    x = "",
    y = ""
  ) +
  theme(plot.background = element_rect(fill = "white")) +
  theme_bw() +
  watermark() +
  theme(legend.position = "none")

# Significant Excess, all years
ts2 <- ts |>
  filter(level == 8, date >= 2020) |>
  select(id, date, cmr_excess_sign) |>
  group_by(id) |>
  summarize(cmr_excess_sign = sum(cmr_excess_sign)) |>
  mutate(cmr_excess_sign = ifelse(cmr_excess_sign >= 1, 1, 0))
ts_plot <- germany_4 |>
  inner_join(ts2, by = c("id")) |>
  as_tibble()

y <- "2020-2022"
ggplot(ts_plot) +
  geom_sf(aes(geometry = geom, fill = cmr_excess_sign)) +
  scale_fill_viridis_c(
    option = "D",
    na.value = "#bbbbbb",
    name = "eCMR",
    label = scales::percent,
    limits = c(0, 1)
  ) +
  labs(
    title = paste0(
      "Jurisdictions with Stat. Sign. Excess Mortality (eCMR) - ",
      "Counties [Sachsen, Germany]"
    ),
    subtitle = paste0(
      "Source: statistik.sachsen.de/genonline ·",
      " Year: ", y, " · 2010-2019 linear trend baseline · ",
      "yellow = TRUE"
    ),
    x = "",
    y = ""
  ) +
  theme(plot.background = element_rect(fill = "white")) +
  theme_bw() +
  watermark() +
  theme(legend.position = "none")





# ggplot(germany_2) +
#   geom_sf(aes(geometry = geom)) +
#   coord_sf()

# y <- "2020"

# ts2 <- ts |> filter(level == 8)
# ts_plot <- germany_4 |>
ts2 <- ts |>
  filter(
    level == 8,
    date >= 2020,
    any(cmr_excess_sign, na.rm = TRUE)
  ) |>
  select(id, date, cmr_excess_sign) |>
  group_by(id) |>
  summarize(cmr_excess_sign = sum(cmr_excess_sign))
#   select(id, geom) |>
#   ts2= c("id")) |>
#   as_tibble()

# ggplot(ts_plot) +
#   geom_sf(aes(geometry = geom, fill = cmr_excess_p)) +
#   coord_sf() +
#   scale_fill_viridis_c(
#     option = "D",
#     na.value = "#bbbbbb",
#     name = "eCMR",
#     label = scales::percent
#   ) +
#   labs(
#     title = paste0(
#       "Relative Excess Crude Mortality Rate (eCMR) - ",
#       "Counties [Sachsen, Germany]"
#     ),
#     subtitle = paste0(
#       "Source: statistik.sachsen.de/genonline ·",
#       " Year: ", y, " · 2010-2019 linear trend baseline"
#     ),
#     x = "",
#     y = ""
#   ) +
#   theme(plot.background = element_rect(fill = "white")) +
#   theme_bw() +
#   watermark()

# source('./covid19/deu/sn/county.r')
