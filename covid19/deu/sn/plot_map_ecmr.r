# Plot County Map
st_layers("./data_static/gadm41_DEU.gpkg")
germany <- st_read(dsn = "./data_static/gadm41_DEU.gpkg", layer = "ADM_ADM_2")

make_chart <- function(y, cum = FALSE) {
  lims <- get_limits(ts |> filter(level == 5, date >= 2020))

  df <- if (cum) {
    ts |> filter(is_cumulative == TRUE)
  } else {
    ts |> filter(date %in% y)
  }

  ts_plot <- germany |>
    select(CC_2, geom) |>
    inner_join(df, by = join_by(CC_2 == id)) |>
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
        " Year: ", paste(y, collapse = ", "),
        " · 2010-2019 linear trend baseline"
      ),
      x = "",
      y = ""
    ) +
    theme(plot.background = element_rect(fill = "white")) +
    theme_bw()
}

for (y in 2020:2022) {
  save_chart(
    make_chart(y),
    paste0("deu/sn/county/map/ecmr_mean_", y),
    upload = FALSE
  )
}
save_chart(
  make_chart(c(2020:2022), TRUE),
  "deu/sn/county/map/ecmr_mean_2020_2022",
  upload = FALSE
)

# Significant Excess
make_chart <- function(y, cum = FALSE) {
  df <- if (cum) {
    ts |> filter(is_cumulative == TRUE)
  } else {
    ts |> filter(date %in% y)
  }
  df <- df |> mutate(cmr_excess_sign = as.integer(cmr_excess_sign))

  ts_plot <- germany |>
    select(CC_2, geom) |>
    inner_join(df, by = join_by(CC_2 == id)) |>
    as_tibble()

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
    theme(legend.position = "none")
}

for (y in 2020:2022) {
  save_chart(
    make_chart(y),
    paste0("deu/sn/county/map/ecmr_sign_", y),
    upload = FALSE
  )
}
save_chart(
  make_chart(y = c(2020:2022), TRUE),
  "deu/sn/county/map/ecmr_sign_2020_2022",
  upload = FALSE
)

# Plot Muni Maps
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

make_chart <- function(y, cum = FALSE) {
  df <- if (cum) {
    ts |> filter(is_cumulative == TRUE, level == 8)
  } else {
    ts |> filter(date %in% y, level == 8)
  }

  ts_plot <- germany_4 |>
    inner_join(df) |>
    as_tibble() |>
    mutate(cmr_excess_p = pmax(0, pmin(1, cmr_excess_p)))

  # Mean Excess
  ggplot(ts_plot) +
    geom_sf(aes(geometry = geom, fill = cmr_excess_p)) +
    scale_fill_viridis_c(
      option = "D",
      na.value = "#bbbbbb",
      name = "eCMR",
      label = scales::percent,
      limits = c(0, 1)
    ) +
    labs(
      title = paste0(
        "Relative Excess Crude Mortality Rate (eCMR) - ",
        "Municipalities [Sachsen, Germany]"
      ),
      subtitle = paste0(
        "Source: statistik.sachsen.de/genonline ·",
        " Year: ", paste(y, collapse = ", "),
        " · 2010-2019 linear trend baseline"
      ),
      x = "",
      y = ""
    ) +
    theme(plot.background = element_rect(fill = "white")) +
    theme_bw()
}

for (y in 2020:2022) {
  save_chart(
    make_chart(y),
    paste0("deu/sn/muni/map/ecmr_mean_", y),
    upload = FALSE
  )
}
save_chart(
  make_chart(y = c(2020:2022), TRUE),
  paste0("deu/sn/muni/map/ecmr_mean_2020_2022"),
  upload = FALSE
)

# Significant Excess
make_chart <- function(y, cum = FALSE) {
  df <- if (cum) {
    ts |> filter(is_cumulative == TRUE, level == 8)
  } else {
    ts |> filter(date %in% y, level == 8)
  }
  df <- df |> mutate(cmr_excess_sign = as.integer(cmr_excess_sign))

  ts_plot <- germany_4 |>
    inner_join(df) |>
    as_tibble() |>
    mutate(cmr_excess_p = pmax(0, pmin(1, cmr_excess_p)))

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
    theme(legend.position = "none")
}

for (y in 2020:2022) {
  save_chart(
    make_chart(y),
    paste0("deu/sn/muni/map/ecmr_sign_", y),
    upload = FALSE
  )
}
save_chart(
  make_chart(y = c(2020:2022), TRUE),
  "deu/sn/muni/map/ecmr_sign_2020_2022",
  upload = FALSE
)

# source('./covid19/deu/sn/plot_map_ecmr.r')
