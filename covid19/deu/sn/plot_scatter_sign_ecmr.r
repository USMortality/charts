# Scatter Plots

## All regions
chart <- make_scatter_plot(ts |> filter(date >= 2010))
save_chart(chart, "deu/sn/scatter_all", upload = FALSE)

## All munis w/ sign. excess
ts_plot <- ts |> filter(
  level == 8,
  date >= 2010,
  any(cmr_excess_sign, na.rm = TRUE)
)
chart <- make_scatter_plot(
  ts_plot,
  paste(
    "Jurisdictions with any sign. excess ('20-'22)",
    paste0("n=", length(unique(ts_plot$id))),
    sep = SEP
  )
)
save_chart(chart, "deu/sn/muni/scatter/cmr_excess_sign", upload = FALSE)

## All munis w/o sign. excess
ts_plot <- ts |> filter(
  level == 8,
  date >= 2010,
  !any(cmr_excess_sign, na.rm = TRUE)
)
chart <- make_scatter_plot(
  ts_plot,
  paste(
    "Jurisdictions without any sign. excess ('20-'22)",
    paste0("n=", length(unique(ts_plot$id))),
    sep = SEP
  )
)
save_chart(chart, "deu/sn/muni/scatter/cmr_no_excess_sign", upload = FALSE)

## All munis w sign. excess in all years
all_excess <- ts |> filter(level == 8, cmr_excess_sign_all == TRUE)
ts_plot <- ts |> filter(date >= 2010, id %in% all_excess$id)
chart <- make_scatter_plot(
  ts_plot,
  paste(
    "Jurisdictions with all years sign. excess ('20-'22)",
    paste0("n=", length(unique(ts_plot$id))),
    sep = SEP
  )
)

save_chart(chart, "deu/sn/muni/scatter/cmr_excess_sign_all", upload = FALSE)

# Excess vs population size
make_chart <- function(df, split_by_sign_excess) {
  if (split_by_sign_excess) {
    chart <- ggplot(df, aes(
      x = population, y = cmr_excess_p,
      color = has_sign_excess
    ))
  } else {
    chart <- ggplot(df, aes(x = population, y = cmr_excess_p))
  }
  chart +
    geom_point() +
    labs(
      title = paste0(
        "Excess Crude Mortality Rate (eCMR) vs. Population by Municipality ",
        "[Saxony, Germany]"
      ),
      subtitle = "2020-2022; Population 2020",
      x = "Population (Log)",
      y = "Excess Deaths/100k population"
    ) +
    theme_bw() +
    coord_trans(x = "log2") +
    scale_y_continuous(labels = scales::percent_format()) +
    theme(legend.position = "top") +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5)) +
    geom_hline(yintercept = 0, linewidth = 0.5) +
    scale_color_manual(values = c("#44781d", "#de5075")) +
    stat_correlation(use_label(c("R", "P", "n", "method"))) +
    stat_poly_line()
}

ts_plot <- ts |>
  filter(
    level == 8,
    date >= 2020,
  ) |>
  group_by(id, jurisdiction) |>
  summarize(
    has_sign_excess = any(cmr_excess_sign, na.rm = TRUE),
    cmr = sum(cmr),
    cmr_baseline = sum(cmr_baseline)
  ) |>
  mutate(
    cmr_excess = cmr - cmr_baseline,
    cmr_excess_p = (cmr / cmr_baseline) - 1
  )

pop <- ts |>
  filter(level == 8, date == 2020) |>
  select(id, population)

chart <- make_chart(ts_plot |> inner_join(pop), FALSE)
save_chart(chart, "deu/sn/muni/scatter/cmr_population", upload = FALSE)
chart <- make_chart(ts_plot |> inner_join(pop), TRUE)
save_chart(chart, "deu/sn/muni/scatter/cmr_population_sign", upload = FALSE)
