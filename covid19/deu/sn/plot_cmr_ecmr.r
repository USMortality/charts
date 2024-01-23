# Plot CMR & eCMR by jurisdiction id
jurisdictions <- list(
  state = unique((ts |> filter(level == 2))$id),
  nuts2 = unique((ts |> filter(level == 3))$id),
  county = unique((ts |> filter(level == 5))$id),
  muni = unique((ts |> filter(level == 8))$id)
)

for (key in names(jurisdictions)) {
  ids <- jurisdictions[[key]]
  for (i in ids) {
    ts_plot <- ts |> filter(id == i, !is.na(date))
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
