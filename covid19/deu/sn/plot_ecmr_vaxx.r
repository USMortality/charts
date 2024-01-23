# Vaccinations
data <- read.csv("./data_static/covid19_deu_vaccinations.csv")

make_chart <- function(y, by_dose) {
  if (by_dose) {
    vaxx <- as_tibble(data) |>
      setNames(c("date", "id", "age_group", "dose", "count")) |>
      mutate(date = date_parse(date, format = "%F")) |>
      group_by(id, dose) |>
      summarize(count = sum(count))
  } else {
    vaxx <- as_tibble(data) |>
      setNames(c("date", "id", "age_group", "dose", "count")) |>
      filter(dose == 1) |>
      mutate(date = date_parse(date, format = "%F")) |>
      group_by(id) |>
      summarize(count = sum(count))
  }

  county_excess <- ts |>
    filter(level == 5, date %in% y, ) |>
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
    filter(level == 5, date %in% y) |>
    select(id, population) |>
    group_by(id) |>
    summarize(population = mean(population))

  ts_plot <- county_excess |> inner_join(
    vaxx |> inner_join(pop) |> mutate(vaxxed = count / population)
  )

  chart <- ggplot(ts_plot, aes(x = vaxxed, y = cmr_excess_p)) +
    stat_correlation(use_label(c("R", "P", "n", "method"))) +
    stat_poly_line() +
    geom_point() +
    labs(
      title = paste0(
        "Excess Crude Mortality Rate (eCMR) vs. COVID-19 Vaccinated by County ",
        "[Saxony, Germany]"
      ),
      subtitle = paste(y, collapse = "-"),
      x = "COVID-19 Vaccinated (1st)",
      y = "Excess Deaths/100k population"
    ) +
    theme_bw() +
    coord_trans(x = "log2") +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_y_continuous(labels = scales::percent_format()) +
    theme(legend.position = "top") +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5)) +
    geom_hline(yintercept = 0, linewidth = 0.5)

  if (by_dose) {
    chart + facet_wrap(vars(dose), scales = "free")
  } else {
    chart
  }
}

save_chart(
  make_chart(2021, FALSE),
  "deu/sn/vaccine/ecmr_2021",
  upload = FALSE
)
save_chart(
  make_chart(2022, FALSE),
  "deu/sn/vaccine/ecmr_2022",
  upload = FALSE
)
save_chart(
  make_chart(c(2021:2022), FALSE),
  "deu/sn/vaccine/ecmr_2021_2022",
  upload = FALSE
)
save_chart(
  make_chart(2021, TRUE),
  "deu/sn/vaccine/ecmr_dose_2021",
  upload = FALSE
)
save_chart(
  make_chart(2022, TRUE),
  "deu/sn/vaccine/ecmr_dose_2022",
  upload = FALSE
)
save_chart(
  make_chart(c(2021:2022), TRUE),
  "deu/sn/vaccine/ecmr_dose_2021_2022",
  upload = FALSE
)

# source("./covid19/deu/sn/plot_ecmr_vaxx.r")
