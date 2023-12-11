source("./lib/common.r")

make_chart <- function(df, fc) {
    fc |> autoplot(df, level = 95) +
        theme_bw() +
        theme(legend.position = "none") +
        watermark() +
        theme(axis.text.x = element_text(
            angle = 30, hjust = 0.5, vjust = 0.5
        )) +
        scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y Jan")
}

# Read source data
df <- read.csv(
    paste0(
        "https://fred.stlouisfed.org/graph/fredgraph.csv?&id=LNU01074597&",
        "scale=left&cosd=1900-01-01&coed=", Sys.Date()
    )
) |>
    setNames(c("date", "value")) |>
    mutate(
        date = make_yearmonth(year(date), month(date)),
        value = value * 1000
    ) |>
    as_tsibble(index = date)

# Months to forecast
fc_months <- max(df$date) - make_yearmonth(year = 2020, month = 1)

# Absolute
fit <- df |>
    filter_index("2015 Jan" ~ "2019 Dec") |>
    model(TSLM(value ~ season()))
fc <- forecast(fit, h = paste0(fc_months, " months"))
chart <- make_chart(df, fc) + labs(
    title = paste0(
        "Actual & Forecasts of Civilian Labor Force - With a Disability, ",
        "16 Years and over [USA]"
    ),
    subtitle = paste0(
        "Source: fred.stlouisfed.org/series/LNU01074597",
        " · Baseline Period: 2015 Jan - 2019 Dec · 95% PI"
    ),
    x = "Month of Year",
    y = "People"
) +
    scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6))

save_chart(chart, "covid19/usa/disability_lf")

# Excess
chart <- make_excess_chart(
    df |>
        inner_join(fc, by = c("date")) |>
        mutate(excess = (value.x - .mean)) |>
        select(date, excess)
) +
    labs(
        title = paste0(
            "Change in Population - With a Disability, 16 Years and over [USA]"
        ),
        subtitle = paste0(
            "Source: fred.stlouisfed.org/series/LNU00074597",
            " · Baseline Period: 2015 Jan - 2019 Dec"
        ),
        x = "Month of Year",
        y = "People"
    )
save_chart(chart, "covid19/usa/disability_lf_excess")

# Rate
pop <- read_csv("https://s3.mortality.watch/data/population/usa/5y.csv")
pop_16_plus <- pop |>
    filter(iso3c == "USA", !age_group %in% c("0-4", "5-9", "10-14", "all")) |>
    group_by(year) |>
    summarize(population = sum(population))
ts <- df |>
    mutate(year = year(date)) |>
    inner_join(pop_16_plus) |>
    mutate(rate = value / population * 1000) |>
    select(date, rate, population)
fit <- ts |>
    filter_index("2015 Jan" ~ "2019 Dec") |>
    model(TSLM(rate ~ season()))
fc <- forecast(fit, h = paste0(fc_months, " months"))

chart <- make_chart(ts, fc) + labs(
    title = paste0(
        "Actual & Forecasts of Civilian Labor Force Rate - With a ",
        "Disability, 16 Years and over [USA]"
    ),
    subtitle = paste0(
        "Source: fred.stlouisfed.org/series/LNU01074597",
        " · Baseline Period: 2015 Jan - 2019 Dec · ",
        "95% PI"
    ),
    x = "Month of Year",
    y = "Disabled per 1,000 people"
)

save_chart(chart, "covid19/usa/disability_lf_rate")

# Excess
chart <- make_excess_chart(
    ts |>
        inner_join(fc, by = c("date")) |>
        mutate(excess = (rate.x - .mean) * population / 1000) |>
        select(date, excess)
) +
    labs(
        title = paste0(
            "Change in Population - With a Disability, 16 Years and over [USA]"
        ),
        subtitle = paste0(
            "Source: fred.stlouisfed.org/series/LNU00074597",
            " · Baseline Period: 2015 Jan - 2019 Dec · Adj. for pop. growth"
        ),
        x = "Month of Year",
        y = "People"
    )
save_chart(chart, "covid19/usa/disability_lf_excess_adj")
