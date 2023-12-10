source("./lib/common.r")

df <- read.csv(
    paste0(
        "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&",
        "chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&",
        "height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&",
        "tts=12&width=1138&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles",
        "=yes&show_tooltip=yes&id=LNU01074597&scale=left&cosd=2008-06-01&coed=",
        Sys.Date(),
        "&line_color=%234572a7&link_values=false&line_style=solid&mark_type=",
        "none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Weekly%2C%20Ending",
        "%20Thursday&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&",
        "transformation=lin&vintage_date=",
        Sys.Date(),
        "&revision_date=",
        Sys.Date(),
        "&nd=1971-04-02"
    )
) |>
    setNames(c("date", "incidence")) |>
    mutate(date = make_yearmonth(year(date), month(date))) |>
    as_tsibble(index = date)

fit <- df |>
    filter(date < make_yearmonth(2021, 1)) |>
    model(TSLM(incidence ~ season() + trend()))
fc <- forecast(fit, h = "36 months")
chart <- fc |> autoplot(df, level = 95) +
    labs(
        title = paste0(
            "Actual & Forecasts of Civilian Labor Force - With a Disability, ",
            "16 Years and over [USA]"
        ),
        subtitle = paste0(
            "Source: fred.stlouisfed.org/series/LNU01074597 ",
            "; Baseline Period: 2008 Jun - 2020 Dec; ",
            "95% PI"
        ),
        x = "Month of Year",
        y = "Thousands of Persons"
    ) +
    theme_bw() +
    theme(legend.position = "none") +
    watermark() +
    theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
    scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y W01")
# + coord_cartesian(ylim = c(0, max(df$incidence)))
save_chart(chart, "covid19/usa/disability")

# By year
pop <- read_csv("https://s3.mortality.watch/data/population/usa/5y.csv")
pop_16_plus <- pop |>
    filter(!age_group %in% c("0-4", "5-9", "10-14", "all")) |>
    group_by(year) |>
    summarize(population = sum(population))

ts <- df |>
    mutate(year = year(date)) |>
    inner_join(pop_16_plus) |>
    mutate(rate = incidence / population * 100000) |>
    select(date, rate)

fit <- ts |>
    filter(date < make_yearmonth(2021, 1)) |>
    model(TSLM(rate ~ season() + trend()))
fc <- forecast(fit, h = "36 months")
chart <-
    fc |> autoplot(ts, level = 95) +
    labs(
        title = paste0(
            "Actual & Forecasts of Civilian Labor Force Rate - With a ",
            "Disability, 16 Years and over [USA]"
        ),
        subtitle = paste0(
            "Source: fred.stlouisfed.org/series/LNU01074597 ",
            "; Baseline Period: 2008 Jun - 2020 Dec; ",
            "95% PI"
        ),
        x = "Month of Year",
        y = "Disabled per 100k Population"
    ) +
    theme_bw() +
    theme(legend.position = "none") +
    watermark() +
    theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
    scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y W01")
# + coord_cartesian(ylim = c(0, max(ts$rate)))

save_chart(chart, "covid19/usa/disability_rate")
