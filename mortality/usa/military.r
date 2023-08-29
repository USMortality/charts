source("lib/common.r")
options(warn = 1)

# National
df1 <- read_excel(
    "./data_static/activeDutyDeathNo.xls",
    sheet = "ActiveDutyDeathNo",
    range = "A6:N50"
)

df2 <- df1 |>
    select(Year, Deaths, Duty) |>
    setNames((c("date", "deaths", "population"))) |>
    mutate(cmr = deaths / population * 100000)


ts <- df2 |>
    filter(date > 2013) |>
    as_tsibble(index = date)

ggplot(ts, aes(x = date, y = cmr)) +
    labs(
        title = "US Military Mortality Rate",
        subtitle = "CMR; Deaths / Active Duty; Source: www.mortality.watch",
        y = "Deaths/100k Active Duty",
        x = "Year"
    ) +
    twitter_theme() +
    geom_col(fill = "#5383EC") +
    geom_text(
        aes(label = round(cmr)),
        size = 3, vjust = 2.5, colour = "#ffffff"
    ) +
    scale_x_continuous(breaks = ts$date) +
    coord_cartesian(ylim = c(50, 80)) +
    theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))\