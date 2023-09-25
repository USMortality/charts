source("lib/common.r")

# Weekly 2017+
wd_usa <- read_remote("deaths/usa/age_weekly_2015-n.csv") |>
    filter(year >= 2017) |> # Totals are only available from 2017.
    mutate(date = date_parse(paste(year, week, 1), format = "%G %V %u")) |>
    filter(!is.na(deaths))

md_usa_10y <- read_remote("deaths/usa/monthly_10y_complete.csv") |>
    mutate(date = date_parse(paste(year, month, 1), format = "%Y %m %d")) |>
    aggregate_80_plus()

weekly <- wd_usa |>
    filter(age_group == "all") |>
    get_daily_from_weekly("deaths")
monthly <- md_usa_10y |>
    filter(age_group == "all") |>
    get_daily_from_monthly("deaths")

df <- weekly |>
    inner_join(monthly, by = c("iso3c", "date")) |>
    select(iso3c, date, deaths.x, deaths.y)

df |>
    filter(iso3c == "USA") |>
    ggplot() +
    geom_line(aes(x = date, y = deaths.x, color = "red")) +
    geom_line(aes(x = date, y = deaths.y, color = "blue"))

result <- df |>
    mutate(year = year(date)) |>
    group_by(iso3c, year) |>
    summarize(weekly = sum(deaths.x), monthly = sum(deaths.y)) |>
    mutate(diff = weekly - monthly) |>
    mutate(diff_p = diff / weekly)

save_csv(result, "deaths/usa/weekly_v_monthly")
