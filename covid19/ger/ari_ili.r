source("lib/common.r")

df <- read_tsv(
    paste0(
        "https://raw.githubusercontent.com/robert-koch-institut/",
        "GrippeWeb_Daten_des_Wochenberichts/main/",
        "GrippeWeb_Daten_des_Wochenberichts.tsv"
    )
)

# ARI
ari_all <- df |>
    filter(
        Erkrankung == "ARE",
        Altersgruppe == "00+",
        Region == "Bundesweit"
    ) |>
    mutate(
        date = make_yearweek(
            year = as.integer(left(Kalenderwoche, 4)),
            week = as.integer(right(Kalenderwoche, 2))
        ),
        incidence = as.integer(Inzidenz)
    ) |>
    select(date, incidence) |>
    as_tsibble(index = date)

# ILI
ili_all <- df |>
    filter(
        Erkrankung == "ILI",
        Altersgruppe == "00+",
        Region == "Bundesweit"
    ) |>
    mutate(
        date = make_yearweek(
            year = as.integer(left(Kalenderwoche, 4)),
            week = as.integer(right(Kalenderwoche, 2))
        ),
        incidence = as.integer(Inzidenz)
    ) |>
    select(date, incidence) |>
    as_tsibble(index = date)

# Plot
fc_start_week <- make_yearweek(year = 2020, week = 11)
fc_weeks <- max(ili_all$date) - fc_start_week

## ARI
fit_ari <- ari_all |>
    filter(date < make_yearweek(year = 2020, week = 11)) |>
    model(TSLM(incidence ~ season()))
fc_ari <- forecast(fit_ari, h = paste0(as.numeric(fc_weeks), " weeks"))
chart <- fc_ari |> autoplot(ari_all, level = 99.9) +
    labs(
        title = "Actual & Forecasts of Weekly ARI Incidence [Germany]",
        subtitle = paste0(
            "Source: RKI/GrippeWeb; ARI = Acute Respiratory ",
            "Illness; Training Period: 2011 W22 - 2020 W10; ",
            "99.9% PI"
        ),
        x = "Week of Year",
        y = "Rate per 100k population"
    ) +
    theme_bw() +
    theme(legend.position = "none") +
    watermark(min(ili_all$date) + 52 * 2.3, 400) +
    theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
    scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y W01") +
    scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3)) +
    coord_cartesian(ylim = c(0, max(ari_all$incidence)))
save_chart(chart, "covid19/ger/ari")

## ILI
fit_ili <- ili_all |>
    filter(date < make_yearweek(year = 2020, week = 11)) |>
    model(TSLM(incidence ~ season()))
fc_ili <- forecast(fit_ili, h = paste0(as.numeric(fc_weeks), " weeks"))
chart <- fc_ili |> autoplot(ili_all, level = 99.9) +
    labs(
        title = "Actual & Forecasts of Weekly ILI Incidence [Germany]",
        subtitle = paste0(
            "Source: RKI/GrippeWeb; ILI = Influenza Like ",
            "Illness; Training Period: 2011 W22 - 2020 W10; ",
            "99.9% PI"
        ),
        x = "Week of Year",
        y = "Rate per 100k population"
    ) +
    theme_bw() +
    theme(legend.position = "none") +
    watermark(min(ili_all$date) + 52 * 2.3, 400) +
    theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
    scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y W01") +
    scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3)) +
    coord_cartesian(ylim = c(0, max(ili_all$incidence)))
save_chart(chart, "covid19/ger/ili")
