source("lib/common.r")

# Define default functions
select <- dplyr::select
filter <- dplyr::filter
mutate <- dplyr::mutate
group_by <- dplyr::group_by
ungroup <- dplyr::ungroup
summarise <- dplyr::summarise
inner_join <- dplyr::inner_join
relocate <- dplyr::relocate
year <- lubridate::year
month <- lubridate::month
week <- lubridate::week
days <- lubridate::days
days_in_month <- lubridate::days_in_month
as_tibble <- tibble::as_tibble
tibble <- tibble::tibble
as_tsibble <- tsibble::as_tsibble
str_replace <- stringr::str_replace
uncount <- tidyr::uncount
sym <- rlang::sym
model <- fabletools::model
date <- lubridate::date
forecast <- fabletools::forecast
select <- dplyr::select
all_of <- dplyr::all_of
nest <- tidyr::nest
unnest <- tidyr::unnest
.data <- dplyr::.data
yearmonth <- tsibble::yearmonth
yearweek <- tsibble::yearweek

get_data <- function(from, to) {
  req <- httr::POST("https://api.bls.gov/publicAPI/v2/timeseries/data/",
    httr::add_headers("Content-Type" = "application/json"),
    body = paste0(
      '{"seriesid": ["CUUR0000SA0"], "startyear":"',
      from,
      '", "endyear":"',
      to,
      '"}'
    )
  )
  httr::stop_for_status(req)
  data <- httr::content(req, "text") |>
    jsonlite::fromJSON()

  as_tibble(data$Results$series$data[[1]]) |>
    mutate(year = as.integer(year)) |>
    mutate(period = as.integer(right(.data$period, 2))) |>
    mutate(value = as.double(.data$value)) |>
    mutate(yearmonth = tsibble::yearmonth(paste0(year, "-", period))) |>
    mutate(value_ref = dplyr::lead(value, 12)) |>
    mutate(value_p = value / .data$value_ref - 1) |>
    filter(!is.na(value_ref)) |>
    select(yearmonth, value_p)
}

current_year <- year(Sys.Date())
#  Load Data
df <- rbind(
  get_data(current_year - 9, current_year),
  get_data(current_year - 19, current_year - 10),
  get_data(current_year - 29, current_year - 20),
  get_data(current_year - 39, current_year - 30),
  get_data(current_year - 49, current_year - 40),
  get_data(current_year - 59, current_year - 50),
  get_data(current_year - 69, current_year - 60),
  get_data(current_year - 79, current_year - 70),
  get_data(current_year - 89, current_year - 80),
  get_data(current_year - 99, current_year - 90)
)

save_csv(df, "economy/usa/inflation")

# Make Chart
chart <- ggplot(
  as_tsibble(df, index = yearmonth),
  aes(x = yearmonth, y = value_p)
) +
  labs(
    title = "Inflation Rate [USA]",
    subtitle = "Source: bls.gov",
    x = "Month of Year",
    y = "12M Rate of Increase"
  ) +
  geom_line(color = "#5383EC", linewidth = 1.5) +
  geom_hline(yintercept = 0) +
  geom_hline(aes(yintercept = 0.02), color = "#58A65C", linetype = "dashed") +
  twitter_theme() +
  watermark() +
  scale_y_continuous(labels = scales::percent) +
  ggrepel::geom_label_repel(
    data = head(df, n = 1) |> mutate(str = paste0(
      yearmonth,
      ": ",
      sprintf("%0.1f%%", value_p * 100)
    )),
    aes(label = str),
    nudge_y = 0.1,
    segment.color = "grey50",
  )

save_chart(chart, "economy/usa/inflation")
