source("lib/common.r")

# Download Data
url <- paste0(
  "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/",
  "EXCEL_FILES/1_General/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1",
  ".xlsx"
)
download.file(
  url, "./data/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx"
)

# Load Data
world_population <- read_excel(
  "./data/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx",
  sheet = "Estimates",
  col_types = c(
    "text", "text", "text", "text", "numeric", "numeric", "numeric"
  ),
  range = cell_cols(6:12)
)
world_deaths <- as_tibble(read.csv(
  "https://github.com/akarlinsky/world_mortality/raw/main/world_mortality.csv"
))

# Population
population <- world_population %>%
  select(1, 6, 7) %>%
  setNames(c("ISO3", "year", "population")) %>%
  filter(year >= 2015)

all_countries <- world_deaths %>%
  filter(year == 2015)

for (country in unique(all_countries$iso3c)) {
  df <- all_countries %>%
    filter(iso3c == country)
  country_name <- unique(df$country_name)

  print(paste(country_name, country, sep = "/"))

  population_country <- as_tsibble(
    population %>% filter(ISO3 == country),
    index = year
  )

  if (length(population_country$ISO3) == 0) {
    print(paste0("no population found for ", country))
    next
  }

  # forecast 2022
  model <- lm(
    population ~ year,
    data = population_country %>% filter(year >= 2020)
  )
  y_22 <- predict(
    model,
    data = population_country,
    newdata = data.frame(year = c(2022))
  )
  population_country <- population_country %>%
    add_row(ISO3 = country, year = 2022, population = y_22)

  # Deaths
  country_deaths <- world_deaths %>%
    filter(iso3c == country)

  time_unit <- unique(country_deaths$time_unit)
  if (length(time_unit) != 1) {
    stop("Different time_units detected.")
  }
  if (time_unit == "weekly") {
    deaths <- country_deaths %>%
      mutate(date = paste0(
        year, "-", str_pad(time, 2, side = "left", "0"), "-1"
      )) %>%
      mutate(date = date_parse(date, format = "%G-%V-%u")) %>%
      tsibble(index = date)
  }
  if (time_unit == "monthly") {
    deaths <- country_deaths %>%
      mutate(date = make_yearmonth(year = year, month = time)) %>%
      tsibble(index = date)
  }

  # Join deaths/population
  mortality <- deaths %>%
    inner_join(population_country, by = "year") %>%
    mutate(mortality = deaths / population * 100) %>%
    mutate(year = as.integer(year)) %>%
    fill_gaps(mortality = 0)

  # Charts
  ## Weekly Mortality
  df <- mortality %>%
    select(date, mortality)
  save_csv(df, paste("mortality", country_name, "weekly", sep = "/"))

  chart1 <-
    ggplot(as_tsibble(df, index = date), aes(x = date, y = mortality)) +
    labs(
      title = paste0("Weekly Mortality [", country_name, "]"),
      subtitle = "Datasources: United Nations, World Mortality Dataset",
      y = "Deaths/100k",
      x = ifelse(time_unit == "weekly", "Week of Year", "Month of Year")
    ) +
    geom_line(color = "#5383EC", linewidth = 1) +
    twitter_theme() +
    watermark(df$yearmonth, df$value_p)
  save_chart(chart1, paste("mortality", country_name, "1", sep = "/"))

  ## STL decomp
  data <- mortality %>%
    model(STL(mortality)) %>%
    components()
  chart2 <- autoplot(data, .vars = mortality) +
    labs(
      title = paste0(
        "Weekly Mortality - STL Decomposition [",
        country_name,
        "]"
      ),
      subtitle = "Datasources: United Nations, World Mortality Dataset",
      y = "Deaths/100k",
      x = ifelse(time_unit == "weekly", "Week of Year", "Month of Year")
    ) +
    twitter_theme() +
    watermark(df$yearmonth, df$value_p)
  save_chart(
    chart2,
    paste("mortality", country_name, "2", sep = "/")
  )

  ## Yearly Mortality
  data <- mortality %>%
    filter(!is.na(year)) %>%
    group_by_key() %>%
    index_by(year) %>%
    summarise(
      mortality = sum(mortality)
    )

  ### Calculate Baseline & Excess w/o outliers
  data_bl <- data %>%
    filter(year <= 2019) %>%
    subset(!(mortality %in% boxplot.stats(mortality)$out))
  model <- lm(y ~ x, data = data.frame(x = data_bl$year, y = data_bl$mortality))
  forecast <- predict(
    model,
    newdata = data.frame(x = seq(2015, max(data$year)))
  )
  fc_df <- data.frame(year = seq(2015, max(data$year)), baseline = forecast)
  data <- data %>%
    inner_join(fc_df, by = "year") %>%
    mutate(excess = mortality - baseline) %>%
    mutate(excess_percent = excess / baseline)

  chart3a <- ggplot(data, aes(x = year, y = mortality)) +
    labs(
      title = paste0("Mortality [", country_name, "]"),
      subtitle = paste0(
        "Baseline: ", toString(data_bl$year), "; ",
        "Source: UN, World Mortality Dataset"
      ),
      y = "Deaths/100k",
      x = "Year"
    ) +
    twitter_theme() +
    watermark(df$yearmonth, df$value_p) +
    geom_col(fill = "#5383EC") +
    geom_abline(
      intercept = model$coefficients[1],
      slope = model$coefficients[2],
      linetype = "dashed",
      colour = "#00000077",
      size = 1
    ) +
    geom_text(
      aes(label = round(mortality, 1)),
      vjust = 2.5, colour = "#ffffff"
    ) +
    geom_text(
      aes(label = percent(excess_percent, accuracy = 0.1)),
      vjust = -.2
    ) +
    scale_y_continuous(labels = comma_format(decimal.mark = ","))

  save_chart(chart3a, paste("mortality", country_name, "3a", sep = "/"))

  chart3b <- ggplot(data, aes(x = year, y = mortality)) +
    labs(
      title = paste0("Mortality [", country_name, "]"),
      subtitle = paste0(
        "Baseline: ", toString(data_bl$year), "; ",
        "Source: UN, World Mortality Dataset"
      ),
      y = "Deaths/100k",
      x = "Year"
    ) +
    twitter_theme() +
    watermark(df$yearmonth, df$value_p) +
    geom_point() +
    geom_abline(
      intercept = model$coefficients[1],
      slope = model$coefficients[2],
      linetype = "dashed",
      colour = "#00000077",
      size = 1
    ) +
    geom_text(
      aes(label = round(mortality, 1)),
      vjust = 1.5,
      colour = "#00000099"
    ) +
    geom_text(
      aes(label = percent(excess_percent, accuracy = 0.1)),
      vjust = -.6,
      colour = "#0000bb"
    ) +
    scale_y_continuous(labels = comma_format(decimal.mark = ","))
  save_chart(chart3b, paste("mortality", country_name, "3b", sep = "/"))

  ## YTD Mortality
  date <- mortality %>%
    filter(!is.na(year)) %>%
    filter(year == max(year))
  max_week <- max(date$time)

  data <- mortality %>%
    filter(!is.na(year)) %>%
    filter(time <= max_week) %>%
    group_by_key() %>%
    index_by(year) %>%
    summarise(
      mortality = sum(mortality)
    )
  # Calculate Baseline & Excess w/o outliers
  data_bl <- data %>%
    filter(year <= 2019) %>%
    subset(!(mortality %in% boxplot.stats(mortality)$out))
  model <- lm(y ~ x, data = data.frame(x = data_bl$year, y = data_bl$mortality))
  forecast <- predict(
    model,
    newdata = data.frame(x = seq(2015, max(data$year)))
  )
  fc_df <- data.frame(year = seq(2015, max(data$year)), baseline = forecast)
  data <- data %>%
    inner_join(fc_df, by = "year") %>%
    mutate(excess = mortality - baseline) %>%
    mutate(excess_percent = excess / baseline)

  chart4a <- ggplot(data, aes(x = year, y = mortality)) +
    labs(
      title = paste0(
        "YTD Mortality (",
        ifelse(time_unit == "weekly", "Week", "Month"),
        " 1-", max_week, ") [", country_name, "]"
      ),
      subtitle = paste0(
        "Baseline: ", toString(data_bl$year), "; ",
        "Source: UN, World Mortality Dataset"
      ),
      y = "Deaths/100k",
      x = "Year"
    ) +
    twitter_theme() +
    watermark(df$yearmonth, df$value_p) +
    geom_col(fill = "#5383EC") +
    geom_abline(
      intercept = model$coefficients[1],
      slope = model$coefficients[2],
      linetype = "dashed",
      colour = "#00000077",
      size = 1
    ) +
    geom_text(
      aes(label = round(mortality, 1)),
      vjust = 2.5, colour = "#ffffff"
    ) +
    geom_text(
      aes(label = percent(excess_percent, accuracy = 0.1)),
      vjust = -.2
    ) +
    scale_y_continuous(labels = comma_format(decimal.mark = ","))
  save_chart(chart4a, paste("mortality", country_name, "4a", sep = "/"))

  chart4b <- ggplot(data, aes(x = year, y = mortality)) +
    labs(
      title = paste0(
        "YTD Mortality (",
        ifelse(time_unit == "weekly", "Week", "Month"),
        " 1-", max_week, ") [", country_name, "]"
      ),
      subtitle = paste0(
        "Baseline: ", toString(data_bl$year), "; ",
        "Source: UN, World Mortality Dataset"
      ),
      y = "Deaths/100k",
      x = ifelse(time_unit == "weekly", "Week of Year", "Month of Year")
    ) +
    twitter_theme() +
    watermark(df$yearmonth, df$value_p) +
    geom_point() +
    geom_abline(
      intercept = model$coefficients[1],
      slope = model$coefficients[2],
      linetype = "dashed",
      colour = "#00000077",
      size = 1
    ) +
    geom_text(
      aes(label = round(mortality, 1)),
      vjust = 1.5,
      colour = "#00000099"
    ) +
    geom_text(
      aes(label = percent(excess_percent, accuracy = 0.1)),
      vjust = -.6,
      colour = "#0000bb"
    ) +
    scale_y_continuous(labels = comma_format(decimal.mark = ","))
  save_chart(chart4b, paste("mortality", country_name, "4b", sep = "/"))

  save_collage(
    paste("mortality", country_name, sep = "/"),
    chart1, chart3a, chart4a, chart2, chart3b, chart4b
  )
}
