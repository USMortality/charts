libs <- read.table("dependencies.txt")
for (lib in libs$V1) {
  library(lib, character.only = TRUE, quietly = TRUE)
}

sf <- 2
options(vsc.dev.args = list(width = 600 * sf, height = 335 * sf, res = 72 * sf))
charts_bucket <- "charts"
data_bucket <- "data"

watermark <- function(x, y) {
  annotate("text",
    y = Inf,
    x = structure(Inf, class = "Date"),
    label = "@USMortality",
    vjust = 1,
    hjust = 1,
    col = "#000000",
    cex = 6,
    fontface = "bold",
    alpha = 0.1
  )
}

twitter_theme <- function() {
  theme_gray() +
    theme(
      plot.title = element_text(size = 18, face = "bold", color = "#333333"),
      plot.subtitle = element_text(size = 14, color = "#999999"),
      axis.title = element_text(size = 13, color = "#999999"),
      axis.text = element_text(size = 12, color = "#000000"),
    )
}

save_csv <- function(df, name, upload = TRUE) {
  file_name <- paste0(name, ".csv")
  local_file_name <- paste0("out/", file_name)
  dir.create(dirname(local_file_name), recursive = TRUE)
  write.csv(df, local_file_name, row.names = FALSE)

  if (upload) {
    put_object(
      file = local_file_name,
      object = file_name,
      bucket = data_bucket
    )
  }
}

save_chart <- function(chart, name, scale, upload = TRUE) {
  if (missing(scale)) scale <- sf
  file_name <- paste0(name, ".png")
  local_file_name <- paste0("out/", file_name)
  dir.create(dirname(local_file_name), recursive = TRUE)

  print(paste0("Saving ", local_file_name))

  ggsave(
    filename = local_file_name,
    plot = chart,
    width = 600,
    height = 335,
    units = "px",
    scale = scale,
    dpi = 144,
    device = grDevices::png,
    type = c("cairo")
  )

  if (upload) {
    put_object(
      file = local_file_name,
      object = file_name,
      bucket = charts_bucket
    )
  }
}

left <- function(string, length) {
  substr(string, 1, length)
}

right <- function(string, length) {
  substr(string, nchar(string) - length + 1, nchar(string))
}

mid <- function(string, start, length) {
  substr(string, start, start + length - 1)
}

save_collage <- function(..., path = NULL, ncol = 2, nrow = 2, scale = 4) {
  figure <- ggarrange(..., ncol = ncol, nrow = nrow)
  save_chart(figure, path, scale = scale)
}

lm_right <- function(formula, data, ...) {
  mod <- lm(formula, data)
  class(mod) <- c("lm_right", class(mod))
  mod
}

predictdf.lm_right <- function(model, xseq, se, level) {
  ## here the main code: truncate to x values at the right
  init_range <- range(model$model$x)
  xseq <- xseq[xseq >= init_range[1]]
  ggplot2:::predictdf.default(model, xseq, se, level)
}

midyear_ <- function(date, month) {
  y <- year(date)
  if (month(date) <= month) {
    paste0(y - 1, "-", y)
  } else {
    paste0(y, "-", y + 1)
  }
}

fluseason <- function(data) {
  if (length(data) > 1) {
    sapply(data, midyear_, 9)
  } else {
    midyear_(data, 9)
  }
}

midyear <- function(data) {
  if (length(data) > 1) {
    sapply(data, midyear_, 6)
  } else {
    midyear_(data, 6)
  }
}

read_remote <- function(path) {
  as_tibble(read.csv(paste0("https://s3.mortality.watch/data/", path)))
}

sumIfNotEmpty <- function(vec) {
  if (all(is.na(vec))) {
    NA
  } else {
    sum(vec, na.rm = TRUE)
  }
}

getCountriesForType <- function(mortality_type, data_yearly, asmr_data) {
  if (mortality_type == "cmr") {
    unique(data_yearly$name)
  } else {
    unique(asmr_data$name)
  }
}

get_usa_deaths <- function(file) {
  deaths_usa <- as_tibble(read.csv(file)) |>
    mutate(year = left(Month.Code, 4), time = right(Month.Code, 2)) |>
    select(7, 8, 4)
  deaths_usa$iso3c <- "USA"
  deaths_usa$country_name <- "United States"
  deaths_usa$time_unit <- "monthly"

  deaths_usa |>
    setNames(
      c("year", "time", "deaths", "iso3c", "country_name", "time_unit")
    ) |>
    relocate(4, 5, 1, 2, 6, 3) |>
    mutate(
      year = as.numeric(year),
      time = as.numeric(time)
    )
}

get_usa_population <- function(file) {
  as_tibble(read.csv(file)) |>
    select(2, 5) |>
    setNames(c("year", "population"))
}

get_usa_mortality <- function(age_group) {
  pop_usa <- get_usa_population(
    paste0("./data_static/usa_pop_", age_group, ".csv")
  )
  deaths_usa <- get_usa_deaths(
    paste0("./data_static/usa_", age_group, ".csv")
  ) |>
    inner_join(pop_usa, by = "year") |>
    mutate(mortality = deaths / population * 100000)
  deaths_usa$age_group <- str_replace(age_group, "_", "-")

  deaths_usa |> select(3, 4, 9, 8)
}

getDailyFromN <- function(wd, column_names, fun) {
  df <- wd |>
    uncount(fun(date), .id = "day") |>
    mutate(date = date(date)) |>
    mutate(date = date + days(day - 1))

  for (column_name in column_names) {
    col <- sym(column_name)
    df <- df |> mutate("{column_name}" := !!col / fun(date))
  }

  df |> select(-ncol(df))
}

getDailyFromWeekly <- function(wd, column_names) {
  getDailyFromN(wd, column_names, function(date) {
    7
  })
}

getDailyFromMonthly <- function(wd, column_names) {
  getDailyFromN(wd, column_names, function(date) {
    days_in_month(date)
  })
}

getDailyFromYearly <- function(wd, column_names) {
  getDailyFromN(wd, column_names, function(date) {
    y <- year(date)
    x <- lubridate::interval(paste0(y, "-01-01"), paste0(y, "-12-31"))
    x %/% days(1) + 1
  })
}

# Forecast n+5
forecast_population <- function(data) {
  fc_n <- 5
  y <- data |>
    as_tsibble(index = year) |>
    tail(n = 5) |>
    model(NAIVE(population ~ drift())) |>
    forecast(h = fc_n)

  last_available_year <- data$year[length(data$year)]
  data$is_projection <- FALSE
  for (i in 1:fc_n) {
    data <- data |> add_row(
      year = as.integer(last_available_year + i),
      population = as.integer(y$.mean[i]),
      is_projection = TRUE
    )
  }

  data
}

interpolate_population <- function(df) {
  df |>
    as_tsibble(index = date) |>
    fill_gaps() |>
    mutate(population = zoo::na.approx(population)) |>
    as_tibble()
}

check_duplicates <- function(df) {
  for (code in unique(df$iso3c)) {
    df_country <- df |> filter(df$iso3c == code)
    for (ag in unique(df_country$age_group)) {
      df_age <- df_country |> filter(df_country$age_group == ag)
      duplicated <- is_duplicated(df_age, index = date)
      if (duplicated) {
        print(
          paste0(
            "iso3c: ", code,
            " | age_group: ", ag,
            " | Duplicates: ", duplicated
          )
        )
        print(duplicates(df_age, index = date))
      }
      ts <- df_age |> as_tsibble(index = date)
    }
  }
}

print_info <- function(df) {
  for (code in unique(df$iso3c)) {
    df_country <- df |> filter(df$iso3c == code)
    print(paste0(
      code, " | dates: ",
      min(df_country$date), " to ", max(df_country$date), " | age_groups: ",
      paste(unique(df_country$age_group), collapse = ", ")
    ))
  }
}
