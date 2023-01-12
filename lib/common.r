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
      bucket = data_bucket,
      multipart = TRUE
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

fluseason_ <- function(date) {
  x <- date
  month(x) <- 10
  day(x) <- 1
  if (date >= x) {
    paste0(year(date), "-", year(date) + 1)
  } else {
    paste0(year(date) - 1, "-", year(date))
  }
}

fluseason2_ <- function(date) {
  y <- year(date)
  if (month(date) <= 9) {
    paste0(y - 1, "-", y)
  } else {
    paste0(y, "-", y + 1)
  }
}

fluseason <- function(data) {
  if (length(data) > 1) {
    sapply(data, fluseason2_)
  } else {
    fluseason2_(data)
  }
}

read_remote <- function(path) {
  as_tibble(read.csv(paste0("https://s3.mortality.watch/data/", path)))
}

get_usa_deaths <- function(file) {
  deaths_usa <- as_tibble(read.csv(file)) %>%
    mutate(year = left(Month.Code, 4), time = right(Month.Code, 2)) %>%
    select(7, 8, 4)
  deaths_usa$iso3c <- "USA"
  deaths_usa$country_name <- "United States"
  deaths_usa$time_unit <- "monthly"

  deaths_usa %>%
    setNames(
      c("year", "time", "deaths", "iso3c", "country_name", "time_unit")
    ) %>%
    relocate(4, 5, 1, 2, 6, 3) %>%
    mutate(
      year = as.numeric(year),
      time = as.numeric(time)
    )
}

get_usa_population <- function(file) {
  as_tibble(read.csv(file)) %>%
    select(2, 5) %>%
    setNames(c("year", "population"))
}

get_usa_mortality <- function(age_group) {
  pop_usa <- get_usa_population(
    paste0("./data_static/usa_pop_", age_group, ".csv")
  )
  deaths_usa <- get_usa_deaths(
    paste0("./data_static/usa_", age_group, ".csv")
  ) %>%
    inner_join(pop_usa, by = "year") %>%
    mutate(mortality = deaths / population * 100000)
  deaths_usa$age_group <- str_replace(age_group, "_", "-")

  deaths_usa %>% select(3, 4, 9, 8)
}

getDailyFromN <- function(wd, column_name, fun) {
  col <- sym(column_name)
  df <- wd %>%
    uncount(fun(date), .id = "day") %>%
    mutate(date = date(date)) %>%
    mutate(date = date + days(day - 1)) %>%
    mutate("{column_name}" := !!col / fun(date))
  df %>% select(-ncol(df))
}

getDailyFromWeekly <- function(wd, column_name) {
  getDailyFromN(wd, column_name, function(date) {
    7
  })
}

getDailyFromMonthly <- function(wd, column_name) {
  getDailyFromN(wd, column_name, function(date) {
    days_in_month(date)
  })
}

getDailyFromYearly <- function(wd, column_name) {
  getDailyFromN(wd, column_name, function(date) {
    y <- year(date)
    x <- interval(paste0(y, "-01-01"), paste0(y, "-12-31"))
    x %/% days(1) + 1
  })
}
