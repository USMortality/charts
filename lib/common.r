# group_by, "drop_last" by default
options(dplyr.summarise.inform = FALSE)
options(warn = 2)
options(readr.show_col_types = FALSE)

upload_files <- TRUE

libs <- read.table("dependencies_r.txt")
for (lib in libs$V1) {
  library(lib, character.only = TRUE, quietly = TRUE)
}

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
ggplot <- ggplot2::ggplot
make_yearmonth <- tsibble::make_yearmonth
arrange <- dplyr::arrange
distinct <- dplyr::distinct
complete <- tidyr::complete
case_when <- dplyr::case_when
across <- dplyr::across

sf <- 2
options(vsc.dev.args = list(width = 600 * sf, height = 335 * sf, res = 72 * sf))
charts_bucket <- "charts"
data_bucket <- "data"

watermark <- function(
    x = structure(Inf, class = "Date"),
    y = Inf,
    latest = "") {
  ggplot2::annotate("text",
    y = y,
    x = x,
    label = paste("@USMortality", latest),
    vjust = 1,
    hjust = 1,
    col = "#000000",
    cex = 6,
    fontface = "bold",
    alpha = 0.1
  )
}

twitter_theme <- function() {
  ggplot2::theme_gray() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 18, face = "bold", color = "#333333"
      ),
      plot.subtitle = ggplot2::element_text(size = 14, color = "#999999"),
      axis.title = ggplot2::element_text(size = 13, color = "#999999"),
      axis.text = ggplot2::element_text(size = 12, color = "#000000"),
    )
}

save_csv <- function(df, name, upload = upload_files) {
  file_name <- paste0(name, ".csv")
  local_file_name <- paste0("out/", file_name)
  if (!dir.exists(dirname(local_file_name))) {
    dir.create(dirname(local_file_name), recursive = TRUE)
  }
  write.csv(df, local_file_name, na = "", row.names = FALSE)

  if (upload) {
    upload_csv(name)
  }
}

write_csv <- function(df, name, append = FALSE) {
  file_name <- paste0(name, ".csv")
  local_file_name <- paste0("out/", file_name)
  if (!dir.exists(dirname(local_file_name))) {
    dir.create(dirname(local_file_name), recursive = TRUE)
  }
  write.table(
    df,
    local_file_name,
    append = FALSE,
    sep = ",",
    na = "",
    col.names = TRUE,
    row.names = FALSE
  )
}

upload_csv <- function(name) {
  file_name <- paste0(name, ".csv")
  local_file_name <- paste0("out/", file_name)
  aws.s3::put_object(
    file = local_file_name,
    object = file_name,
    bucket = data_bucket
  )
}

save_chart <- function(chart, name, scale, upload = upload_files) {
  if (missing(scale)) scale <- sf
  file_name <- paste0(name, ".png")
  local_file_name <- paste0("out/", file_name)
  if (!dir.exists(dirname(local_file_name))) {
    dir.create(dirname(local_file_name), recursive = TRUE)
  }

  print(paste0("Saving ", local_file_name))

  ggplot2::ggsave(
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
    aws.s3::put_object(
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
  figure <- ggpubr::ggarrange(..., ncol = ncol, nrow = nrow)
  save_chart(figure, path, scale = scale)
}

lm_right <- function(formula, data, ...) {
  mod <- lm(formula, data)
  class(mod) <- c("lm_right", class(mod))
  mod
}

predictdf.lm_right <- function(
    # nolint: object_name_linter.
    model,
    xseq,
    se,
    level) { # nolint: object_name_linter.
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

sum_if_not_empty <- function(vec) {
  if (all(is.na(vec))) {
    NA
  } else {
    sum(vec, na.rm = TRUE)
  }
}

get_countries_for_type <- function(mortality_type, data_yearly, asmr_data) {
  if (mortality_type == "cmr") {
    unique(data_yearly$name)
  } else {
    unique(asmr_data$name)
  }
}

get_usa_deaths <- function(file) {
  deaths_usa <- as_tibble(read.csv(file)) |>
    mutate(year = left("Month.Code", 4), time = right("Month.Code", 2)) |>
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
    mutate(mortality = "deaths" / "population" * 100000)
  deaths_usa$age_group <- str_replace("age_group", "_", "-")

  deaths_usa |> select(3, 4, 9, 8)
}

get_daily_from_n <- function(wd, columns, ex_columns, fun) { # nolint
  if (nrow(wd) == 0) {
    return(wd)
  }
  df <- wd |>
    uncount(fun(.data$date), .id = "day") |>
    mutate(date = date(.data$date)) |>
    mutate(date = .data$date + days(.data$day - 1))

  for (col in columns) {
    col <- sym(col)
    df <- df |> mutate("{col}" := !!col / fun(date)) # nolint
  }

  # Set all exclude columns, except first row to NA, for population interpol.
  for (col in ex_columns) {
    df[[col]] <- ifelse(df$day == 1, df[[col]], NA)
  }

  df |> select(-ncol(df))
}

get_daily_from_weekly <- function(wd, columns, ex_columns = c()) {
  get_daily_from_n(wd, columns, ex_columns, fun = function(date) {
    7
  })
}

get_daily_from_monthly <- function(wd, columns, ex_columns = c()) {
  get_daily_from_n(wd, columns, ex_columns, fun = function(date) {
    days_in_month(date)
  })
}

get_daily_from_yearly <- function(wd, columns, ex_columns = c()) {
  get_daily_from_n(wd, columns, ex_columns, fun = function(date) {
    y <- year(date)
    x <- lubridate::interval(paste0(y, "-01-01"), paste0(y, "-12-31"))
    x %/% days(1) + 1
  })
}

suppress_warnings <- function(.expr, .f, ...) {
  eval.parent(
    substitute(
      withCallingHandlers(.expr, warning = function(w) {
        cm <- conditionMessage(w)
        cond <- if (is.character(.f)) {
          grepl(.f, cm)
        } else {
          rlang::as_function(.f)(cm, ...)
        }
        if (cond) invokeRestart("muffleWarning")
      })
    )
  )
}

# Forecast n+5
forecast_population <- function(data) {
  fc_n <- 5
  df <- data |>
    as_tsibble(index = year) |>
    tail(n = 5)
  y <- suppress_warnings(
    df |> model(fable::TSLM(population ~ trend())) |> forecast(h = fc_n),
    "perfect fit"
  )

  last_available_year <- data$year[length(data$year)]
  data$is_projection <- FALSE
  for (i in 1:fc_n) {
    data <- data |> tibble::add_row(
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
    tsibble::fill_gaps() |>
    mutate(population = zoo::na.approx(.data$population)) |>
    as_tibble()
}

check_duplicates <- function(df) {
  for (code in unique(df$iso3c)) {
    df_country <- df |> filter(df$iso3c == code)
    for (ag in unique(df_country$age_group)) {
      df_age <- df_country |> filter(df_country$age_group == ag)
      duplicated <- tsibble::is_duplicated(df_age, index = date)
      if (duplicated) {
        print(
          paste0(
            "iso3c: ", code,
            " | age_group: ", ag,
            " | Duplicates: ", duplicated
          )
        )
        print(tsibble::duplicates(df_age, index = date))
      }
      df_age |> as_tsibble(index = date)
    }
  }
}

print_info <- function(df) {
  for (code in unique(df$iso3c)) {
    df_country <- df |> filter("iso3c" == code)
    for (t in unique(df_country$type)) {
      df_country_type <- df_country |> filter("type" == t)
      for (s in unique(df_country_type$source)) {
        df_country_type_source <- df_country_type |> filter(source == s)
        print(paste0(
          code,
          " | type: ", t,
          " | source: ", s,
          " | dates: ",
          min(df_country_type_source$date), " to ",
          max(df_country_type_source$date), " | age_groups: ",
          paste(unique(df_country_type_source$age_group), collapse = ", ")
        ))
      }
    }
  }
}

first_pct <- function(df) {
  sprintf("%0.1f%%", head(df, n = 1) * 100)
}

last_pct <- function(df) {
  sprintf("%0.1f%%", tail(df, n = 1) * 100)
}

as_usd <- function(x) {
  scales::dollar(x)
}

first_usd <- function(df) {
  scales::dollar(head(df, n = 1))
}

last_usd <- function(df) {
  scales::dollar(tail(df, n = 1))
}

is_double <- function(num) {
  grepl("^[0-9]+.[0-9]+$", num)
}

as_double <- function(num) {
  if (is_double(num)) {
    as.double(num)
  } else {
    NA
  }
}

is_integer <- function(num) {
  grepl("^[0-9]+$", num)
}

as_integer <- function(num) {
  if (is_integer(num)) {
    as.integer(num)
  } else {
    NA
  }
}

aggregate_80_plus <- function(df) {
  df |>
    mutate(
      age_group = case_when(
        age_group %in% c("80-89", "90+") ~ "80+",
        .default = age_group
      )
    ) |>
    group_by(across(-deaths)) |>
    summarise(deaths = sum(.data$deaths)) |>
    ungroup()
}

smart_round <- function(x) {
  y <- floor(x)
  indices <- tail(order(x - y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  as.integer(y)
}

complete_single_na <- function(
    df, df2, col, groups, guard_limit = 9, should_guard = TRUE) {
  # Debug
  # print(paste(unique(df$iso3c), unique(df$year), unique(df$age_group)))

  # No need for imputation or too many.
  if (sum(is.na(df[[col]])) != 1) {
    return(df |> select(-all_of(groups)))
  }

  # Filter the reference.
  total <- df2
  for (group in groups) {
    total <- total |> filter(.data[[group]] == unique(df[[group]]))
  }

  # Find sum target.
  if (nrow(total) == 0 || is.na(total[[col]])) {
    return(df |> select(-all_of(groups)))
  }
  remainder <- total[[col]] - sum(df[[col]], na.rm = TRUE)
  # Guard against wrong result.
  if (should_guard && remainder < 0 || remainder > guard_limit) {
    msg <- paste("completed value is out of valid range:", remainder)
    for (group in groups) {
      msg <- paste(msg, ", ", group, ": ", df[[group]])
    }
    stop(msg)
  }
  if (!should_guard && remainder < 0) {
    print(paste(
      unique(df$fips_state),
      unique(df$year),
      unique(df$age_group)
    ))
    remainder <- 0
  }

  # Store completed result including comment.
  if (!"comment" %in% colnames(df)) {
    df$comment <- NA
  }
  new_comment <- paste0(col, "__completed_total")
  df <- df |> mutate(comment = ifelse(is.na(!!sym(col)),
    ifelse(is.na(comment),
      new_comment,
      paste(comment, new_comment, sep = ",")
    ),
    comment
  ))
  df[[col]][is.na(df[[col]])] <- remainder

  return(df |> select(-all_of(groups)))
}

complete_from_aggregate <- function(
    df, df2, col, aggregate_group, target_groups, groups) {
  # Debug
  # print(paste(unique(df$year), unique(df$fips)))

  df_ag <- df |> filter(.data$age_group %in% target_groups)
  # No NA or more than 1 NA
  if (sum(is.na(df_ag[[col]])) == 0 || sum(is.na(df_ag[[col]])) > 1) {
    return(df |> select(-all_of(groups)))
  }

  # Filter the reference.
  total <- df2
  for (group in groups) {
    total <- total |> filter(.data[[group]] == unique(df[[group]]))
  }
  total <- total |> filter(.data$age_group == aggregate_group)

  # Find sum target.
  if (nrow(total) == 0 || is.na(total[[col]])) {
    return(df |> select(-all_of(groups)))
  }
  remainder <- total[[col]] - sum(df_ag[[col]], na.rm = TRUE)

  # Guard against wrong result.
  if (remainder < 0 || remainder > 9) {
    stop(paste(
      "completed value is invalid:",
      remainder, unique(df$iso3c), unique(df$date)
    ))
  }

  # Store completed result including comment.
  if (!"comment" %in% colnames(df)) {
    df$comment <- NA
  }
  new_comment <- paste0(col, "__completed_aggregate")
  position <- df$age_group %in% df_ag$age_group & is.na(df[[col]])
  old_comment <- df$comment[position]
  df$comment[position] <- ifelse(is.na(old_comment),
    new_comment,
    paste(old_comment, new_comment, sep = ",")
  )
  df[[col]][position] <- remainder

  return(df |> select(-all_of(groups)))
}

impute_weighted_sum <- function(df, df2, nom_col, denom_col, groups) {
  # Debug
  # print(paste(unique(df$year), unique(df$fips_state), unique(df$age_group)))

  na_rows <- sum(is.na(df[[nom_col]]))
  # No need for imputation.
  if (na_rows == 0) {
    return(df |> select(-all_of(groups)))
  }

  # Filter the reference.
  total <- df2
  for (group in groups) {
    total <- total |> filter(.data[[group]] == unique(df[[group]]))
  }

  # Find sum target.
  if (nrow(total) == 0 || is.na(total[[nom_col]])) {
    return(df |> select(-all_of(groups)))
  }
  remainder <- total[[nom_col]] - sum(df[[nom_col]], na.rm = TRUE)

  # Guard against wrong result.
  if (remainder < 0 || remainder > na_rows * 9) {
    msg <- paste("completed value is out of valid range:", remainder)
    for (group in groups) {
      msg <- paste(msg, ", ", group, ": ", df[[group]])
    }
    stop(msg)
  }

  # Spread remainder based on denom_col
  if (!"comment" %in% colnames(df)) {
    df$comment <- NA
  }
  new_comment <- paste0(nom_col, "__imputed_weighted_sum")
  df <- df |> mutate(comment = ifelse(is.na(!!sym(nom_col)),
    ifelse(is.na(comment),
      new_comment,
      paste(comment, new_comment, sep = ",")
    ),
    comment
  ))
  if (!is.na(denom_col) &&
    sum(is.na(df[[denom_col]])) == 0 &&
    sum(df[[denom_col]]) > 0
  ) {
    # If denominator is available, spread proportionally, only limited by
    # <10 constraint.
    df_est <- spread(
      df |> filter(is.na(.data[[nom_col]])),
      remainder,
      nom_col,
      denom_col
    )
  } else { # If denominator is zero, spread evenly.
    df_est <- df |>
      filter(is.na(.data[[nom_col]])) |>
      mutate(!!nom_col := remainder / na_rows)
    # Round to full integer, maintaining sum
    df_est[[nom_col]] <- smart_round(df_est[[nom_col]])
  }

  rbind(df |> filter(!is.na(.data[[nom_col]])), df_est) |>
    select(-all_of(groups))
}

spread <- function(df, remainder, nom_col, denom_col) {
  if (nrow(df) == 0) {
    return
  }
  total_denom <- sum(df[[denom_col]])
  est <- df |>
    mutate(!!nom_col := remainder * .data[[denom_col]] / total_denom)
  est[[nom_col]] <- smart_round(est[[nom_col]])
  overflown <- est |> filter(.data[[nom_col]] > 9)

  if (nrow(overflown) == 0) {
    return(est)
  }

  overflown[[nom_col]] <- 9
  return(rbind(
    overflown,
    spread(
      est |> filter(.data[[nom_col]] <= 9),
      remainder - nrow(overflown) * 9,
      nom_col,
      denom_col
    )
  ))
}

repeat_until_stable <- function(df, col, fun) {
  prev_na <- 0
  while (TRUE) {
    df <- df |> fun()
    curr_na <- sum(is.na(df[[col]]))
    if (curr_na == prev_na) break
    prev_na <- curr_na
  }
  return(df)
}

retry_download <- function(url, dir, retry = 3) {
  success <- FALSE

  for (i in 1:retry) {
    tryCatch(
      {
        download.file(url, dir)
        success <- TRUE
      },
      error = function(e) {
        if (i < retry) {
          message(paste0("Download attempt ", i, " failed. Retrying..."))
        } else {
          stop("Download failed after ", retry, " attempts.")
        }
      }
    )

    if (success) {
      break
    }
  }
}
