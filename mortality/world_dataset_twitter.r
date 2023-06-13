source("lib/common.r")

url_base <- "https://s3.mortality.watch/charts/mortality/cmr/"
my_app <- rtweet_bot(
  api_key = Sys.getenv("TWITTER_API_KEY"),
  api_secret = Sys.getenv("TWITTER_API_SECRET"),
  access_token = Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret = Sys.getenv("TWITTER_ACCESS_SECRET")
)
auth_as(my_app)

world_max_date_old <- read_remote("mortality/world_max_date.csv")
data_ytd <- read_remote("mortality/world_ytd.csv")

df <- data_ytd |>
  group_by(iso3c, jurisdiction) |>
  summarise(max = max(max_date_cmr))

downloadImage <- function(iso3c) {
  url <- paste0(url_base, iso3c, ".png")
  download.file(URLencode(url), "/tmp/tweet.png", mode = "wb")
}

tweet <- function(iso3c, jurisdiction, max) {
  downloadImage(iso3c)
  post_tweet(
    paste0(
      "📊 Mortality Data for ", jurisdiction,
      " has Been Updated! 🗺️\n",
      ">> Latest data now available through ",
      max, " <<\n",
      "🔗 ",
      paste0(
        "https://www.mortality.watch/explorer/?q=%257B%2522c%2522%253A%255B%2522",
        jurisdiction,
        "%2522%255D%252C%2522t%2522%253A%2522cmr%2522%252C%2522ct%2522%253A%2522weekly_52w_sma%2522%252C%2522df%2522%253A%25222015%2520W53%2522%252C%2522dt%2522%253A%25222023%2520W12%2522%252C%2522sp%2522%253A%2522country%2522%252C%2522v%2522%253A1%257D"
      )
    ),
    media = paste0("/tmp/tweet.png"),
    media_alt_text = paste0(
      "Mortality.Watch: Weekly Mortality (52W SMA) [", jurisdiction, "]"
    )
  )
}

tweets <- 0
for (n in seq_len(nrow(df))) {
  val <- df[n, ]
  if (left(val$jurisdiction, 6) != "USA - " &&
    left(val$jurisdiction, 6) != "DEU - ") {
    val_old <- world_max_date_old |> filter(iso3c == val$iso3c)
    if (length(val_old$iso3c) == 0 || val$max != val_old$max) {
      print(paste(val$jurisdiction, "changed"))
      tweet(val$iso3c, val$jurisdiction, val$max)
      Sys.sleep(5 * 60)
      tweets <- tweets + 1
    } else {
      print(paste(val$jurisdiction, "unchanged"))
    }
  }
}

save_csv(df, "mortality/world_max_date")
