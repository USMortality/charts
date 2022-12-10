source("lib/common.r")

my_app <- rtweet_bot(
  api_key = Sys.getenv("TWITTER_API_KEY"),
  api_secret = Sys.getenv("TWITTER_API_SECRET"),
  access_token = Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret = Sys.getenv("TWITTER_ACCESS_SECRET")
)
auth_as(my_app)

if (file.exists("./data/world_max_date.csv")) {
  world_max_date_old <- read.csv("./data/world_max_date.csv")
}
data_ytd <- as_tibble(read.csv("./out/mortality/world_ytd.csv"))

df <- data_ytd %>%
  group_by(iso3c, name) %>%
  summarise(max = max(max_date))

save_csv(df, "mortality/world_max_date")

tweet <- function(name, max) {
  post_tweet(
    paste0(
      "Mortality Data for ", name,
      " has been updated. Latest data now available through ", max, "."
    ),
    media = paste0("./out/mortality/", name, "/weekly_52w_sma_line.png"),
    media_alt_text = paste("Weekly Mortality (52W SMA)", name)
  )
  post_tweet(
    paste("Weekly Mortality in", name),
    media = paste0("./out/mortality/", name, "/weekly_line.png"),
    media_alt_text = paste("Weekly Mortality", name),
    in_reply_to_status_id = get_my_timeline()$id_str[1]
  )
  post_tweet(
    paste("Monthly Mortality in", name),
    media = paste0("./out/mortality/", name, "/monthly_line.png"),
    media_alt_text = paste("Monthly Mortality", name),
    in_reply_to_status_id = get_my_timeline()$id_str[1]
  )
  post_tweet(
    paste("Quarterly Mortality in", name),
    media = paste0("./out/mortality/", name, "/quarterly_line.png"),
    media_alt_text = paste("Quarterly Mortality", name),
    in_reply_to_status_id = get_my_timeline()$id_str[1]
  )
  post_tweet(
    paste("Yearly Mortality in", name),
    media = paste0("./out/mortality/", name, "/yearly_line.png"),
    media_alt_text = paste("Yearly Mortality", name),
    in_reply_to_status_id = get_my_timeline()$id_str[1]
  )
  post_tweet(
    paste("YTD Mortality in", name, "through", max),
    media = paste0("./out/mortality/", name, "/ytd_line.png"),
    media_alt_text = paste("Mortality YTD", name),
    in_reply_to_status_id = get_my_timeline()$id_str[1]
  )
  post_tweet(
    paste("Mortality by Flu Season in", name),
    media = paste0("./out/mortality/", name, "/fluseason_line.png"),
    media_alt_text = paste("Mortality by Flu Season", name),
    in_reply_to_status_id = get_my_timeline()$id_str[1]
  )
  post_tweet(
    paste("Weekly Mortality - STL Decomposition in", name),
    media = paste0("./out/mortality/", name, "/stl_line.png"),
    media_alt_text = paste("Mortality by Flu Season", name),
    in_reply_to_status_id = get_my_timeline()$id_str[1]
  )
  post_tweet(
    paste("Yearly Mortality in", name),
    media = paste0("./out/mortality/", name, "/yearly_bar.png"),
    media_alt_text = paste("Yearly Mortality", name),
    in_reply_to_status_id = get_my_timeline()$id_str[1]
  )
  post_tweet(
    paste("YTD Mortality in", name, "through", max),
    media = paste0("./out/mortality/", name, "/ytd_bar.png"),
    media_alt_text = paste("YTD Mortality", name),
    in_reply_to_status_id = get_my_timeline()$id_str[1]
  )
  post_tweet(
    paste("Mortality in", name),
    media = paste0("./out/mortality/", name, ".png"),
    media_alt_text = paste("Mortality", name),
    in_reply_to_status_id = get_my_timeline()$id_str[1]
  )
  post_tweet(
    paste0(
      "Charts: https://www.mortality.watch/charts?prefix=mortality/", name,
      "/ \n",
      "Data: https://www.mortality.watch/data?prefix=mortality/ \n",
      "Source Code: https://github.com/USMortality/charts/tree/master/mortality"
    ),
    in_reply_to_status_id = get_my_timeline()$id_str[1]
  )
}

for (n in seq_len(nrow(df))) {
  val <- df[n, ]
  val_old <- world_max_date_old %>% filter(iso3c == val$iso3c)
  if (val$max != val_old$max) {
    print(paste(val$name, "changed"))
    tweet(val$name, val$max)
    Sys.sleep(15 * 60)
  } else {
    print(paste(val$name, "unchanged"))
  }
}
