source("lib/common.r")

# Median Home Prices
df <- read.csv(
  paste0(
    "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=958&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=MSPUS&scale=left&cosd=1963-01-01&coed=",
    Sys.Date(),
    "&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=",
    Sys.Date(),
    "&revision_date=",
    Sys.Date(),
    "&nd=1963-01-01"
  )
)

data <- df |>
  setNames(c("date", "price")) |>
  mutate(date = yearquarter(ymd(date))) |>
  as_tsibble(index = date)

chart <-
  ggplot(data, aes(x = date, y = price)) +
  scale_x_yearquarter(date_breaks = "8 year") +
  scale_y_continuous(
    trans = "log2",
    labels = scales::dollar_format(prefix = "$", suffix = "")
  ) +
  geom_line(aes(y = price)) +
  labs(
    title = "Median Sales Price of Houses Sold [USA]",
    subtitle = "Source: Federal Reserve Bank of St. Louis",
    y = "US Dollars",
    x = "Quarter of Year"
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    color = "black",
    linetype = "dashed"
  ) +
  twitter_theme() +
  ggrepel::geom_label_repel(
    data = tail(data, n = 1) |> mutate(str = paste0(
      date, ": ", as_usd(price)
    )),
    aes(label = str),
    nudge_y = 0.1,
    segment.color = "grey50",
  )

save_chart(chart, "economy/usa/median_home_price")
