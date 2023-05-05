source("lib/common.r")

df <- read.csv(
  paste0(
    "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=639&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=MORTGAGE30US&scale=left&cosd=1970-01-12&coed=",
    Sys.Date(),
    "&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Weekly%2C%20Ending%20Thursday&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=",
    Sys.Date(),
    "&revision_date=",
    Sys.Date(),
    "&nd=1971-04-02"
  )
)

monthly_payment <- 3000

calculateAmount <- function(rate) {
  # calculate simple monthly rate
  monthly_rate <- rate / 100 / 12

  # calculate (constant) contractual monthly payment amount
  #       derived from the present value formula for annuities
  r <- (1 + monthly_rate)^360 - 1
  monthly_payment / (monthly_rate * (r + 1) / r)
}

data <- df |>
  setNames(c("date", "rate")) |>
  mutate(date = ymd(date), possible_price = calculateAmount(rate)) |>
  as_tsibble(index = date)

chart <- ggplot(data, aes(x = date)) +
  geom_line(aes(y = possible_price)) +
  scale_y_continuous(
    labels = scales::dollar_format(prefix = "$", suffix = "")
  ) +
  labs(
    title = paste0("How much 'house' does $3,000 buy you?"),
    y = "Mortgage Value",
    x = "Week of Year"
  ) +
  twitter_theme()

save_chart(chart, "economy/usa/mortgage_buying_power.png")
