source("lib/common.r")

df <- fromJSON("data_static/genome_length.json")

get_info <- function(d) {
  q <- d |> quantile(probs = c(.025, .975), na.rm = TRUE)
  data.frame(mean = mean(d), lower = q[[1]], upper = q[[2]], max = max(d))
}

df[sapply(df, zero_length)] <- NA

quantiles <- df |> lapply(get_info)
data <- as.data.frame(do.call(rbind, quantiles)) |> as_tibble()
data$date <- seq(as.Date("2020/1/1"), by = "day", length.out = nrow(data))

ggplot(data, aes(x = date, y = mean)) +
  labs(
    title = paste0("Sequence Length uploaded to GISAID"),
    subtitle = "95% percentile; Source: GISAID",
    y = "nt",
    x = "Date"
  ) +
  geom_ribbon(
    aes(
      ymin = lower,
      ymax = upper,
      x = date
    ),
    alpha = 0.2,
    fill = "#5383EC",
    color = "#5383EC"
  ) +
  geom_line(color = "black", linewidth = 1) +
  geom_hline(yintercept = 29903, linetype = 2) +
  scale_y_continuous(limits = c(29903 * .98, 29903 * 1.001)) +
  twitter_theme() +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))

# Max
ggplot(data |> select(date, max), aes(x = date, y = max)) +
  labs(
    title = paste0("Max Sequence Length uploaded to GISAID"),
    subtitle = "Max Length; Source: GISAID",
    y = "nt",
    x = "Date"
  ) +
  scale_y_continuous(limits = c(29903 * .99, 29903 * 1.01)) +
  geom_line(color = "red", linewidth = 1) +
  geom_hline(yintercept = 29903, linetype = 2) +
  twitter_theme() +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
