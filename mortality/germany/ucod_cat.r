source("lib/common.r")

data <- read_remote("mortality/Germany/deaths_icd10_year.csv")

data %>%
  filter(icd10 %in% c("U071", "U072")) %>%
  group_by(year) %>%
  summarise(deaths = sum(deaths))

who_icd10_categories <- c(
  "A00-B99 Certain infectious and parasitic diseases",
  "C00-D48 Neoplasms",
  "D50-D89 Diseases of the blood and blood-forming organs..",
  "E00-E90 Endocrine, nutritional and metabolic diseases",
  "F00-F99 Mental and behavioural disorders",
  "G00-G99 Diseases of the nervous svstem",
  "H00-H59 Diseases of the eye and adnexa",
  "H60-H95 Diseases of the ear and mastoid process",
  "I00-I99 Diseases of the circulatory system",
  "J00-J99 Diseases of the respiratory system",
  "K00-K93 Diseases of the digestive system",
  "L00-L99 Diseases of the skin and subcutaneous tissue",
  "M00-M99 Diseases of the musculoskeletal system and connective tissue",
  "N00-N99 Diseases of the genitourinary system",
  "O00-O99 Pregnancy, childbirth and the puerperium",
  "P00-P96 Certain conditions originating in the perinatal period",
  "Q00-Q99 Congenital malformations, deformations and chromosomal abn.",
  "R00-R99 Symptoms, signs and abnormal clinical and laboratory findings..",
  "S00-T98 Injury, poisoning and certain other consequences of external causes",
  "U00-U85 Codes for special purposes",
  "V01-Y89 External causes of morbidity and mortality",
  "Z00-Z99 Factors influencing health status and contact with health services"
)

generateListOfCodes <- function(start, end) {
  start_letter <- left(start, 1)
  end_letter <- left(end, 1)
  l <- LETTERS[which(LETTERS == start_letter):which(LETTERS == end_letter)]
  result <- c()
  for (letter in l) {
    start_d <- ifelse(letter == start_letter, as.numeric(right(start, 2)), 0)
    end_d <- ifelse(letter == end_letter, as.numeric(right(end, 2)), 99)
    for (number in start_d:end_d) {
      result <- c(result, paste0(letter, str_pad(number, 2, pad = "0")))
    }
  }
  result
}

diff <- 0
for (codes in who_icd10_categories) {
  if (left(codes, 1) %in% LETTERS[21:26]) next

  all_codes <- generateListOfCodes(left(codes, 3), mid(codes, 5, 3))
  df <- data %>%
    mutate(icd10 = left(icd10, 3)) %>%
    filter(grepl(
      paste0(all_codes, collapse = "|"),
      icd10
    )) %>%
    group_by(year) %>%
    summarise(deaths = sum(deaths, na.rm = TRUE)) %>%
    ungroup()

  df2 <- df %>% as_tsibble(index = year)
  training_data <- df2 %>%
    filter(year <= 2019) %>%
    filter(year >= 2010)

  prediction <- training_data %>%
    model(RW(deaths ~ drift())) %>%
    forecast(h = 2)
  pandemic <- df2 %>% filter(year > 2019)
  pandemic$expected <- (hilo(prediction, level = 80) %>% unpack_hilo(cols = `80%`))$`80%_upper`
  pandemic <- pandemic %>% mutate(difference = expected - deaths)

  chart <- ggplot(df2, aes(x = year, y = deaths)) +
    labs(
      title = codes,
      subtitle = "Quelle: Destatis",
      y = "Deaths",
      x = "Year"
    ) +
    twitter_theme() +
    geom_line(color = "#5383EC") +
    geom_text(
      aes(label = round(deaths)),
      vjust = 2.5, colour = "#ffffff", size = 1
    ) +
    geom_smooth(
      data = training_data,
      method = "lm_right",
      fullrange = TRUE,
      se = TRUE,
      level = .95,
      linetype = "dashed",
    ) +
    geom_smooth(
      data = training_data,
      method = "lm",
      fullrange = FALSE,
      se = FALSE,
      linetype = "solid"
    ) +
    theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
    scale_y_continuous(labels = comma_format(decimal.mark = ","))

  save_chart(
    chart, paste0("mortality/Germany/", left(codes, 7)),
    upload = FALSE
  )

  diff <- sum(diff, pandemic$difference[2])
}

print(diff)
