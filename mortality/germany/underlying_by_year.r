source("lib/common.r")

data <- read_remote("mortality/Germany/deaths_icd10_year.csv")

# By Year
data %>%
  mutate(category = left(icd10, 1)) %>%
  filter(grepl(paste(LETTERS[1:21], collapse = "|"), left(icd10, 1))) %>%
  group_by(year) %>%
  summarise(deaths = sum(deaths))
