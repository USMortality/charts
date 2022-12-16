# Source:
# https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Gesundheit/Todesursachen/Publikationen/Downloads-Todesursachen/todesursachenstatistik-5232101207015.html
# https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Gesundheit/Todesursachen/Publikationen/Downloads-Todesursachen/todesursachenstatistik-5232101217015.html
# https://www.statistischebibliothek.de/mir/servlets/solr/find?condQuery=Todesursachenstatistik+ICD10-Klassifikation

source("lib/common.r")

# ICD-10: A-U
## Load Data
d2003_1 <- read_excel("./mortality/germany/icd10/2003.xls", sheet = "TUDEU03")
d2004_1 <- read_excel("./mortality/germany/icd10/2004.xls", sheet = "TUDEU04")
d2005_1 <- read_excel("./mortality/germany/icd10/2005.xls", sheet = "tudeu05")
d2006_1 <- read_excel("./mortality/germany/icd10/2006.xls", sheet = "TUDEU06")
d2007_1 <- read_excel("./mortality/germany/icd10/2007.xls", sheet = "TUDEU07")
d2008_1 <- read_excel("./mortality/germany/icd10/2008.xls", sheet = "tudeu08")
d2009_1 <- read_excel("./mortality/germany/icd10/2009.xls", sheet = "tudeu09")
d2010_1 <- read_excel("./mortality/germany/icd10/2010.xls", sheet = "tudeu2010")
d2011_1 <- read_excel("./mortality/germany/icd10/2011.xls", sheet = "tudeu2011")
d2012_1 <- read_excel("./mortality/germany/icd10/2012.xlsx", sheet = "tudeu2012")
d2013_1 <- read_excel("./mortality/germany/icd10/2013.xlsx", sheet = "tudeu2013")
d2014_1 <- read_excel("./mortality/germany/icd10/2014.xlsx", sheet = "tudeu2014")
d2015_1 <- read_excel("./mortality/germany/icd10/2015.xlsx", sheet = "tudeu15")
d2016_1 <- read_excel("./mortality/germany/icd10/2016.xlsx", sheet = "tudeu16")
d2017_1 <- read_excel("./mortality/germany/icd10/2017.xlsx", sheet = "tudeu17")
d2018_1 <- read_excel("./mortality/germany/icd10/2018.xlsx", sheet = "tudeu18")
d2019_1 <- read_excel("./mortality/germany/icd10/2019.xlsx", sheet = "tudeu19")
d2020_1 <- read_excel("./mortality/germany/icd10/2020.xlsx", sheet = "tudeu20")
d2021_1 <- read_excel("./mortality/germany/icd10/2021.xlsx", sheet = "tudeu2021")

## Fix Column names
colnames(d2004_1) <- colnames(d2003_1)
colnames(d2005_1) <- colnames(d2003_1)
colnames(d2006_1) <- colnames(d2003_1)
colnames(d2007_1) <- colnames(d2003_1)
colnames(d2008_1) <- colnames(d2003_1)
colnames(d2009_1) <- colnames(d2003_1)
colnames(d2010_1) <- colnames(d2003_1)
colnames(d2011_1) <- colnames(d2003_1)
colnames(d2012_1) <- colnames(d2003_1)
colnames(d2013_1) <- colnames(d2003_1)
colnames(d2014_1) <- colnames(d2003_1)
colnames(d2015_1) <- colnames(d2003_1)
colnames(d2016_1) <- colnames(d2003_1)
colnames(d2017_1) <- colnames(d2003_1)
colnames(d2018_1) <- colnames(d2003_1)
colnames(d2019_1) <- colnames(d2003_1)
colnames(d2020_1) <- colnames(d2003_1)
colnames(d2021_1) <- colnames(d2003_1)

## Insert Year
d2003_1$year <- 2003
d2004_1$year <- 2004
d2005_1$year <- 2005
d2006_1$year <- 2006
d2007_1$year <- 2007
d2008_1$year <- 2008
d2009_1$year <- 2009
d2010_1$year <- 2010
d2011_1$year <- 2011
d2012_1$year <- 2012
d2013_1$year <- 2013
d2014_1$year <- 2014
d2015_1$year <- 2015
d2016_1$year <- 2016
d2017_1$year <- 2017
d2018_1$year <- 2018
d2019_1$year <- 2019
d2020_1$year <- 2020
d2021_1$year <- 2021

## Merge
data1 <- rbind(d2003_1, d2004_1, d2005_1, d2006_1, d2007_1, d2008_1, d2009_1, d2010_1, d2011_1, d2012_1, d2013_1, d2014_1, d2015_1, d2016_1, d2017_1, d2018_1, d2019_1, d2020_1, d2021_1) %>% relocate(year)

# ICD-10: V-Z
## Load Data
d2003_2 <- read_excel("./mortality/germany/icd10/2003.xls", sheet = "VTUDEU03") %>% select(-2)
d2004_2 <- read_excel("./mortality/germany/icd10/2004.xls", sheet = "VTDEU04") %>% select(-2)
d2005_2 <- read_excel("./mortality/germany/icd10/2005.xls", sheet = "vtudeu05") %>% select(-2)
d2006_2 <- read_excel("./mortality/germany/icd10/2006.xls", sheet = "VTDEU06") %>% select(-2)
d2007_2 <- read_excel("./mortality/germany/icd10/2007.xls", sheet = "VTDEU") %>% select(-2, -3)
d2008_2 <- read_excel("./mortality/germany/icd10/2008.xls", sheet = "vtudeu08 ") %>% select(-2, -3)
d2009_2 <- read_excel("./mortality/germany/icd10/2009.xls", sheet = "vtudeu09") %>% select(-2, -3)
d2010_2 <- read_excel("./mortality/germany/icd10/2010.xls", sheet = "vtudeu2010") %>% select(-2, -3)
d2011_2 <- read_excel("./mortality/germany/icd10/2011.xls", sheet = "vtudeu2011") %>% select(-2, -3)
d2012_2 <- read_excel("./mortality/germany/icd10/2012.xlsx", sheet = "vtudeu2012") %>% select(-2, -3)
d2013_2 <- read_excel("./mortality/germany/icd10/2013.xlsx", sheet = "vtudeu2013") %>% select(-2, -3)
d2014_2 <- read_excel("./mortality/germany/icd10/2014.xlsx", sheet = "vtudeu2014") %>% select(-2, -3)
d2015_2 <- read_excel("./mortality/germany/icd10/2015.xlsx", sheet = "vtudeu2015") %>% select(-2, -3)
d2016_2 <- read_excel("./mortality/germany/icd10/2016.xlsx", sheet = "vtudeu2016") %>% select(-2, -3)
d2017_2 <- read_excel("./mortality/germany/icd10/2017.xlsx", sheet = "vtudeu17") %>% select(-2, -3)
d2018_2 <- read_excel("./mortality/germany/icd10/2018.xlsx", sheet = "vtudeu18") %>% select(-2, -3)
d2019_2 <- read_excel("./mortality/germany/icd10/2019.xlsx", sheet = "vtudeu19") %>% select(-2, -3)
d2020_2 <- read_excel("./mortality/germany/icd10/2020.xlsx", sheet = "vtudeu20") %>% select(-2, -3)
d2021_2 <- read_excel("./mortality/germany/icd10/2021.xlsx", sheet = "vtudeu2021") %>% select(-2, -3)

# Fix Column names
colnames(d2003_2) <- colnames(d2003_1)
colnames(d2004_2) <- colnames(d2003_1)
colnames(d2005_2) <- colnames(d2003_1)
colnames(d2006_2) <- colnames(d2003_1)
colnames(d2007_2) <- colnames(d2003_1)
colnames(d2008_2) <- colnames(d2003_1)
colnames(d2009_2) <- colnames(d2003_1)
colnames(d2010_2) <- colnames(d2003_1)
colnames(d2011_2) <- colnames(d2003_1)
colnames(d2012_2) <- colnames(d2003_1)
colnames(d2013_2) <- colnames(d2003_1)
colnames(d2014_2) <- colnames(d2003_1)
colnames(d2015_2) <- colnames(d2003_1)
colnames(d2016_2) <- colnames(d2003_1)
colnames(d2017_2) <- colnames(d2003_1)
colnames(d2018_2) <- colnames(d2003_1)
colnames(d2019_2) <- colnames(d2003_1)
colnames(d2020_2) <- colnames(d2003_1)
colnames(d2021_2) <- colnames(d2003_1)

# Insert Year
d2003_2$year <- 2003
d2004_2$year <- 2004
d2005_2$year <- 2005
d2006_2$year <- 2006
d2007_2$year <- 2007
d2008_2$year <- 2008
d2009_2$year <- 2009
d2010_2$year <- 2010
d2011_2$year <- 2011
d2012_2$year <- 2012
d2013_2$year <- 2013
d2014_2$year <- 2014
d2015_2$year <- 2015
d2016_2$year <- 2016
d2017_2$year <- 2017
d2018_2$year <- 2018
d2019_2$year <- 2019
d2020_2$year <- 2020
d2021_2$year <- 2021

# Merge
data <- rbind(data1, d2003_2, d2004_2, d2005_2, d2006_2, d2007_2, d2008_2, d2009_2, d2010_2, d2011_2, d2012_2, d2013_2, d2014_2, d2015_2, d2016_2, d2017_2, d2018_2, d2019_2, d2020_2, d2021_2) %>%
  relocate(year) %>%
  setNames(c("year", "icd10", "sex", "all", "0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-80", "80-84", "85-89", "90+")) %>%
  mutate(sex = ifelse(sex == 1, "m", "f")) %>%
  pivot_longer(cols = c(4:24), names_to = "age_group", values_to = "deaths") %>%
  filter(age_group != "all") %>%
  mutate(deaths = as.numeric(deaths)) %>%
  mutate(deaths = ifelse(is.na(deaths), 0, deaths))

# By year
data_year <- data %>%
  select(-3, -4) %>%
  group_by(year, icd10) %>%
  summarise(deaths = sum(deaths))

# By year/sex
data_year_sex <- data %>%
  select(-4) %>%
  group_by(year, icd10, sex) %>%
  summarise(deaths = sum(deaths))

# By year/age_group
data_year_age <- data %>%
  select(-3) %>%
  group_by(year, icd10, age_group) %>%
  summarise(deaths = sum(deaths))

save_csv(data, "mortality/Germany/deaths_icd10_year_sex_age")
save_csv(data_year, "mortality/Germany/deaths_icd10_year")
save_csv(data_year_sex, "mortality/Germany/deaths_icd10_year_sex")
save_csv(data_year_age, "mortality/Germany/deaths_icd10_year_age")
