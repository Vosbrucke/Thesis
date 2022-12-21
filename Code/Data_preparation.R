# Data preparation


# Load libraries
library(rvest)
library(tidyverse)
library(magrittr)
library(janitor)
library(rebus)
library(rio)
library(corrplot)


# Load european countries codes
eu_countries <- read.csv("Raw_data/countries_eu_inflation.csv")


# Load regional migration data (international as well)
migration_international <- read.csv("Raw_data/REGION_MIGRANTS_28102022131136074.csv") %>% 
  mutate(country_code = str_sub(REG_ID, 1, 2)) %>% 
  inner_join(eu_countries, by = c("country_code" = "code")) %>% 
  arrange(country_code) %>% 
  # filter(Place.of.birth %in% c("Foreign-born", "Native-Born")) %>%
  # filter(Place.of.birth %in% c("Native-Born")) %>%
  # filter(Place.of.birth %in% c("Foreign-born")) %>% 
  # filter(Place.of.birth == "Foreign-born from inside EU-27 countries") %>%
  group_by(Indicator, country_code, Region) %>%
  select(-c(VAR, TIME, ORIGIN)) %>% 
  pivot_wider(values_from = Value, names_from = c(Indicator, Place.of.birth)) %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  ungroup() %>% 
  select(1:3, starts_with("difference")) 


# Load regional migration data (this is only country specific migration- not international migration)
demography <- read_csv("Raw_data/REGION_DEMOGR_12112022224946351.csv") %>% 
  mutate(country_code = str_sub(REG_ID, 1, 2)) %>% 
  inner_join(eu_countries, by = c("country_code" = "code")) %>% 
  group_by(Indicator, country_code, Region) %>%
  arrange(country_code, Region, Year) %>% 
  filter(TL == "2") %>% 
  select(-c(VAR, TIME)) %>% 
  pivot_wider(values_from = Value, names_from = Indicator) %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  ungroup() %>% 
  select(3, 4, year, where(is.numeric), -c(tl, power_code_code, starts_with("reference")))


# Load sex ratio and dependency data
demography_2 <- read_csv("Raw_data/REGION_DEMOGR_12112022233317690.csv") %>% 
  mutate(country_code = str_sub(REG_ID, 1, 2)) %>% 
  filter(SEX == "T") %>%
  filter(Unit == "Percentage") %>% 
  filter(Region != str_detect(Region, "not regionalised")) %>% 
  inner_join(eu_countries, by = c("country_code" = "code")) %>% 
  group_by(Indicator, country_code, Region) %>%
  arrange(country_code, Region, Year) %>% 
  filter(TL == "2") %>% 
  select(-c(VAR, TIME)) %>% 
  pivot_wider(values_from = Value, names_from = Indicator) %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  mutate(sex_ratio_total_population_percent_population_males_over_females = ifelse(is.na(sex_ratio_total_population_percent_population_males_over_females), lead(sex_ratio_total_population_percent_population_males_over_females), sex_ratio_total_population_percent_population_males_over_females)) %>% 
  filter(is.na(flag_codes)) %>% 
  ungroup() %>% 
  select(3, year, where(is.numeric), -c(tl, power_code_code, starts_with("reference")))


# Load regional GDP
economy_1 <- read_csv("Raw_data/Regional_gdp_ppp_real_prices.csv") %>% 
  mutate(country_code = str_sub(geo, 1, 2)) %>% 
  select(geo, year = TIME_PERIOD, gdp_regional = OBS_VALUE)


# Load regional employment by sectors (extended sector category)
economy_2 <- read_csv("Raw_data/REGION_ECONOM_12112022232025706.csv") %>% 
  mutate(country_code = str_sub(REG_ID, 1, 2)) %>% 
  inner_join(eu_countries, by = c("country_code" = "code")) %>% 
  group_by(Indicator, country_code, Region) %>%
  arrange(country_code, Region, Year) %>% 
  filter(TL == "2") %>% 
  select(-c(VAR, TIME)) %>% 
  pivot_wider(values_from = Value, names_from = Indicator) %>% 
  mutate(detection = str_detect(Region, "not regionalised")) %>% 
  filter(detection == F) %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  ungroup() %>% 
  select(3, year, where(is.numeric), -c(tl, power_code_code))


# Load regional disposable and primary income
economy_3 <- read_csv("Raw_data/REGION_ECONOM_12112022233048252.csv") %>% 
  mutate(country_code = str_sub(REG_ID, 1, 2)) %>% 
  inner_join(eu_countries, by = c("country_code" = "code")) %>% 
  filter(Measure == "USD per head, current prices, current PPP") %>% 
  group_by(Indicator, country_code, Region) %>%
  arrange(country_code, Region, Year) %>% 
  filter(TL == "2") %>% 
  select(-c(VAR, TIME)) %>% 
  pivot_wider(values_from = Value, names_from = Indicator) %>% 
  mutate(detection = str_detect(Region, "not regionalised")) %>% 
  filter(detection == F) %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  ungroup() %>% 
  select(3, year, where(is.numeric), -c(tl, power_code_code, starts_with("reference")))


# Load regional labour utilization rate- clean data
economy_4 <- read_csv("Raw_data/REGION_ECONOM_12112022231737971.csv") %>% 
  mutate(country_code = str_sub(REG_ID, 1, 2)) %>% 
  inner_join(eu_countries, by = c("country_code" = "code")) %>% 
  group_by(Indicator, country_code, Region) %>%
  arrange(country_code, Region, Year) %>% 
  filter(TL == "2") %>% 
  select(-c(VAR, TIME)) %>% 
  pivot_wider(values_from = Value, names_from = Indicator) %>% 
  mutate(detection = str_detect(Region, "not regionalised")) %>% 
  filter(detection == F) %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  ungroup() %>% 
  select(3, year, where(is.numeric), -c(tl, power_code_code, starts_with("reference")))


# Load regional gva per sector- OECD data
economy_5 <- read_csv("Raw_data/REGION_ECONOM_20122022163157050.csv") %>% 
  mutate(country_code = str_sub(REG_ID, 1, 2)) %>% 
  inner_join(eu_countries, by = c("country_code" = "code")) %>% 
  group_by(Indicator, country_code, Region) %>%
  arrange(country_code, Region, Year) %>% 
  filter(TL == "2") %>%
  filter(MEAS == "USD_PPP", VAR != "GVA_IND_10_VC") %>% 
  select(-c(VAR, TIME)) %>% 
  pivot_wider(values_from = Value, names_from = Indicator) %>% 
  mutate(detection = str_detect(Region, "not regionalised")) %>% 
  filter(detection == F) %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  ungroup() %>% 
  select(3, year, where(is.numeric), -c(tl, power_code_code, starts_with("reference"))) %>% 
  mutate(across(contains("gva"), ~ .x / regional_gross_value_added_total_activities, .names = "proc_{.col}")) %>% 
  # Leave only percentage columns
  select(1, 2, contains("proc_")) 


# Load regional innovation counted as patents per million inhabitants- clean data
innovation <- read_csv("Raw_data/REGION_INNOVATION_13112022131051535.csv") %>% 
  mutate(country_code = str_sub(REG_ID, 1, 2)) %>% 
  inner_join(eu_countries, by = c("country_code" = "code")) %>% 
  group_by(Indicator, country_code, Region) %>%
  arrange(country_code, Region, Year) %>% 
  filter(TL == "2") %>% 
  select(-c(VAR, TIME)) %>% 
  pivot_wider(values_from = Value, names_from = Indicator) %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  ungroup() %>% 
  select(3, year, where(is.numeric), -c(tl, power_code_code))


# Load regional environmental indicators in regions- clean data
environment_air_pollution <- read_csv("Raw_data/REGION_SOCIAL_13112022133611222.csv") %>% 
  mutate(country_code = str_sub(REG_ID, 1, 2)) %>% 
  inner_join(eu_countries, by = c("country_code" = "code")) %>% 
  group_by(Indicator, country_code, Region) %>%
  arrange(country_code, Region, Year) %>% 
  filter(TL == "2") %>% 
  select(-c(VAR, TIME)) %>% 
  pivot_wider(values_from = Value, names_from = Indicator) %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  ungroup() %>% 
  select(3, year, starts_with("air_pollution_in_pm2_5_"))


# Load regional income distribution and poverty. Data with only one time frame- 2013 year
income <- read_csv("Raw_data/RWB_13112022134013519.csv") %>% 
  mutate(country_code = str_sub(REG_ID, 1, 2)) %>% 
  inner_join(eu_countries, by = c("country_code" = "code")) %>% 
  group_by(Indicator, country_code, Regions) %>%
  arrange(country_code, Regions, Time) %>% 
  pivot_wider(values_from = Value, names_from = Indicator) %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  filter(ind == "GINI", meas == "VALUE") %>% 
  ungroup() %>% 
  select(1, year = time, starts_with("gini_at"))


# Laod regional data on R&D personnel and researchers by sector of performance, sex and NUTS 2 regions
r_and_d_personnel <- read_csv("Raw_data/rd_p_persreg_linear 2.csv") %>% 
  mutate(country_code = str_sub(geo, 1, 2)) %>% 
  filter(sectperf == "TOTAL", unit == "PC_EMP_FTE", sex == F, prof_pos == "RSE") %>% 
  inner_join(eu_countries, by = c("country_code" = "code")) %>% 
  group_by(unit, geo) %>%
  set_colnames(make_clean_names(colnames(.))) %>% 
  arrange(geo, time_period) %>% 
  ungroup() %>% 
  select(geo, time_period, obs_value)

joined <- economy_1 %>% 
  # full_join(demography_2, by = c("reg_id", "year")) %>%
  full_join(economy_2, by = c("geo" = "reg_id", "year")) %>% 
  full_join(economy_3, by = c("geo" = "reg_id", "year")) %>% 
  full_join(economy_4, by = c("geo" = "reg_id", "year")) %>% 
  full_join(economy_5, by = c("geo" = "reg_id", "year")) %>% 
  full_join(environment_air_pollution, by = c("geo" = "reg_id", "year")) %>% 
  # full_join(education, by = c("geo" = "reg_id", "year")) %>% 
  full_join(innovation, by = c("geo" = "reg_id", "year")) %>% 
  full_join(r_and_d_personnel, by = c("geo", "year" = "time_period")) %>% 
  arrange(geo, year)


## Below there are eurostat data provided- clean, with little to no gaps in panel observations

# Economically active population
economic_act_rates <- read_csv("Raw_data/Eurostat/Economic activity rates by sex, age and NUTS 2 regions (%).csv") %>% 
  select(-c(1:4, OBS_FLAG, sex, age)) %>% 
  rename(econ_act_rate = OBS_VALUE)

# Economically active population by education level data
economic_active_pop <- read_csv("Raw_data/Eurostat/Economically active population by sex, age, educational attainment level and NUTS 2 regions.csv")


employment_in_tech_to_2008 <- read_csv("Raw_data/Eurostat/Employment in technology and knowledge-intensive sectors by NUTS 2 regions and sex 1994-2008.csv") %>% 
  filter(sex == T, unit == "PC_EMP", nace_r1 == "HTC") %>% 
  group_by(unit, geo) %>%
  pivot_wider(values_from = OBS_VALUE, names_from = nace_r1) 

employment_in_tech_from_2008 <- read_csv("Raw_data/Eurostat/Employment in technology and knowledge-intensive sectors by NUTS 2 regions and sex 2008- 2021.csv") %>% 
  filter(sex == T, unit == "PC_EMP") %>% 
  group_by(unit, geo) %>%
  pivot_wider(values_from = OBS_VALUE, names_from = nace_r2) 

# Employment in technology data
employment_in_tech_full <- employment_in_tech_to_2008 %>% 
  bind_rows(employment_in_tech_from_2008) %>% 
  ungroup() %>% 
  arrange(geo, TIME_PERIOD) %>% 
  select(geo, TIME_PERIOD, HTC)

# HTC will not be used in the analysis due to high positive correlation with HRST and 
# higher number of missing number (928) then HRST which had 411 NAs.


# HR in science data
HR_in_science <- read_csv("Raw_data/Eurostat/HR in science and technology by category and NUTS 2 regions.csv") %>% 
  # select(-c(1:5, OBS_FLAG)) %>% 
  filter(unit == "PC_ACT") %>% 
  pivot_wider(values_from = OBS_VALUE, names_from = category) %>% 
  select(geo, TIME_PERIOD, HRST)


# Disposable income data
income_disposable <- read_csv("Raw_data/Eurostat/Income of households by NUTS 2 regions.csv") %>% 
  select(-c(1:6, OBS_FLAG)) %>% 
  rename(income_disp = OBS_VALUE)


# Education data
education <- read_csv("Raw_data/Eurostat/Participation rate in education and training (last 4 weeks) by NUTS 2 regions for age class 25-64 years.csv") %>% 
  select(-c(1:5, OBS_FLAG, age)) %>% 
  rename(participation_in_formal_and_or_non_formal_training_25_to_64_year_olds_population = OBS_VALUE)


# Population data
population <- read_csv("Raw_data/Eurostat/Population by NUTS 2.csv") %>% 
  select(-c(1:6, OBS_FLAG)) %>% 
  rename(population = OBS_VALUE)


# Population structure (median age- overall, female, male)
population_structure <- read_csv("Raw_data/Eurostat/Population structure indicators by NUTS 2 region.csv") %>% 
  select(-c(1:3, 5, OBS_FLAG)) %>% 
  pivot_wider(names_from = indic_de, values_from = OBS_VALUE)


# Population by educational attainment level eurostat data
education_level <- read_csv("Raw_data/Population by educational attainment level.csv") %>% 
  filter(age == "Y25-64", isced11 == "ED5-8") %>% 
  select(geo, TIME_PERIOD, higher_education_perc = OBS_VALUE)


# Disposable income of households eurostat data
hh_income <- read_csv("Raw_data/Income of households.csv") %>% 
  select(geo, TIME_PERIOD, disp_income = OBS_VALUE)


# Load well-being data from OECD datasets
well_being <- read_csv("Raw_data/RWB_12122022203859610.csv") %>% 
  mutate(country_code = str_sub(REG_ID, 1, 2)) %>% 
  inner_join(eu_countries, by = c("country_code" = "code")) %>%
  group_by(Indicator, country_code, Regions) %>%
  arrange(country_code, Regions, Time) %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  select(-c(ind, unit_code, unit)) %>% 
  pivot_wider(values_from = value, names_from = indicator)


# Join data from eurostat and oecd data sets
joined_eu <- economic_act_rates %>% 
  full_join(HR_in_science, by = c("geo", "TIME_PERIOD")) %>% 
  full_join(employment_in_tech_full, by = c("geo", "TIME_PERIOD")) %>% 
  full_join(education, by = c("geo", "TIME_PERIOD")) %>%
  full_join(population_structure, by = c("geo", "TIME_PERIOD")) %>%
  full_join(education_level, by = c("geo", "TIME_PERIOD")) %>% 
  full_join(hh_income, by = c("geo", "TIME_PERIOD")) %>% 
  mutate(country_code = str_sub(geo, 1, 2)) %>% 
  inner_join(eu_countries, by = c("country_code" = "code")) %>% 
  select(-c(country_name_pl)) %>% 
  filter(!str_count(geo) %in% c(2,3))


# Final data set
joined_fully <- joined %>% 
  full_join(joined_eu, by = c("geo", "year" = "TIME_PERIOD")) %>% 
  filter(year > 1999)  %>%
  filter(country_name %in% c(eu_countries$country_name)) %>% 
  arrange(country_name, geo, year) %>% 
  mutate(is_duplicate = year == lag(year)) %>% 
  filter(is_duplicate == F)

# Save the data frame
write_csv(joined_fully, "Processed_data/joined_regional_data.csv")
