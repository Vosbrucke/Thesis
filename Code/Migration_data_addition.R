# Case study analysis of selected countries

# Load libraries
library(tidyverse)
library(broom)
library(kableExtra)
library(magrittr)
library(ggtext)
library(sf)
library(spdep)
library(spatialreg)
library(splm)
library(spdep)
library(plm)
library(janitor)
library(zoo)


# Read df
df <- read_csv("Processed_data/df.csv")

# Add Sweden detailed data on population
df_sweden <- read_csv("Raw_data/Foreign-born population/Sweden_population.csv", skip = 1) %>% 
  filter(sex == "total", age == "total") %>% 
  pivot_longer(cols = 5:25, names_to = "year") %>% 
  select(-age, -sex) %>% 
  mutate(
    reg_id = str_sub(region, 1, 5),
    year = as.double(year)
    ) %>% 
  pivot_wider(names_from = "foreign/Swedish background", values_from = "value") %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  dplyr::inner_join(df, by = c("year", "reg_id" = "nuts2016")) %>% 
  mutate(across(contains("born"), ~ .x / population)) %>% 
  filter(year > 2002)



# Add Germany detailed data on population
df_germany <- read_csv2("Raw_data/Foreign-born population/Germany_population.csv", skip = 8, col_names = F, locale = readr::locale(encoding = "latin1"))

df_germany_headers <- c("year", "reg_id", "region_name", "grouping", "male", "female")

df_germany_1 <- df_germany %>% 
  set_colnames(df_germany_headers) %>% 
  mutate(
    across(c(5, 6), as.integer),
    foreign_born_population = male + female,
    year = as.double(year),
    reg_id = as.double(reg_id)
    ) %>% 
  select(1:4, 7) %>% 
  # Fixing naming
  mutate(
    region_name = tolower(region_name),
    region_name = str_replace(region_name, "ö", "o"),
    region_name = str_replace(region_name, "ü", "u"),
    region_name = str_replace(region_name, "ä", "a"),
    region_name = str_replace(region_name, "ß", "ss"),
    region_name = str_replace(region_name, " am ", " a. "),
    region_name = str_replace(region_name, " i. ", " im "),
    region_name = str_replace(region_name, " a. ", " am "),
    region_name = str_replace(region_name, " a. ", " am "),
    region_name = str_replace(region_name, " imr oberpfalz", " i. d. opf"),
    region_name = str_replace(region_name, "pfaffenhofen anr ilm", "pfaffenhofen a. d. ilm"),
    region_name = str_replace(region_name, " anr ", " a. d."),
    region_name = str_replace(region_name, "sankt ", "st. "),
    region_name = str_remove_all(region_name, ",.*| (de)")
    )

test <- df %>% 
  filter(country == "Germany") %>% 
  mutate(
    regionname = tolower(regionname),
    regionname = str_remove_all(regionname, ",.*| (de)"),
    regionname = str_replace(regionname, " i. d. opf.", " i. d. opf"),
    regionname = str_replace(regionname, " a. ", " am "),
  ) %>% 
  filter(year %in% 2015:2022) %>% 
  dplyr::anti_join(df_germany_1, by = c("year", "regionname" = "region_name"))

# There are some errors with this approach. First is the amount of work to deal with it. Second is that by deleting the distinction on cities and regions around city I created some data with double appearance

reg_id <- c(09186, 09363, 09373, 09374, 09479, 09575, 09773, 12051, NA, 03455, 05117, 07313, 07316)

fix <- tibble(region_name = test$regionname %>% unique(), reg_id = reg_id)

df_germany_2 <- df_germany_1 %>% 
  full_join(fix, by = "reg_id") %>% 
  mutate(region_name = ifelse(is.na(region_name.y), region_name.x, region_name.y)) %>% 
  select(-c(3, 6)) %>% 
  filter(!is.na(grouping)) %>% 
  mutate(foreign_born_population = as.integer(foreign_born_population)) %>% 
  pivot_wider(names_from = grouping, values_from = foreign_born_population)

df_germany <- df %>% 
  filter(country == "Germany") %>% 
  mutate(
    regionname = tolower(regionname),
    regionname = str_remove_all(regionname, ",.*| (de)"),
    regionname = str_replace(regionname, " i. d. opf.", " i. d. opf"),
    regionname = str_replace(regionname, " a. ", " am "),
  ) %>% 
  filter(year %in% 2015:2022) %>% 
  dplyr::inner_join(df_germany_2, by = c("year", "regionname" = "region_name")) %>% 
  mutate(across(33:50, ~ .x / Insgesamt)) %>% 
  filter(!is.na(Insgesamt))

df_germany %>% 
  select(33:50) %>% 
  colnames() %>% 
  paste(., collapse = ", ")

df_germany_colnames_1 <- df_germany %>% 
  select(1:32) %>% 
  colnames()

df_germany_names <- c(df_germany_colnames_1, 'Total', 'EU-28 (until January 31, 2020)', 'third countries to EU-28 (until January 31, 2020)', 'Africa', 'North Africa', 'West Africa', 'Central Africa', 'East Africa', 'South Africa', 'America', 'North America', 'Central America and the Caribbean', 'South America', 'Asia', 'Western Asia', 'South and Southeast Asia', 'East and Central Asia', 'Australia and Oceania')

colnames(df_germany) <- make_clean_names(df_germany_names)


# Add Italy detailed data on population
df_italy <- read_csv("Raw_data/Foreign-born population/Italy_population.csv") %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  filter(age == "TOTAL", sex == 9) %>% 
  select(3, 7, 8) %>%
  rename(foreign_born_population = obs_value)

# Load correct italian codes
df_italy_codes <- read_csv("Raw_data/Foreign-born population/Codici-statistici-e-denominazioni-al-17_01_2023.csv") %>% 
  set_colnames("region") %>% 
  mutate(
    region_name = str_sub(region, start = 9, -1) %>% tolower(),
    code = str_sub(region, 2, 6) 
    ) %>% 
  select(-1)

# Make the correction
df_italy %<>% 
  inner_join(df_italy_codes, by = c("ref_area" = "code")) %>% 
  dplyr::inner_join(df %>% mutate(regionname = tolower(regionname)), by = c("time_period" = "year", "region_name" = "regionname")) %>% 
  mutate(perc_foreign_born_population = foreign_born_population / population)


# Add Spain detailed data on population
df_spain <- read_csv2("Raw_data/Foreign-born population/Spain_population.csv", locale = readr::locale(encoding = "latin1")) %>%
  set_colnames(make_clean_names(colnames(.))) %>% 
  filter(provinces != "National Total") %>% 
  mutate(
    total = str_remove_all(total, ","),
    total = as.integer(total),
    year = str_sub(period, -4, -1) %>% as.double(),
    name = str_remove_all(provinces, "[0-9]"),
    name = str_remove_all(name, " "),
    code = str_sub(provinces, 1, 2),
    code = as.double(code)
    ) %>% 
  group_by(year, name, nationality) %>% 
  filter(row_number() == 2) %>% 
  select(1,3,6:9) %>% 
  filter(!nationality %in% c("País de Europa menos UE27_2020", "País de la UE27_2020 sin España"))

test <- df %>% 
  filter(country == "Spain") %>% 
  # pull(regionname) %>% unique()
  anti_join(df_spain, by = c("year", "regionname" = "name")) %>% pull(regionname) %>% unique()

test_2 <- df_spain %>% pull(name) %>% unique()

correct_names_in_df_spain <- test_2[test_2 %in% test]
wrong_names_in_df_spain <- test_2[!test_2 %in% test]


test_3 <- df %>% 
  filter(country == "Spain") %>% 
  arrange(regionname) %>% 
  pull(regionname) %>% 
  unique() 

to_fix <- test_3[!test_3 %in% correct_names_in_df_spain] 

code_wrong_names <- df_spain %>% 
  filter(name %in% wrong_names_in_df_spain) %>% ungroup() %>% select(code, name) %>% distinct() %>% arrange(name)

# I'll need to manually fix the names in the csv file
write_csv(tibble(code_wrong_names, c(to_fix, "missing")), "Processed_data/Spain_nuts3_fix.csv")

# Read the fixed data
fixed_names <- read_csv2("Processed_data/Spain_nuts3_fixed.csv")

df_spain_fixed <- df_spain %>% 
  full_join(fixed_names, by = "code") %>% 
  mutate(regionname = ifelse(is.na(name.y), name.x, name.y)) %>% 
  pivot_wider(names_from = nationality, values_from = total) %>% 
  select(2, 6:17) %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  mutate(
    total_non_spanish = total - spanish,
    percentage_total_non_spanish = total_non_spanish / total
    )

df_spain <- df %>% 
  inner_join(df_spain_fixed, by = c("year", "regionname"))

# Add Hungary detailed data on population
df_hungary <- read_csv2("Raw_data/Foreign-born population/Hungary_population.csv", skip = 1, col_names = T) %>% 
  filter(!is.na(nuts2016)) %>% 
  select(-3, -4) %>% 
  pivot_longer(cols = 3:25, names_to = "year") %>% 
  mutate(
    foreign_born_population = str_remove(value, " ") %>% as.integer(),
    year = as.integer(year)
    ) %>% 
  select(- value) %>% 
  dplyr::inner_join(df, by = c("year", "nuts2016")) %>% 
  set_colnames(make_clean_names(colnames(.))) %>%
  mutate(perc_foreign_born_population = foreign_born_population / population)



# df_hungary_fixed <- read.csv2("Processed_data/Hungary_nuts3_fixed.csv")
# 
# # Add the nuts 3 regional code
# df %>% 
#   filter(country == "Hungary", year == 2019) %>% 
#   select(nuts2016, regionname) %>% 
#   write_csv(.,"Processed_data/Hungary_nuts3_fix.csv")


## Country specific analysis of what factors were influential 

# First I need to add spatial component to the countries datasets
# Load shapefile for regions- this will be used to join with data frame 
shp <- st_read("Raw_data/NUTS_RG_60M_2016_3035/NUTS_RG_60M_2016_3035.shp") %>% 
  # Filter data only on for nuts 3 level
  filter(LEVL_CODE == 3) %>% 
  filter(NUTS_ID %in% unique(df$nuts2016))

# Retrieve coordinates in matrix form
coordinates <- st_coordinates(shp)

# Construct neighbors list from polygon list
queen_neighbour <- poly2nb(shp, queen = T)

# Check which regions had no neighbors
which_regions <- data.frame(is_neighbor = lapply(queen_neighbour, sum) > 0)

# Remove regions with no neighbor in shapefile
shp %<>% 
  bind_cols(which_regions) %>% 
  filter(is_neighbor == T)

# Join data frame with shapefile while keeping only those counties that are present in shapefile geometry data
shp_germany <- shp %>% 
  filter(CNTR_CODE == "DE") %>% 
  left_join(df_germany, by = c("NUTS_ID" = "nuts2016")) %>% 
  filter(!is.na(country))

shp_italy <- shp %>% 
  filter(CNTR_CODE == "IT") %>% 
  left_join(df_italy, by = c("NUTS_ID" = "nuts2016")) %>% 
  filter(!is.na(country)) %>% 
  na.omit()

# Construct neighbors list from polygon list
queen_neighbour <- poly2nb(shp_italy, queen = T)

# Create spatial weights for neighbors lists
listw_italy <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)


shp_spain <- shp %>% 
  filter(CNTR_CODE == "ES") %>% 
  left_join(df_spain, by = c("NUTS_ID" = "nuts2016")) %>% 
  filter(!is.na(country))

shp_sweden <- shp %>% 
  filter(CNTR_CODE == "SE") %>% 
  left_join(df_sweden, by = c("NUTS_ID" = "reg_id")) %>% 
  filter(!is.na(CNTR_CODE))

shp_hungary <- shp %>% 
  filter(CNTR_CODE == "HU") %>% 
  left_join(df_hungary, by = c("NUTS_ID" = "nuts2016")) %>% 
  filter(!is.na(CNTR_CODE))

# Visual presentation of the selected countries (yet to be done)



# Panel analysis for Spain and Germany
# Germany case
r_side_germany <- df_germany %>% 
  select(17, 20:28, 34:50) %>%
  colnames()

formula_germany <- as.formula(paste("growth_farright_p_perc ~", paste0(r_side_germany, collapse = " + ")))

germany_per_year <- function(i, type) {
  lm_germany <- lm(formula_germany, df_germany %>% filter(type == type, year == i))
  summary(lm_germany)
}

lm_germany_ep <- lapply(seq(2009, 2019, 5), germany_per_year, type = "EP")
names(lm_germany_ep) <- paste0("Linear regression model for EP elections in ", seq(2009, 2019, 5))

# Save results in txt format
sink("Results/Germany/lm_germany_ep.txt")
print(lm_germany_ep)
sink()


lm_germany_parliamentary <- lapply(seq(2006, 2018, 4), germany_per_year, type = "Parliament")
names(lm_germany_parliamentary) <- paste0("Linear regression model for Parliamentary elections in ", seq(2006, 2018, 4))

# Save results in txt format
sink("Results/Germany/lm_germany_parliamentary.txt")
print(lm_germany_parliamentary)
sink()

# # SAR 
# sar_germany_per_year <- function(i, type) {
#   
#   shp_germany_year_type <- shp_germany %>% 
#     filter(type == type, year == i)
#   
#   # Construct neighbors list from polygon list
#   queen_neighbour <- poly2nb(shp_germany_year_type, queen = T)
#   
#   # Create spatial weights for neighbors lists
#   listw_germany <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
#   
#   
#   sar_germany <- lagsarlm(formula_germany, data = shp_germany_year_type, listw = listw_germany) 
#   
#   summary(sar_germany)
# }
# 
# sar_germany_ep <- lapply(seq(2009, 2019, 5), sar_germany_per_year, type = "EP")
# names(sar_germany_ep) <- paste0("SAR model for EP elections in ", seq(2009, 2019, 5))
# sar_germany_ep
# 
# # Save results in txt format
# sink("Results/Germany/sar_germany_ep.txt")
# print(sar_germany_ep)
# sink()


# # For 2010 the analysis doesn't work
# sar_germany_parliamentary <- lapply(c(2006, 2014, 2018), sar_germany_per_year, type = "Parliamentary")
# names(sar_germany_parliamentary) <- paste0("SAR model for Parliamentary elections in ", c(2006, 2014, 2018))
# sar_germany_parliamentary
# 
# # Save results in txt format
# sink("Results/Germany/sar_germany_parliamentary.txt")
# print(sar_germany_parliamentary)
# sink()


# It doesn't work due to somehow very low number
# # SEM
# sem_germany_per_year <- function(i, type) {
#   
#   shp_germany_year_type <- shp_germany %>% 
#     filter(type == type, year == i)
#   
#   # Construct neighbors list from polygon list
#   queen_neighbour <- poly2nb(shp_germany_year_type, queen = T)
#   
#   # Create spatial weights for neighbors lists
#   listw_germany <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
#   
#   
#   sem_germany <- errorsarlm(formula_germany, data = shp_germany_year_type, listw = listw_germany) 
#   
#   # tidy(summary(sar_germany)) %>% arrange(p.value)
#   
#   summary(sem_germany)
# }
# 
# sem_germany_ep <- lapply(seq(2009, 2019, 5), sem_germany_per_year, type = "EP")
# names(sem_germany_ep) <- paste0("SEM model for ", seq(2009, 2019, 5))
# sem_germany_ep
# 
# # For 2010 the analysis doesn't work
# sem_germany_parliamentary <- lapply(c(2006, 2014, 2018), sem_germany_per_year, type = "Parliamentary")
# names(sem_germany_parliamentary) <- c(2006, 2014, 2018)
# sem_germany_parliamentary

# Spatial panel analysis for EP elections
panel_germany <- pdata.frame(shp_germany %>% filter(type == "EP", year > 2004), c("NUTS_ID", "year")) %>% 
  relocate(c(NUTS_ID, year), .before = LEVL_CODE)


# Construct neighbors list from polygon list
queen_neighbour <- poly2nb(shp_germany %>% filter(year == 2019), queen = T)

# Create spatial weights for neighbors lists
listw_germany <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)


sem_fe1 <- spml(formula_germany, data = panel_germany, listw = listw_germany, model = "within", lag = T, effect = "individual", spatial.error = "b")

# Save results in txt format
sink("Results/Germany/sem_fe1.txt")
print("Spatial panel SEM model with fixed effects and one direction for EP elections in 2009-2019 period")
print(summary(sem_fe1))
sink()

# Spatial panel analysis for Parliamentary elections
panel_germany <- pdata.frame(shp_germany %>% filter(type == "Parliament", year > 2004), c("NUTS_ID", "year")) %>% 
  relocate(c(NUTS_ID, year), .before = LEVL_CODE)


# Construct neighbors list from polygon list
queen_neighbour <- poly2nb(shp_germany %>% filter(year == 2019), queen = T)

# Create spatial weights for neighbors lists
listw_germany <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)


sem_fe1 <- spml(formula_germany, data = panel_germany, listw = listw_germany, model = "within", lag = T, effect = "individual", spatial.error = "b")

# Save results in txt format
sink("Results/Germany/sem_fe1.txt")
print("Spatial panel SEM model with fixed effects and one direction for Parliament elections in 2006-2018 period")
print(summary(sem_fe1))
sink()


# Year specific analysis for Sweden, Italy and Hungary
# Sweden case
r_side_sweden <- df_sweden %>% 
  select(22, 25:33, 4:7) %>%
  colnames()

formula_sweden <- as.formula(paste("growth_farright_p_perc ~", paste0(r_side_sweden, collapse = " + ")))

sweden_per_year <- function(i, type) {
  lm_sweden <- lm(formula_sweden, df_sweden %>% filter(type == type, year == i))
  summary(lm_sweden)
}

lm_sweden_ep <- lapply(seq(2009, 2019, 5), sweden_per_year, type = "EP")
names(lm_sweden_ep) <- paste0("Linear regression model for EP elections in ", seq(2009, 2019, 5))

# Save results in txt format
sink("Results/Sweden/lm_sweden_ep.txt")
print(lm_sweden_ep)
sink()


lm_sweden_parliamentary <- lapply(seq(2006, 2018, 4), sweden_per_year, type = "Parliament")
names(lm_sweden_parliamentary) <- paste0("Linear regression model for Parliamentary elections in ", seq(2006, 2018, 4))

# Save results in txt format
sink("Results/Sweden/lm_sweden_parliamentary.txt")
print(lm_sweden_parliamentary)
sink()

# # SAR 
# sar_sweden_per_year <- function(i, type) {
#   
#   shp_sweden_year_type <- shp_sweden %>% 
#     filter(type == type, year == i)
#   
#   # Construct neighbors list from polygon list
#   queen_neighbour <- poly2nb(shp_sweden_year_type, queen = T)
#   
#   # Create spatial weights for neighbors lists
#   listw_sweden <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
#   
#   
#   sar_sweden <- lagsarlm(formula_sweden, data = shp_sweden_year_type, listw = listw_sweden) 
#   
#   summary(sar_sweden)
# }
# 
# sar_sweden_ep <- lapply(seq(2009, 2019, 5), sar_sweden_per_year, type = "EP")
# names(sar_sweden_ep) <- paste0("SAR model for EP elections in ", seq(2009, 2019, 5))
# sar_sweden_ep
# 
# # Save results in txt format
# sink("Results/Sweden/sar_sweden_ep.txt")
# print(sar_sweden_ep)
# sink()


# # For 2010 the analysis doesn't work
# sar_sweden_parliamentary <- lapply(c(2006, 2014, 2018), sar_sweden_per_year, type = "Parliamentary")
# names(sar_sweden_parliamentary) <- paste0("SAR model for Parliamentary elections in ", c(2006, 2014, 2018))
# sar_sweden_parliamentary
# 
# # Save results in txt format
# sink("Results/Sweden/sar_sweden_parliamentary.txt")
# print(sar_sweden_parliamentary)
# sink()


# It doesn't work due to somehow very low number
# # SEM
# sem_sweden_per_year <- function(i, type) {
#   
#   shp_sweden_year_type <- shp_sweden %>% 
#     filter(type == type, year == i)
#   
#   # Construct neighbors list from polygon list
#   queen_neighbour <- poly2nb(shp_sweden_year_type, queen = T)
#   
#   # Create spatial weights for neighbors lists
#   listw_sweden <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
#   
#   
#   sem_sweden <- errorsarlm(formula_sweden, data = shp_sweden_year_type, listw = listw_sweden) 
#   
#   # tidy(summary(sar_sweden)) %>% arrange(p.value)
#   
#   summary(sem_sweden)
# }
# 
# sem_sweden_ep <- lapply(seq(2009, 2019, 5), sem_sweden_per_year, type = "EP")
# names(sem_sweden_ep) <- paste0("SEM model for ", seq(2009, 2019, 5))
# sem_sweden_ep
# 
# # For 2010 the analysis doesn't work
# sem_sweden_parliamentary <- lapply(c(2006, 2014, 2018), sem_sweden_per_year, type = "Parliamentary")
# names(sem_sweden_parliamentary) <- c(2006, 2014, 2018)
# sem_sweden_parliamentary

# Spatial panel analysis for EP elections
panel_sweden <- pdata.frame(shp_sweden %>% filter(type == "EP", year > 2004), c("NUTS_ID", "year")) %>% 
  relocate(c(NUTS_ID, year), .before = LEVL_CODE)


# Construct neighbors list from polygon list
queen_neighbour <- poly2nb(shp_sweden %>% filter(year == 2019), queen = T)

# Create spatial weights for neighbors lists
listw_sweden <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)


sem_fe1 <- spml(formula_sweden, data = panel_sweden, listw = listw_sweden, model = "within", lag = T, effect = "individual", spatial.error = "b")

# Save results in txt format
sink("Results/Sweden/sem_fe1.txt")
print("Spatial panel SEM model with fixed effects and one direction for EP elections in 2009-2019 period")
print(summary(sem_fe1))
sink()

# Spatial panel analysis for Parliamentary elections
panel_sweden <- pdata.frame(shp_sweden %>% filter(type == "Parliament", year > 2004), c("NUTS_ID", "year")) %>% 
  relocate(c(NUTS_ID, year), .before = LEVL_CODE)


# Construct neighbors list from polygon list
queen_neighbour <- poly2nb(shp_sweden %>% filter(year == 2019), queen = T)

# Create spatial weights for neighbors lists
listw_sweden <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)


sem_fe1 <- spml(formula_sweden, data = panel_sweden, listw = listw_sweden, model = "within", lag = T, effect = "individual", spatial.error = "b")

# Save results in txt format
sink("Results/Sweden/sem_fe1.txt")
print("Spatial panel SEM model with fixed effects and one direction for Parliament elections in 2006-2018 period")
print(summary(sem_fe1))
sink()



# Italy case
r_side_italy <- df_italy %>% 
  select(19, 22:30, 34) %>%
  colnames()

formula_italy <- as.formula(paste("growth_farright_p_perc ~", paste0(r_side_italy, collapse = " + ")))

lm_italy <- lm(formula_italy, df_italy)

# Save results in txt format
sink("Results/Italy/lr_italy.txt")
print("Linear regression model for EP in 2019")
print(summary(lm_italy))
sink()

# SAR 
sar_italy <- lagsarlm(formula_italy, data = shp_italy, listw = listw_italy) 

tidy(summary(sar_italy)) %>% arrange(p.value)
glance(summary(sar_italy))

# Save results in txt format
sink("Results/Italy/sar_italy.txt")
print("SAR model for EP in 2019")
print(summary(sar_italy))
sink()


# SEM
sem_italy <- errorsarlm(formula_italy, data = shp_italy, listw = listw_italy) 

tidy(summary(sem_italy)) %>% arrange(p.value)
glance(summary(sem_italy))

# Save results in txt format
sink("Results/Italy/sem_italy.txt")
print("SEM model for EP in 2019")
print(summary(sem_italy))
sink()


# SARAR
sarar_italy <- sacsarlm(formula_italy, data = shp_italy, listw = listw_italy, type="sac") 

tidy(summary(sarar_italy)) %>% arrange(p.value)
glance(summary(sarar_italy))

# Save results in txt format
sink("Results/Italy/sarar_italy.txt")
print("SARAR model for EP in 2019")
print(summary(sem_italy))
sink()

# It won't work due to only one year data
# spml_italy <- spml(formula_italy, data = shp_italy, listw = listw_italy)

# Hungary case
r_side_hungary <- df_hungary %>% 
  select(19, 22:30, 34) %>%
  colnames()

formula_hungary <- as.formula(paste("growth_farright_p_perc ~", paste0(r_side_hungary, collapse = " + ")))

hungary_per_year <- function(i, type) {
  lm_hungary <- lm(formula_hungary, df_hungary %>% filter(type == type, year == i))
  summary(lm_hungary)
}

lm_hungary_ep <- lapply(seq(2009, 2019, 5), hungary_per_year, type = "EP")
names(lm_hungary_ep) <- paste0("Linear regression model for EP elections in ", seq(2009, 2019, 5))

# Save results in txt format
sink("Results/Hungary/lm_hungary_ep.txt")
print(lm_hungary_ep)
sink()


lm_hungary_parliamentary <- lapply(seq(2006, 2018, 4), hungary_per_year, type = "Parliament")
names(lm_hungary_parliamentary) <- paste0("Linear regression model for Parliamentary elections in ", seq(2006, 2018, 4))

# Save results in txt format
sink("Results/Hungary/lm_hungary_parliamentary.txt")
print(lm_hungary_parliamentary)
sink()

# SAR 
sar_hungary_per_year <- function(i, type) {
  
  shp_hungary_year_type <- shp_hungary %>% 
    filter(type == type, year == i)
  
  # Construct neighbors list from polygon list
  queen_neighbour <- poly2nb(shp_hungary_year_type, queen = T)
  
  # Create spatial weights for neighbors lists
  listw_hungary <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
  
  
  sar_hungary <- lagsarlm(formula_hungary, data = shp_hungary_year_type, listw = listw_hungary) 
  
  summary(sar_hungary)
}

sar_hungary_ep <- lapply(seq(2009, 2019, 5), sar_hungary_per_year, type = "EP")
names(sar_hungary_ep) <- paste0("SAR model for EP elections in ", seq(2009, 2019, 5))
sar_hungary_ep

# Save results in txt format
sink("Results/Hungary/sar_hungary_ep.txt")
print(sar_hungary_ep)
sink()


# For 2010 the analysis doesn't work
sar_hungary_parliamentary <- lapply(c(2006, 2014, 2018), sar_hungary_per_year, type = "Parliamentary")
names(sar_hungary_parliamentary) <- paste0("SAR model for Parliamentary elections in ", c(2006, 2014, 2018))
sar_hungary_parliamentary

# Save results in txt format
sink("Results/Hungary/sar_hungary_parliamentary.txt")
print(sar_hungary_parliamentary)
sink()


# It doesn't work due to somehow very low number
# # SEM
# sem_hungary_per_year <- function(i, type) {
#   
#   shp_hungary_year_type <- shp_hungary %>% 
#     filter(type == type, year == i)
#   
#   # Construct neighbors list from polygon list
#   queen_neighbour <- poly2nb(shp_hungary_year_type, queen = T)
#   
#   # Create spatial weights for neighbors lists
#   listw_hungary <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
#   
#   
#   sem_hungary <- errorsarlm(formula_hungary, data = shp_hungary_year_type, listw = listw_hungary) 
#   
#   # tidy(summary(sar_hungary)) %>% arrange(p.value)
#   
#   summary(sem_hungary)
# }
# 
# sem_hungary_ep <- lapply(seq(2009, 2019, 5), sem_hungary_per_year, type = "EP")
# names(sem_hungary_ep) <- paste0("SEM model for ", seq(2009, 2019, 5))
# sem_hungary_ep
# 
# # For 2010 the analysis doesn't work
# sem_hungary_parliamentary <- lapply(c(2006, 2014, 2018), sem_hungary_per_year, type = "Parliamentary")
# names(sem_hungary_parliamentary) <- c(2006, 2014, 2018)
# sem_hungary_parliamentary

# Spatial panel analysis for EP elections
panel_hungary <- pdata.frame(shp_hungary %>% filter(type == "EP", year > 2004), c("NUTS_ID", "year")) %>% 
  relocate(c(NUTS_ID, year), .before = LEVL_CODE)


# Construct neighbors list from polygon list
queen_neighbour <- poly2nb(shp_hungary %>% filter(year == 2019), queen = T)

# Create spatial weights for neighbors lists
listw_hungary <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)


sem_fe1 <- spml(formula_hungary, data = panel_hungary, listw = listw_hungary, model = "within", lag = T, effect = "individual", spatial.error = "b")

# Save results in txt format
sink("Results/Hungary/sem_fe1.txt")
print("Spatial panel SEM model with fixed effects and one direction for EP elections in 2009-2019 period")
print(summary(sem_fe1))
sink()

# Spatial panel analysis for Parliamentary elections
panel_hungary <- pdata.frame(shp_hungary %>% filter(type == "Parliament", year > 2004), c("NUTS_ID", "year")) %>% 
  relocate(c(NUTS_ID, year), .before = LEVL_CODE)


# Construct neighbors list from polygon list
queen_neighbour <- poly2nb(shp_hungary %>% filter(year == 2019), queen = T)

# Create spatial weights for neighbors lists
listw_hungary <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)


sem_fe1 <- spml(formula_hungary, data = panel_hungary, listw = listw_hungary, model = "within", lag = T, effect = "individual", spatial.error = "b")

# Save results in txt format
sink("Results/Hungary/sem_fe1.txt")
print("Spatial panel SEM model with fixed effects and one direction for Parliament elections in 2006-2018 period")
print(summary(sem_fe1))
sink()

