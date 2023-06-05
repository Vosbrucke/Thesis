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
library(ggplot2)

set.ZeroPolicyOption(TRUE)

get.ZeroPolicyOption()

{
# Read df for all countries (the data without foreign-born population included)
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

df_germany_codes <- readxl::read_xlsx("Raw_data/Foreign-born population/germany_adm_classification.xlsx", sheet = 2) %>% 
  select(1, 4)

df_germany_codes_headers <- c("ideu", "nuts2016")

df_germany_codes %<>% 
  set_colnames(df_germany_codes_headers) %>% 
  filter(
    str_length(ideu) == 5
  )

df_germany_1 <- df_germany %>% 
  set_colnames(df_germany_headers) %>% 
  inner_join(df_germany_codes, by = c("reg_id" = "ideu")) %>% 
    mutate(
      across(c(5, 6), as.integer),
      foreign_born_population = male + female,
      year = as.double(year),
      reg_id = as.double(reg_id)
      ) %>%
    select(1:4, 7, 8) %>% 
    mutate(foreign_born_population = as.integer(foreign_born_population)) %>%
    pivot_wider(names_from = grouping, values_from = foreign_born_population)

df_germany <- df %>% 
    dplyr::inner_join(df_germany_1, by = c("year", "nuts2016")) %>%
    mutate(across(34:51, ~ .x / Insgesamt)) %>%
    filter(!is.na(Insgesamt))

# Take names to translate 
df_germany %>% 
  select(34:51) %>% 
  colnames() %>% 
  paste(., collapse = ", ")

# Take the English column names
df_germany_colnames_1 <- df_germany %>% 
  select(1:33) %>% 
  colnames()

# Make a full English column names vector
df_germany_names <- c(df_germany_colnames_1, 'Total', 'EU-28 (until January 31, 2020)', 'third countries to EU-28 (until January 31, 2020)', 'Africa', 'North Africa', 'West Africa', 'Central Africa', 'East Africa', 'South Africa', 'America', 'North America', 'Central America and the Caribbean', 'South America', 'Asia', 'Western Asia', 'South and Southeast Asia', 'East and Central Asia', 'Australia and Oceania')

# Change column names for the data frame
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
  mutate(
    across(3:12, ~ .x / Total),
    foreign_born_population = 1 - Spanish
    )

# Get the column names to translator
colnames(df_spain_fixed) %>% 
  paste(., collapse = ", ")

# Set English column names
colnames(df_spain_fixed) <- c('year', 'regionname', 'Total', 'Spanish', 'Country of the EU28 without Spain', 'Country of Europe minus the EU28', 'From Africa', 'From North America', 'From Central America and the Caribbean', 'From South America', 'From Asia', 'From Oceania', 'Stateless persons', 'foreign_born_population')

df_spain <- df %>% 
  inner_join(df_spain_fixed, by = c("year", "regionname")) %>% 
  set_colnames(make_clean_names(colnames(.)))

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


# Used code to fix regional codes in Hungary
# df_hungary_fixed <- read.csv2("Processed_data/Hungary_nuts3_fixed.csv")
# 
# # Add the nuts 3 regional code
# df %>% 
#   filter(country == "Hungary", year == 2019) %>% 
#   select(nuts2016, regionname) %>% 
#   write_csv(.,"Processed_data/Hungary_nuts3_fix.csv")
}

# Adding the shapefiles to data frames
{
# Load shapefile for regions- this will be used to join with data frame 
shp <- st_read("Raw_data/NUTS_RG_10M_2016_3035/NUTS_RG_10M_2016_3035.shp") %>% 
  # Filter data only on for nuts 3 level
  dplyr::filter(LEVL_CODE == 3) %>% 
  dplyr::filter(NUTS_ID %in% unique(df$nuts2016))
  
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
  left_join(df_germany %>% filter(year %in% c(2017, 2019)), by = c("NUTS_ID" = "nuts2016")) %>% 
  filter(!is.na(total))

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
}

# Visualizations
{
# Add a theme map
theme_map <- function(...) {
  theme_minimal() +
    theme(
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = "white",
                                     color = "white"),
      panel.background = element_rect(fill = "white",
                                      color = "white"),
      legend.background = element_rect(fill = "white",
                                       color = "white"),
      # borders and margins
      # plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      # panel.border = element_blank(),
      # panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 11, color = "black"),
      legend.text = element_text(size = 9, hjust = 0,
                                 color = "black"),
      plot.title = element_text(size = 15, hjust = 0,
                                color = "black"),
      # plot.title.position = "plot",
      # plot.caption.position = "plot",
      plot.subtitle = element_text(size = 10, hjust = 0.5,
                                   color = "black",
                                   margin = margin(b = -0.1,
                                                   t = -0.1,
                                                   l = 2,
                                                   unit = "cm"),
                                   debug = F),
      # captions
      plot.caption = element_text(size = 7,
                                  hjust = .5,
                                  margin = margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184"),
...
)
}

# Mapping eurosceptism in the selected countries

# Germany visualization
shp_germany %>% filter(type == "EP") %>% 
ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill = growth_farright_p_perc), linewidth = 0.2, color = "white") +
  labs(
    x = "", 
    y = "", 
    title = paste("Far-right political movements growth in Germany in", type, "elections (%)"),
  ) +
  scale_fill_gradient2(low = "#3B9AB2", mid = "#FEF8F7", high = "#F21A00", name = "", midpoint = 0, limits = c(0, 40)) +
  theme_map() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.text = element_text(hjust = 0.5),
    legend.spacing = unit(0.4, "cm"),
  )  +
  facet_wrap(~year)

ggsave("Plots/Countries_analysis/Germany_ep.png", dpi = 600, width = 20, height = 20, units = "cm", bg = "white")

shp_germany %>% filter(type == "Parliament") %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill = growth_farright_p_perc), linewidth = 0.2, color = "white") +
  labs(
    x = "", 
    y = "", 
    title = paste("Far-right political movements growth in Germany in parliamentary elections (%)"),
  ) +
  scale_fill_gradient2(low = "#3B9AB2", mid = "#FEF8F7", high = "#F21A00", name = "", midpoint = 0, limits = c(0, 40)) +
  theme_map() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.text = element_text(hjust = 0.5),
    legend.spacing = unit(0.4, "cm"),
  )  +
  facet_wrap(~year)

ggsave("Plots/Countries_analysis/Germany_parliamentary.png", dpi = 600, width = 20, height = 20, units = "cm", bg = "white")

# Spain visualization
shp_spain %>% filter(type == "EP") %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill = growth_farright_p_perc), linewidth = 0.2, color = "white") +
  labs(
    x = "", 
    y = "", 
    title = paste("Far-right political movements growth in Spain in", type, "elections (%)"),
  ) +
  scale_fill_gradient2(low = "#3B9AB2", mid = "#FEF8F7", high = "#F21A00", name = "", midpoint = 0, limits = c(0, 30)) +
  theme_map() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.text = element_text(hjust = 0.5),
    legend.spacing = unit(0.4, "cm"),
  )  +
  facet_wrap(~year)

ggsave("Plots/Countries_analysis/Spain_ep.png", dpi = 600, width = 20, height = 20, units = "cm", bg = "white")

shp_spain %>% filter(type == "Parliament") %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill = growth_farright_p_perc), linewidth = 0.2, color = "white") +
  labs(
    x = "", 
    y = "", 
    title = paste("Far-right political movements growth in Spain in parliamentary elections (%)"),
  ) +
  scale_fill_gradient2(low = "#3B9AB2", mid = "#FEF8F7", high = "#F21A00", name = "", midpoint = 0, limits = c(0, 30)) +
  theme_map() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.text = element_text(hjust = 0.5),
    legend.spacing = unit(0.4, "cm"),
  )  +
  facet_wrap(~year)

ggsave("Plots/Countries_analysis/Spain_parliamentary.png", dpi = 600, width = 20, height = 20, units = "cm", bg = "white")

# Italy visualization
shp_italy %>% filter(type == "EP") %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill = growth_farright_p_perc), linewidth = 0.2, color = "white") +
  labs(
    x = "", 
    y = "", 
    title = paste("Far-right political movements growth in Italy in", type, "elections (%)"),
  ) +
  scale_fill_gradient2(low = "#3B9AB2", mid = "#FEF8F7", high = "#F21A00", name = "", midpoint = 0, limits = c(0, 50)) +
  theme_map() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.text = element_text(hjust = 0.5),
    legend.spacing = unit(0.4, "cm"),
  ) +
  facet_wrap(~2019)

ggsave("Plots/Countries_analysis/Italy_ep.png", dpi = 600, width = 20, height = 20, units = "cm", bg = "white")

# Hungary visualization
shp_hungary %>% filter(year > 2004, type == "EP") %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill = growth_farright_p_perc), linewidth = 0.2, color = "white") +
  labs(
    x = "", 
    y = "", 
    title = paste("Far-right political movements growth in Hungary in", type, "elections (%)"),
  ) +
  scale_fill_gradient2(low = "#3B9AB2", mid = "#FEF8F7", high = "#F21A00", name = "", midpoint = 0, limits = c(-20, 80), breaks = seq(-20, 80, 20)) +
  theme_map() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.text = element_text(hjust = 0.5),
    legend.spacing = unit(0.4, "cm"),
  )  +
  facet_wrap(~year)

ggsave("Plots/Countries_analysis/Hungary_ep.png", dpi = 600, width = 20, height = 20, units = "cm", bg = "white")

shp_hungary %>% filter(type == "Parliament") %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill = growth_farright_p_perc), linewidth = 0.2, color = "white") +
  labs(
    x = "", 
    y = "", 
    title = paste("Far-right political movements growth in Hungary in parliamentary elections (%)"),
  ) +
  scale_fill_gradient2(low = "#3B9AB2", mid = "#FEF8F7", high = "#F21A00", name = "", midpoint = 0, limits = c(-20, 80), breaks = seq(-20, 80, 20)) +
  theme_map() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.text = element_text(hjust = 0.5),
    legend.spacing = unit(0.4, "cm"),
  )  +
  facet_wrap(~year)

ggsave("Plots/Countries_analysis/Hungary_parliamentary.png", dpi = 600, width = 20, height = 20, units = "cm", bg = "white")

# Sweden visualization
shp_sweden %>% filter(year > 2004, type == "EP") %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill = growth_farright_p_perc), linewidth = 0.2, color = "white") +
  labs(
    x = "", 
    y = "", 
    title = paste("Far-right political movements growth in Sweden in", type, "elections (%)"),
  ) +
  scale_fill_gradient2(low = "#3B9AB2", mid = "#FEF8F7", high = "#F21A00", name = "", midpoint = 0, limits = c(0, 10)) +
  theme_map() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.text = element_text(hjust = 0.5),
    legend.spacing = unit(0.4, "cm"),
  )  +
  facet_wrap(~year, ncol = 3)

ggsave("Plots/Countries_analysis/Sweden_ep.png", dpi = 600, width = 20, height = 20, units = "cm", bg = "white")

shp_sweden %>% filter(type == "Parliament") %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill = growth_farright_p_perc), linewidth = 0.2, color = "white") +
  labs(
    x = "", 
    y = "", 
    title = paste("Far-right political movements growth in Sweden in parliamentary elections (%)"),
  ) +
  scale_fill_gradient2(low = "#3B9AB2", mid = "#FEF8F7", high = "#F21A00", name = "", midpoint = 0, limits = c(0, 10)) +
  theme_map() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.text = element_text(hjust = 0.5),
    legend.spacing = unit(0.4, "cm"),
  )  +
  facet_wrap(~year, ncol = 4)

ggsave("Plots/Countries_analysis/Sweden_parliamentary.png", dpi = 600, width = 20, height = 20, units = "cm", bg = "white")
}

# Panel analysis
{
# Spain case
r_side_spain <- df_spain %>% 
  select(17, 20, 22:28, 43) %>%
  colnames()

formula_spain <- as.formula(paste("growth_farright_p_perc ~", paste0(r_side_spain, collapse = " + ")))

spain_per_year <- function(i, type) {
  lm_spain <- lm(formula_spain, df_spain %>% filter(type == type, year == i))
  summary(lm_spain)
}

lm_spain_ep <- lapply(seq(2014, 2019, 5), spain_per_year, type = "EP")
names(lm_spain_ep) <- paste0("Linear regression model for EP elections in ", seq(2014, 2019, 5))

# Save results in txt format
sink("Results/Spain/lm_spain_ep.txt")
  print(lm_spain_ep)
sink()

df_spain %>% filter(type == "Parliament") %>% pull(year) %>% unique()

lm_spain_parliamentary <- lapply(2019, spain_per_year, type = "Parliament")
names(lm_spain_parliamentary) <- paste0("Linear regression model for Parliamentary elections in ", 2019)

# Save results in txt format
sink("Results/Spain/lm_spain_parliamentary.txt")
  print(lm_spain_parliamentary)
sink()

# SAR
sar_spain_per_year <- function(i, type) {

  shp_spain_year_type <- shp_spain %>%
    filter(type == type, year == i)

  # Construct neighbors list from polygon list
  queen_neighbour <- poly2nb(shp_spain_year_type, queen = T)

  # Create spatial weights for neighbors lists
  listw_spain <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)


  sar_spain <- lagsarlm(formula_spain, data = shp_spain_year_type, listw = listw_spain)

  summary(sar_spain)
}

sar_spain_ep <- lapply(seq(2014, 2019, 5), sar_spain_per_year, type = "EP")
names(sar_spain_ep) <- paste0("SAR model for EP elections in ", seq(2014, 2019, 5))
sar_spain_ep

# Save results in txt format
sink("Results/Spain/sar_spain_ep.txt")
  print(sar_spain_ep)
sink()


# For 2010 the analysis doesn't work
sar_spain_parliamentary <- lapply(c(2019), sar_spain_per_year, type = "Parliamentary")
names(sar_spain_parliamentary) <- paste0("SAR model for Parliamentary elections in ", 2019)
sar_spain_parliamentary

# Save results in txt format
sink("Results/Spain/sar_spain_parliamentary.txt")
  print(sar_spain_parliamentary)
sink()


# SEM
sem_spain_per_year <- function(i, type) {

  shp_spain_year_type <- shp_spain %>%
    filter(type == type, year == i)

  # Construct neighbors list from polygon list
  queen_neighbour <- poly2nb(shp_spain_year_type, queen = T)

  # Create spatial weights for neighbors lists
  listw_spain <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
  
  sem_spain <- errorsarlm(formula_spain, data = shp_spain_year_type, listw = listw_spain)

  summary(sem_spain)
}

sem_spain_ep <- lapply(seq(2014, 2019, 5), sem_spain_per_year, type = "EP")
names(sem_spain_ep) <- paste0("SEM model for EP elections in ", seq(2014, 2019, 5))

# Save results in txt format
sink("Results/Spain/sem_spain_ep.txt")
  print(sem_spain_ep)
sink()


# SEM model for Parliamentary elections in 2019
sem_spain_parliamentary <- lapply(2019, sem_spain_per_year, type = "Parliamentary")
names(sem_spain_parliamentary) <- "SEM model for Parliamentary elections in 2019"

# Save results in txt format
sink("Results/Spain/sem_spain_parliamentary.txt")
  print(sem_spain_parliamentary)
sink()

# Spatial panel analysis for EP elections
panel_spain_ep <- pdata.frame(shp_spain %>% filter(type == "EP"), c("NUTS_ID", "year")) %>% 
  relocate(c(NUTS_ID, year), .before = LEVL_CODE)


# Construct neighbors list from polygon list
queen_neighbour <- poly2nb(shp_spain %>% filter(type == "EP", year == 2019), queen = T)

# Create spatial weights for neighbors lists
listw_spain <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)


sem_fe1 <- spml(formula_spain, data = panel_spain_ep, listw = listw_spain, model = "within", lag = T, effect = "individual", spatial.error = "b")

# # Save results in txt format
# sink("Results/Spain/sem_fe1_ep.txt")
#   print("Spatial panel SEM model with fixed effects and one direction for EP elections in 2014 and 2019 years")
#   print(summary(sem_fe1))
# sink()

# It cannot be done as we lack more time periods to analyse. So far there's only 2019 year where the far-right movements grew
# # Spatial panel analysis for Parliamentary elections
# panel_spain_parliamentary <- pdata.frame(shp_spain %>% filter(type == "Parliament"), c("NUTS_ID", "year")) %>% 
#   relocate(c(NUTS_ID, year), .before = LEVL_CODE)
# 
# 
# # Construct neighbors list from polygon list
# queen_neighbour <- poly2nb(shp_spain %>% filter(type == "Parliament", year == 2019), queen = T)
# 
# # Create spatial weights for neighbors lists
# listw_spain <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
# 
# 
# sem_fe1 <- spml(formula_spain, data = panel_spain_parliamentary, listw = listw_spain, model = "within", lag = T, effect = "individual", spatial.error = "b")
# 
# # Save results in txt format
# sink("Results/Spain/sem_fe1_parliamentary.txt")
# print("Spatial panel SEM model with fixed effects and one direction for Parliament elections in 2006-2018 period")
# print(summary(sem_fe1))
# sink()



# Germany case
r_side_germany <- df_germany %>% 
  select(17, 20, 22:28, 36, 37, 43, 47) %>%
  colnames()

formula_germany <- as.formula(paste("growth_farright_p_perc ~", paste0(r_side_germany, collapse = " + ")))


# EP elections LM 
lm_germany_ep <- lm(formula_germany, df_germany %>% filter(year == 2019))

# Save results in txt format
sink("Results/Germany/lm_germany_ep.txt")
  print("Linear regression model for EP elections in 2019")
  print(lm_germany_ep)
  glance(lm_germany_ep)
sink()


# Parliamentary elections LM 
lm_germany_parliamentary <- lm(formula_germany, df_germany %>% filter(year == 2017))

# Save results in txt format
sink("Results/Germany/lm_germany_parliamentary.txt")
  print("Linear regression model for Parliamentary elections in 2017")
  print(lm_germany_parliamentary)
  glance(lm_germany_parliamentary)
sink()

# Preparation to spatial analysis for EP elections in 2019
shp_germany_ep <- shp_germany %>%
  filter(year == 2019)

# Construct neighbors list from polygon list
queen_neighbour <- poly2nb(shp_germany_ep, queen = T)

# Create spatial weights for neighbors lists
listw_germany_ep <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)


# Preparation to spatial analysis for Parliamentary elections in 2017
shp_germany_parliamentary <- shp_germany %>%
  filter(year == 2017)

# Construct neighbors list from polygon list
queen_neighbour <- poly2nb(shp_germany_parliamentary, queen = T)

# Create spatial weights for neighbors lists
listw_germany_parliamentary <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)


# SAR EP model
sar_germany_ep <- lagsarlm(formula_germany, data = shp_germany_ep, listw = listw_germany_ep)

# Save results in txt format
sink("Results/Germany/sar_germany_ep.txt")
  print("SAR model for EP elections in 2019")
  print(sar_germany_ep)
  glance(sar_germany_ep)
sink()


# SAR model for Parliamentary elections in 2017
sar_germany_parliamentary <- lagsarlm(formula_germany, data = shp_germany_parliamentary, listw = listw_germany_parliamentary)

# Save results in txt format
sink("Results/Germany/sar_germany_parliamentary.txt")
  print("SAR model for Parliamentary elections in 2017")
  print(summary(sar_germany_parliamentary))
  glance(summary(sar_germany_parliamentary))
sink()


# SEM model for 2019 EP elections
sem_germany_ep <- errorsarlm(formula_germany, data = shp_germany_ep, listw = listw_germany_ep)

# Save results in txt format
sink("Results/Germany/sem_germany_ep.txt")
  print("SEM model for EP in 2019")
  print(summary(sarar_germany_ep))
  glance(summary(sarar_germany_ep))
sink()


# SEM model for 2017 Parliamentary elections
sem_germany_parliamentary <- errorsarlm(formula_germany, data = shp_germany_parliamentary, listw = listw_germany_parliamentary)

# Save results in txt format
sink("Results/Germany/sem_germany_ep.txt")
  print("SEM model for Parliamentary elections in 2017")
  print(summary(sem_germany_parliamentary))
  glance(summary(sem_germany_parliamentary))
sink()


# SARAR for EP elections in 2019
sarar_germany_ep <- sacsarlm(formula_germany, data = shp_germany_ep, listw = listw_germany_ep, type="sac") 

# Save results in txt format
sink("Results/Germany/sarar_germany_ep.txt")
  print("SARAR model for EP in 2019")
  print(summary(sarar_germany_ep))
  glance(summary(sarar_germany_ep))
sink()


# SARAR for Parliamentary elections in 2017
sarar_germany_parliamentary <- sacsarlm(formula_germany, data = shp_germany_parliamentary, listw = listw_germany_parliamentary, type="sac") 

# Save results in txt format
sink("Results/Germany/sarar_germany_parliamentary.txt")
  print("SARAR model for Parliamentary elections in 2017")
  print(summary(sarar_germany_parliamentary))
  glance(summary(sarar_germany_parliamentary))
sink()

# Spatial panel analysis for EP elections
panel_germany <- pdata.frame(shp_germany %>% filter(type == "EP", year > 2004), c("NUTS_ID", "year")) %>% 
  relocate(c(NUTS_ID, year), .before = LEVL_CODE)


# Construct neighbors list from polygon list
queen_neighbour <- poly2nb(shp_germany %>% filter(year == 2019), queen = T)

# Create spatial weights for neighbors lists
listw_germany <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)

# I cannot do a panel spatial analysis due to lack of data on migration. It starts from only 2015 enabling only two years, yet 1 year per election type
# sem_fe1 <- spml(formula_germany, data = panel_germany, listw = listw_germany, model = "within", lag = T, effect = "individual", spatial.error = "b")
# 
# # Save results in txt format
# sink("Results/Germany/sem_fe1_ep.txt")
#   print("Spatial panel SEM model with fixed effects and one direction for EP elections in 2017 period")
#   print(summary(sem_fe1))
# sink()
# 
# 
# # Spatial panel analysis for Parliamentary elections
# shp_germany_parliament <- shp_germany %>% filter(type == "Parliament", year > 2004)
# 
# # Construct neighbors list from polygon list
# queen_neighbour <- poly2nb(shp_germany_parliament, queen = T)
# 
# # Create spatial weights for neighbors lists
# listw_germany <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
# 
# # Check which regions had no neighbors
# which_regions <- data.frame(is_neighbor_2 = lapply(queen_neighbour, sum) > 0)
# 
# # Remove regions with no neighbor in shapefile
# shp_germany_parliament %<>%
#   bind_cols(which_regions) %>%
#   filter(is_neighbor_2 == T)
# 
# # Construct neighbors list from polygon list
# queen_neighbour <- poly2nb(shp_germany_parliament, queen = T)
# 
# # Create spatial weights for neighbors lists
# listw_germany <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
# 
# set.ZeroPolicyOption(TRUE)
# 
# get.ZeroPolicyOption()
# 
# panel_germany <- pdata.frame(shp_germany_parliament, c("NUTS_ID", "year")) %>% 
#   relocate(c(NUTS_ID, year), .before = LEVL_CODE)
# 
# sem_fe1 <- spml(formula_germany, data = panel_germany, listw = listw_germany, model = "within", lag = T, effect = "individual", spatial.error = "b")
# 
# # Save results in txt format
# sink("Results/Germany/sem_fe1_parliamentary.txt")
# print("Spatial panel SEM model with fixed effects and one direction for Parliament elections in 2006-2018 period")
# print(summary(sem_fe1))
# sink()


# Year specific analysis for Sweden, Italy and Hungary
# Sweden case
r_side_sweden <- df_sweden %>% 
  select(22, 25, 27:33, 4) %>%
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

# SAR
sar_sweden_per_year <- function(i, type) {

  shp_sweden_year_type <- shp_sweden %>%
    filter(type == type, year == i)

  # Construct neighbors list from polygon list
  queen_neighbour <- poly2nb(shp_sweden_year_type, queen = T)

  # Create spatial weights for neighbors lists
  listw_sweden <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)


  sar_sweden <- lagsarlm(formula_sweden, data = shp_sweden_year_type, listw = listw_sweden)

  summary(sar_sweden)
}

sar_sweden_ep <- lapply(seq(2009, 2019, 5), sar_sweden_per_year, type = "EP")
names(sar_sweden_ep) <- paste0("SAR model for EP elections in ", seq(2009, 2019, 5))
sar_sweden_ep

# Save results in txt format
sink("Results/Sweden/sar_sweden_ep.txt")
print(sar_sweden_ep)
sink()


# For 2010 the analysis doesn't work
sar_sweden_parliamentary <- lapply(c(2006, 2010, 2014, 2018), sar_sweden_per_year, type = "Parliamentary")
names(sar_sweden_parliamentary) <- paste0("SAR model for Parliamentary elections in ", c(2006, 2010, 2014, 2018))
sar_sweden_parliamentary

# Save results in txt format
sink("Results/Sweden/sar_sweden_parliamentary.txt")
  print(sar_sweden_parliamentary)
sink()


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
# sem_sweden_ep <- lapply(seq(2019, 2019, 5), sem_sweden_per_year, type = "EP")
# names(sem_sweden_ep) <- paste0("SEM model for EP elections in ", seq(2009, 2019, 5))
# sem_sweden_ep
# 
# sem_sweden_parliamentary <- lapply(c(2006, 2014, 2018), sem_sweden_per_year, type = "Parliamentary")
# names(sem_sweden_parliamentary) <- c(2006, 2014, 2018)
# sem_sweden_parliamentary

# Spatial panel analysis for EP elections
panel_sweden <- pdata.frame(shp_sweden %>% filter(type == "EP", year > 2004), c("NUTS_ID", "year")) %>% 
  relocate(c(NUTS_ID, year), .before = LEVL_CODE)


# Construct neighbors list from polygon list
queen_neighbour <- poly2nb(shp_sweden %>% filter(type == "EP", year == 2019), queen = T)

# Create spatial weights for neighbors lists
listw_sweden <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)


# SEM model with fixed effects, one directional
sem_fe1 <- spml(formula_sweden, data = panel_sweden, listw = listw_sweden, model = "within", lag = T, effect = "individual", spatial.error = "b")

# Save results in txt format
sink("Results/Sweden/sem_fe1_ep.txt")
  print("Spatial panel SEM model with fixed effects and one direction for EP elections in 2009-2019 period")
  print(summary(sem_fe1))
sink()

# Spatial panel analysis for Parliamentary elections
panel_sweden <- pdata.frame(shp_sweden %>% filter(type == "Parliament", year > 2004), c("NUTS_ID", "year")) %>% 
  relocate(c(NUTS_ID, year), .before = LEVL_CODE)


# Construct neighbors list from polygon list
queen_neighbour <- poly2nb(shp_sweden %>% filter(type == "Parliament", year == 2018), queen = T)

# Create spatial weights for neighbors lists
listw_sweden <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)


# SEM model with fixed effects, one directional
sem_fe1 <- spml(formula_sweden, data = panel_sweden, listw = listw_sweden, model = "within", lag = T, effect = "individual", spatial.error = "b")

# Save results in txt format
sink("Results/Sweden/sem_fe1_parliamentary.txt")
  print("Spatial panel SEM model with fixed effects and one direction for Parliament elections in 2006-2018 period")
  print(summary(sem_fe1))
sink()



# Italy case
r_side_italy <- df_italy %>% 
  select(19, 22, 24:30, 34) %>%
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
  glance(summary(sar_italy))
sink()
closeAllConnections()


# SEM
sem_italy <- errorsarlm(formula_italy, data = shp_italy, listw = listw_italy) 

tidy(summary(sem_italy)) %>% arrange(p.value)
glance(summary(sem_italy))

# Save results in txt format
sink("Results/Italy/sem_italy.txt")
  print("SEM model for EP in 2019")
  print(summary(sem_italy))
  glance(summary(sem_italy))
sink()


# SARAR
sarar_italy <- sacsarlm(formula_italy, data = shp_italy, listw = listw_italy, type="sac") 

tidy(summary(sarar_italy)) %>% arrange(p.value)
glance(summary(sarar_italy))

# Save results in txt format
sink("Results/Italy/sarar_italy.txt")
  print("SARAR model for EP in 2019")
  print(summary(sarar_italy))
  glance(summary(sarar_italy))
sink()

# It won't work due to only one year data
# spml_italy <- spml(formula_italy, data = shp_italy, listw = listw_italy)

# Hungary case
r_side_hungary <- df_hungary %>% 
  select(19, 22, 24:30, 34) %>%
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


# Doesn't work due to a correlation between variables
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
queen_neighbour <- poly2nb(shp_hungary %>% filter(type == "EP", year == 2019), queen = T)

# Create spatial weights for neighbors lists
listw_hungary <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)


sem_fe1 <- spml(formula_hungary, data = panel_hungary, listw = listw_hungary, model = "within", lag = T, effect = "individual", spatial.error = "b")

# Save results in txt format
sink("Results/Hungary/sem_fe1_ep.txt")
  print("Spatial panel SEM model with fixed effects and one direction for EP elections in 2009-2019 period")
  print(summary(sem_fe1))
sink()

# Spatial panel analysis for Parliamentary elections
panel_hungary <- pdata.frame(shp_hungary %>% filter(type == "Parliament", year > 2004), c("NUTS_ID", "year")) %>% 
  relocate(c(NUTS_ID, year), .before = LEVL_CODE)


# Construct neighbors list from polygon list
queen_neighbour <- poly2nb(shp_hungary %>% filter(type == "Parliament", year == 2018), queen = T)

# Create spatial weights for neighbors lists
listw_hungary <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)


sem_fe1 <- spml(formula_hungary, data = panel_hungary, listw = listw_hungary, model = "within", lag = T, effect = "individual", spatial.error = "b")

# Save results in txt format
sink("Results/Hungary/sem_fe1_parliamentary.txt")
  print("Spatial panel SEM model with fixed effects and one direction for Parliament elections in 2006-2018 period")
  print(summary(sem_fe1))
sink()
}

# Panel analysis but only for migration data
{
  # Spain case
  r_side_spain <- df_spain %>% 
    select(43) %>%
    colnames()
  
  formula_spain <- as.formula(paste("growth_farright_p_perc ~", paste0(r_side_spain, collapse = " + ")))
  
  spain_per_year <- function(i, type) {
    lm_spain <- lm(formula_spain, df_spain %>% filter(type == type, year == i))
    summary(lm_spain)
  }
  
  lm_spain_ep <- lapply(seq(2014, 2019, 5), spain_per_year, type = "EP")
  names(lm_spain_ep) <- paste0("Linear regression model for EP elections in ", seq(2014, 2019, 5))
  
  # Save results in txt format
  sink("Results/Spain/Migration_only/lm_spain_ep.txt")
  print(lm_spain_ep)
  sink()
  
  df_spain %>% filter(type == "Parliament") %>% pull(year) %>% unique()
  
  lm_spain_parliamentary <- lapply(2019, spain_per_year, type = "Parliament")
  names(lm_spain_parliamentary) <- paste0("Linear regression model for Parliamentary elections in ", 2019)
  
  # Save results in txt format
  sink("Results/Spain/Migration_only/lm_spain_parliamentary.txt")
  print(lm_spain_parliamentary)
  sink()
  
  # SAR
  sar_spain_per_year <- function(i, type) {
    
    shp_spain_year_type <- shp_spain %>%
      filter(type == type, year == i)
    
    # Construct neighbors list from polygon list
    queen_neighbour <- poly2nb(shp_spain_year_type, queen = T)
    
    # Create spatial weights for neighbors lists
    listw_spain <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
    
    
    sar_spain <- lagsarlm(formula_spain, data = shp_spain_year_type, listw = listw_spain)
    
    summary(sar_spain)
  }
  
  sar_spain_ep <- lapply(seq(2014, 2019, 5), sar_spain_per_year, type = "EP")
  names(sar_spain_ep) <- paste0("SAR model for EP elections in ", seq(2014, 2019, 5))
  sar_spain_ep
  
  # Save results in txt format
  sink("Results/Spain/Migration_only/sar_spain_ep.txt")
  print(sar_spain_ep)
  sink()
  
  
  # For 2010 the analysis doesn't work
  sar_spain_parliamentary <- lapply(c(2019), sar_spain_per_year, type = "Parliamentary")
  names(sar_spain_parliamentary) <- paste0("SAR model for Parliamentary elections in ", 2019)
  sar_spain_parliamentary
  
  # Save results in txt format
  sink("Results/Spain/Migration_only/sar_spain_parliamentary.txt")
  print(sar_spain_parliamentary)
  sink()
  
  
  # SEM
  sem_spain_per_year <- function(i, type) {
    
    shp_spain_year_type <- shp_spain %>%
      filter(type == type, year == i)
    
    # Construct neighbors list from polygon list
    queen_neighbour <- poly2nb(shp_spain_year_type, queen = T)
    
    # Create spatial weights for neighbors lists
    listw_spain <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
    
    sem_spain <- errorsarlm(formula_spain, data = shp_spain_year_type, listw = listw_spain)
    
    summary(sem_spain)
  }
  
  sem_spain_ep <- lapply(seq(2014, 2019, 5), sem_spain_per_year, type = "EP")
  names(sem_spain_ep) <- paste0("SEM model for EP elections in ", seq(2014, 2019, 5))
  
  # Save results in txt format
  sink("Results/Spain/Migration_only/sem_spain_ep.txt")
  print(sem_spain_ep)
  sink()
  
  
  # SEM model for Parliamentary elections in 2019
  sem_spain_parliamentary <- lapply(2019, sem_spain_per_year, type = "Parliamentary")
  names(sem_spain_parliamentary) <- "SEM model for Parliamentary elections in 2019"
  
  # Save results in txt format
  sink("Results/Spain/Migration_only/sem_spain_parliamentary.txt")
  print(sem_spain_parliamentary)
  sink()
  
  # Spatial panel analysis for EP elections
  panel_spain_ep <- pdata.frame(shp_spain %>% filter(type == "EP"), c("NUTS_ID", "year")) %>% 
    relocate(c(NUTS_ID, year), .before = LEVL_CODE)
  
  
  # Construct neighbors list from polygon list
  queen_neighbour <- poly2nb(shp_spain %>% filter(type == "EP", year == 2019), queen = T)
  
  # Create spatial weights for neighbors lists
  listw_spain <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
  
  
  sem_fe1 <- spml(formula_spain, data = panel_spain_ep, listw = listw_spain, model = "within", lag = T, effect = "individual", spatial.error = "b")

  # # Save results in txt format
  # sink("Results/Spain/Migration_only/sem_fe1_ep.txt")
  #   print("Spatial panel SEM model with fixed effects and one direction for EP elections in 2014 and 2019 years")
  #   print(summary(sem_fe1))
  # sink()
  
  # It cannot be done as we lack more time periods to analyse. So far there's only 2019 year where the far-right movements grew
  # # Spatial panel analysis for Parliamentary elections
  # panel_spain_parliamentary <- pdata.frame(shp_spain %>% filter(type == "Parliament"), c("NUTS_ID", "year")) %>% 
  #   relocate(c(NUTS_ID, year), .before = LEVL_CODE)
  # 
  # 
  # # Construct neighbors list from polygon list
  # queen_neighbour <- poly2nb(shp_spain %>% filter(type == "Parliament", year == 2019), queen = T)
  # 
  # # Create spatial weights for neighbors lists
  # listw_spain <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
  # 
  # 
  # sem_fe1 <- spml(formula_spain, data = panel_spain_parliamentary, listw = listw_spain, model = "within", lag = T, effect = "individual", spatial.error = "b")
  # 
  # # Save results in txt format
  # sink("Results/Spain/sem_fe1_parliamentary.txt")
  # print("Spatial panel SEM model with fixed effects and one direction for Parliament elections in 2006-2018 period")
  # print(summary(sem_fe1))
  # sink()
  
  
  
  # Germany case
  r_side_germany <- df_germany %>% 
    select(36, 37, 43, 47) %>%
    colnames()
  
  formula_germany <- as.formula(paste("growth_farright_p_perc ~", paste0(r_side_germany, collapse = " + ")))
  
  
  # EP elections LM 
  lm_germany_ep <- lm(formula_germany, df_germany %>% filter(year == 2019))
  
  # Save results in txt format
  sink("Results/Germany/Migration_only/lm_germany_ep.txt")
  print("Linear regression model for EP elections in 2019")
  print(lm_germany_ep)
  glance(lm_germany_ep)
  sink()
  
  
  # Parliamentary elections LM 
  lm_germany_parliamentary <- lm(formula_germany, df_germany %>% filter(year == 2017))
  
  # Save results in txt format
  sink("Results/Germany/Migration_only/lm_germany_parliamentary.txt")
  print("Linear regression model for Parliamentary elections in 2017")
  print(lm_germany_parliamentary)
  glance(lm_germany_parliamentary)
  sink()
  
  # Preparation to spatial analysis for EP elections in 2019
  shp_germany_ep <- shp_germany %>%
    filter(year == 2019)
  
  # Construct neighbors list from polygon list
  queen_neighbour <- poly2nb(shp_germany_ep, queen = T)
  
  # Create spatial weights for neighbors lists
  listw_germany_ep <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
  
  
  # Preparation to spatial analysis for Parliamentary elections in 2017
  shp_germany_parliamentary <- shp_germany %>%
    filter(year == 2017)
  
  # Construct neighbors list from polygon list
  queen_neighbour <- poly2nb(shp_germany_parliamentary, queen = T)
  
  # Create spatial weights for neighbors lists
  listw_germany_parliamentary <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
  
  
  # SAR EP model
  sar_germany_ep <- lagsarlm(formula_germany, data = shp_germany_ep, listw = listw_germany_ep)
  
  # Save results in txt format
  sink("Results/Germany/Migration_only/sar_germany_ep.txt")
  print("SAR model for EP elections in 2019")
  print(sar_germany_ep)
  glance(sar_germany_ep)
  sink()
  
  
  # SAR model for Parliamentary elections in 2017
  sar_germany_parliamentary <- lagsarlm(formula_germany, data = shp_germany_parliamentary, listw = listw_germany_parliamentary)
  
  # Save results in txt format
  sink("Results/Germany/Migration_only/sar_germany_parliamentary.txt")
  print("SAR model for Parliamentary elections in 2017")
  print(summary(sar_germany_parliamentary))
  glance(summary(sar_germany_parliamentary))
  sink()
  
  
  # SEM model for 2019 EP elections
  sem_germany_ep <- errorsarlm(formula_germany, data = shp_germany_ep, listw = listw_germany_ep)
  
  # Save results in txt format
  sink("Results/Germany/Migration_only/sem_germany_ep.txt")
  print("SEM model for EP in 2019")
  print(summary(sarar_germany_ep))
  glance(summary(sarar_germany_ep))
  sink()
  
  
  # SEM model for 2017 Parliamentary elections
  sem_germany_parliamentary <- errorsarlm(formula_germany, data = shp_germany_parliamentary, listw = listw_germany_parliamentary)
  
  # Save results in txt format
  sink("Results/Germany/Migration_only/sem_germany_ep.txt")
  print("SEM model for Parliamentary elections in 2017")
  print(summary(sem_germany_parliamentary))
  glance(summary(sem_germany_parliamentary))
  sink()
  
  
  # SARAR for EP elections in 2019
  sarar_germany_ep <- sacsarlm(formula_germany, data = shp_germany_ep, listw = listw_germany_ep, type="sac") 
  
  # Save results in txt format
  sink("Results/Germany/Migration_only/sarar_germany_ep.txt")
  print("SARAR model for EP in 2019")
  print(summary(sarar_germany_ep))
  glance(summary(sarar_germany_ep))
  sink()
  
  
  # SARAR for Parliamentary elections in 2017
  sarar_germany_parliamentary <- sacsarlm(formula_germany, data = shp_germany_parliamentary, listw = listw_germany_parliamentary, type="sac") 
  
  # Save results in txt format
  sink("Results/Germany/Migration_only/sarar_germany_parliamentary.txt")
  print("SARAR model for Parliamentary elections in 2017")
  print(summary(sarar_germany_parliamentary))
  glance(summary(sarar_germany_parliamentary))
  sink()
  
  # Spatial panel analysis for EP elections
  panel_germany <- pdata.frame(shp_germany %>% filter(type == "EP", year > 2004), c("NUTS_ID", "year")) %>% 
    relocate(c(NUTS_ID, year), .before = LEVL_CODE)
  
  
  # Construct neighbors list from polygon list
  queen_neighbour <- poly2nb(shp_germany %>% filter(year == 2019), queen = T)
  
  # Create spatial weights for neighbors lists
  listw_germany <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
  
  # I cannot do a panel spatial analysis due to lack of data on migration. It starts from only 2015 enabling only two years, yet 1 year per election type
  # sem_fe1 <- spml(formula_germany, data = panel_germany, listw = listw_germany, model = "within", lag = T, effect = "individual", spatial.error = "b")
  # 
  # # Save results in txt format
  # sink("Results/Germany/sem_fe1_ep.txt")
  #   print("Spatial panel SEM model with fixed effects and one direction for EP elections in 2017 period")
  #   print(summary(sem_fe1))
  # sink()
  # 
  # 
  # # Spatial panel analysis for Parliamentary elections
  # shp_germany_parliament <- shp_germany %>% filter(type == "Parliament", year > 2004)
  # 
  # # Construct neighbors list from polygon list
  # queen_neighbour <- poly2nb(shp_germany_parliament, queen = T)
  # 
  # # Create spatial weights for neighbors lists
  # listw_germany <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
  # 
  # # Check which regions had no neighbors
  # which_regions <- data.frame(is_neighbor_2 = lapply(queen_neighbour, sum) > 0)
  # 
  # # Remove regions with no neighbor in shapefile
  # shp_germany_parliament %<>%
  #   bind_cols(which_regions) %>%
  #   filter(is_neighbor_2 == T)
  # 
  # # Construct neighbors list from polygon list
  # queen_neighbour <- poly2nb(shp_germany_parliament, queen = T)
  # 
  # # Create spatial weights for neighbors lists
  # listw_germany <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
  # 
  # set.ZeroPolicyOption(TRUE)
  # 
  # get.ZeroPolicyOption()
  # 
  # panel_germany <- pdata.frame(shp_germany_parliament, c("NUTS_ID", "year")) %>% 
  #   relocate(c(NUTS_ID, year), .before = LEVL_CODE)
  # 
  # sem_fe1 <- spml(formula_germany, data = panel_germany, listw = listw_germany, model = "within", lag = T, effect = "individual", spatial.error = "b")
  # 
  # # Save results in txt format
  # sink("Results/Germany/sem_fe1_parliamentary.txt")
  # print("Spatial panel SEM model with fixed effects and one direction for Parliament elections in 2006-2018 period")
  # print(summary(sem_fe1))
  # sink()
  
  
  # Year specific analysis for Sweden, Italy and Hungary
  # Sweden case
  r_side_sweden <- df_sweden %>% 
    select(4:7) %>%
    colnames()
  
  formula_sweden <- as.formula(paste("growth_farright_p_perc ~", paste0(r_side_sweden, collapse = " + ")))
  
  sweden_per_year <- function(i, type) {
    lm_sweden <- lm(formula_sweden, df_sweden %>% filter(type == type, year == i))
    summary(lm_sweden)
  }
  
  lm_sweden_ep <- lapply(seq(2009, 2019, 5), sweden_per_year, type = "EP")
  names(lm_sweden_ep) <- paste0("Linear regression model for EP elections in ", seq(2009, 2019, 5))
  
  # Save results in txt format
  sink("Results/Sweden/Migration_only/lm_sweden_ep.txt")
  print(lm_sweden_ep)
  sink()
  
  
  lm_sweden_parliamentary <- lapply(seq(2006, 2018, 4), sweden_per_year, type = "Parliament")
  names(lm_sweden_parliamentary) <- paste0("Linear regression model for Parliamentary elections in ", seq(2006, 2018, 4))
  
  # Save results in txt format
  sink("Results/Sweden/Migration_only/lm_sweden_parliamentary.txt")
  print(lm_sweden_parliamentary)
  sink()
  
  # SAR
  sar_sweden_per_year <- function(i, type) {
    
    shp_sweden_year_type <- shp_sweden %>%
      filter(type == type, year == i)
    
    # Construct neighbors list from polygon list
    queen_neighbour <- poly2nb(shp_sweden_year_type, queen = T)
    
    # Create spatial weights for neighbors lists
    listw_sweden <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
    
    
    sar_sweden <- lagsarlm(formula_sweden, data = shp_sweden_year_type, listw = listw_sweden)
    
    summary(sar_sweden)
  }
  
  sar_sweden_ep <- lapply(seq(2009, 2019, 5), sar_sweden_per_year, type = "EP")
  names(sar_sweden_ep) <- paste0("SAR model for EP elections in ", seq(2009, 2019, 5))
  sar_sweden_ep
  
  # Save results in txt format
  sink("Results/Sweden/Migration_only/sar_sweden_ep.txt")
  print(sar_sweden_ep)
  sink()
  
  
  # For 2010 the analysis doesn't work
  sar_sweden_parliamentary <- lapply(c(2006, 2010, 2014, 2018), sar_sweden_per_year, type = "Parliamentary")
  names(sar_sweden_parliamentary) <- paste0("SAR model for Parliamentary elections in ", c(2006, 2010, 2014, 2018))
  sar_sweden_parliamentary
  
  # Save results in txt format
  sink("Results/Sweden/Migration_only/sar_sweden_parliamentary.txt")
  print(sar_sweden_parliamentary)
  sink()
  
  
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
  # sem_sweden_ep <- lapply(seq(2019, 2019, 5), sem_sweden_per_year, type = "EP")
  # names(sem_sweden_ep) <- paste0("SEM model for EP elections in ", seq(2009, 2019, 5))
  # sem_sweden_ep
  # 
  # sem_sweden_parliamentary <- lapply(c(2006, 2014, 2018), sem_sweden_per_year, type = "Parliamentary")
  # names(sem_sweden_parliamentary) <- c(2006, 2014, 2018)
  # sem_sweden_parliamentary
  
  # Spatial panel analysis for EP elections
  panel_sweden <- pdata.frame(shp_sweden %>% filter(type == "EP", year > 2004), c("NUTS_ID", "year")) %>% 
    relocate(c(NUTS_ID, year), .before = LEVL_CODE)
  
  
  # Construct neighbors list from polygon list
  queen_neighbour <- poly2nb(shp_sweden %>% filter(type == "EP", year == 2019), queen = T)
  
  # Create spatial weights for neighbors lists
  listw_sweden <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
  
  
  # SEM model with fixed effects, one directional
  sem_fe1 <- spml(formula_sweden, data = panel_sweden, listw = listw_sweden, model = "within", lag = T, effect = "individual", spatial.error = "b")
  
  # Save results in txt format
  sink("Results/Sweden/Migration_only/sem_fe1_ep.txt")
  print("Spatial panel SEM model with fixed effects and one direction for EP elections in 2009-2019 period")
  print(summary(sem_fe1))
  sink()
  
  # Spatial panel analysis for Parliamentary elections
  panel_sweden <- pdata.frame(shp_sweden %>% filter(type == "Parliament", year > 2004), c("NUTS_ID", "year")) %>% 
    relocate(c(NUTS_ID, year), .before = LEVL_CODE)
  
  
  # Construct neighbors list from polygon list
  queen_neighbour <- poly2nb(shp_sweden %>% filter(type == "Parliament", year == 2018), queen = T)
  
  # Create spatial weights for neighbors lists
  listw_sweden <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
  
  
  # SEM model with fixed effects, one directional
  sem_fe1 <- spml(formula_sweden, data = panel_sweden, listw = listw_sweden, model = "within", lag = T, effect = "individual", spatial.error = "b")
  
  # Save results in txt format
  sink("Results/Sweden/Migration_only/sem_fe1_parliamentary.txt")
  print("Spatial panel SEM model with fixed effects and one direction for Parliament elections in 2006-2018 period")
  print(summary(sem_fe1))
  sink()
  
  
  
  # Italy case
  r_side_italy <- df_italy %>% 
    select(34) %>%
    colnames()
  
  formula_italy <- as.formula(paste("growth_farright_p_perc ~", paste0(r_side_italy, collapse = " + ")))
  
  lm_italy <- lm(formula_italy, df_italy)
  
  # Save results in txt format
  sink("Results/Italy/Migration_only/lr_italy.txt")
  print("Linear regression model for EP in 2019")
  print(summary(lm_italy))
  sink()
  
  # SAR 
  sar_italy <- lagsarlm(formula_italy, data = shp_italy, listw = listw_italy) 
  
  tidy(summary(sar_italy)) %>% arrange(p.value)
  glance(summary(sar_italy))
  
  # Save results in txt format
  sink("Results/Italy/Migration_only/sar_italy.txt")
  print("SAR model for EP in 2019")
  print(summary(sar_italy))
  glance(summary(sar_italy))
  sink()
  closeAllConnections()
  
  
  # SEM
  sem_italy <- errorsarlm(formula_italy, data = shp_italy, listw = listw_italy) 
  
  tidy(summary(sem_italy)) %>% arrange(p.value)
  glance(summary(sem_italy))
  
  # Save results in txt format
  sink("Results/Italy/Migration_only/sem_italy.txt")
  print("SEM model for EP in 2019")
  print(summary(sem_italy))
  glance(summary(sem_italy))
  sink()
  
  
  # SARAR
  sarar_italy <- sacsarlm(formula_italy, data = shp_italy, listw = listw_italy, type="sac") 
  
  tidy(summary(sarar_italy)) %>% arrange(p.value)
  glance(summary(sarar_italy))
  
  # Save results in txt format
  sink("Results/Italy/Migration_only/sarar_italy.txt")
  print("SARAR model for EP in 2019")
  print(summary(sarar_italy))
  glance(summary(sarar_italy))
  sink()
  
  # It won't work due to only one year data
  # spml_italy <- spml(formula_italy, data = shp_italy, listw = listw_italy)
  
  # Hungary case
  r_side_hungary <- df_hungary %>% 
    select(34) %>%
    colnames()
  
  formula_hungary <- as.formula(paste("growth_farright_p_perc ~", paste0(r_side_hungary, collapse = " + ")))
  
  hungary_per_year <- function(i, type) {
    lm_hungary <- lm(formula_hungary, df_hungary %>% filter(type == type, year == i))
    summary(lm_hungary)
  }
  
  lm_hungary_ep <- lapply(seq(2009, 2019, 5), hungary_per_year, type = "EP")
  names(lm_hungary_ep) <- paste0("Linear regression model for EP elections in ", seq(2009, 2019, 5))
  
  # Save results in txt format
  sink("Results/Hungary/Migration_only/lm_hungary_ep.txt")
  print(lm_hungary_ep)
  sink()
  
  
  lm_hungary_parliamentary <- lapply(seq(2006, 2018, 4), hungary_per_year, type = "Parliament")
  names(lm_hungary_parliamentary) <- paste0("Linear regression model for Parliamentary elections in ", seq(2006, 2018, 4))
  
  # Save results in txt format
  sink("Results/Hungary/Migration_only/lm_hungary_parliamentary.txt")
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
  sink("Results/Hungary/Migration_only/sar_hungary_ep.txt")
  print(sar_hungary_ep)
  sink()
  
  
  # For 2010 the analysis doesn't work
  sar_hungary_parliamentary <- lapply(c(2006, 2014, 2018), sar_hungary_per_year, type = "Parliamentary")
  names(sar_hungary_parliamentary) <- paste0("SAR model for Parliamentary elections in ", c(2006, 2014, 2018))
  sar_hungary_parliamentary
  
  # Save results in txt format
  sink("Results/Hungary/Migration_only/sar_hungary_parliamentary.txt")
  print(sar_hungary_parliamentary)
  sink()
  
  
  # Doesn't work due to a correlation between variables
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
  queen_neighbour <- poly2nb(shp_hungary %>% filter(type == "EP", year == 2019), queen = T)
  
  # Create spatial weights for neighbors lists
  listw_hungary <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
  
  
  sem_fe1 <- spml(formula_hungary, data = panel_hungary, listw = listw_hungary, model = "within", lag = T, effect = "individual", spatial.error = "b")
  
  # Save results in txt format
  sink("Results/Hungary/Migration_only/sem_fe1_ep.txt")
  print("Spatial panel SEM model with fixed effects and one direction for EP elections in 2009-2019 period")
  print(summary(sem_fe1))
  sink()
  
  # Spatial panel analysis for Parliamentary elections
  panel_hungary <- pdata.frame(shp_hungary %>% filter(type == "Parliament", year > 2004), c("NUTS_ID", "year")) %>% 
    relocate(c(NUTS_ID, year), .before = LEVL_CODE)
  
  
  # Construct neighbors list from polygon list
  queen_neighbour <- poly2nb(shp_hungary %>% filter(type == "Parliament", year == 2018), queen = T)
  
  # Create spatial weights for neighbors lists
  listw_hungary <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
  
  
  sem_fe1 <- spml(formula_hungary, data = panel_hungary, listw = listw_hungary, model = "within", lag = T, effect = "individual", spatial.error = "b")
  
  # Save results in txt format
  sink("Results/Hungary/Migration_only/sem_fe1_parliamentary.txt")
  print("Spatial panel SEM model with fixed effects and one direction for Parliament elections in 2006-2018 period")
  print(summary(sem_fe1))
  sink()
}

