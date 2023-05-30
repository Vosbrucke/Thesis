# Data preparation NUTS2

# Load libraries
library(rvest)
library(tidyverse)
library(magrittr)
library(janitor)
library(VIM)
library(mice)
library(qqplotr)

# Load European countries codes
eu_countries <- read.csv("Raw_data/countries_eu_inflation.csv")

# Load density data
df_density <- read_csv("Raw_data/Nuts2/demo_r_d3dens__custom_6387968_linear.csv") %>% 
  mutate(country_code = str_sub(geo, 1, 2)) %>% 
  inner_join(eu_countries, by = c("country_code" = "code")) %>% 
  arrange(country_code) %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  select(geo, year = time_period, country_code, country_name, pop_density = obs_value)

# Load population groups
df_pop_groups <- read_csv("Raw_data/Nuts2/demo_r_pjanaggr3__custom_6388294_linear.csv") %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  select(-obs_flag) %>% 
  pivot_wider(names_from = c(sex, age), values_from = obs_value) %>% 
  dplyr::group_by(time_period, geo) %>% 
  dplyr::mutate(
    TOTAL = F_TOTAL + M_TOTAL, 
    GE65 = F_Y_GE65 + M_Y_GE65, 
    ELDER_RATE = GE65 / TOTAL,
    FEM_RATE = M_TOTAL / F_TOTAL * 100
  ) %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  select(geo, year = time_period, population = total, elder_rate, fem_rate)

# Load gdp data
df_gdp <- read_csv("Raw_data/Nuts2/nama_10r_2gdp__custom_6388021_linear.csv") %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  filter(unit == "EUR_HAB") %>% 
  select(geo, year = time_period, gdp = obs_value)

# Load migration data
df_migration_rate <- read_csv("/Users/mr.fox/Desktop/Github/Thesis/Raw_data/Nuts2/demo_r_gind3__custom_6388633_linear.csv") %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  select(-obs_flag) %>% 
  # Filter for crude rate of net migration 
  filter(indic_de == "CNMIGRATRT") %>% 
  select(geo, year = time_period, migration_rate = obs_value)

# Load unemployment data based on foreign, european, noneuropean and total subset
df_unemployment <- read_csv("/Users/mr.fox/Desktop/Github/Thesis/Raw_data/Nuts2/lfst_r_lfur2gac__custom_6388808_linear.csv") %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  select(-obs_flag) %>% 
  pivot_wider(names_from = c_birth, values_from = obs_value) %>% 
  select(geo, year = time_period, TOTAL) %>% 
  set_colnames(make_clean_names(colnames(.)))


# Loading migration into the analysis
df_employment_migration_full <- read.csv("Raw_data/REGION_MIGRANTS_28102022131136074.csv") %>% 
  mutate(country_code = str_sub(REG_ID, 1, 2)) %>% 
  inner_join(eu_countries, by = c("country_code" = "code")) %>% 
  arrange(country_code) %>% 
  na.omit() %>% 
  mutate(is_total = str_detect(Indicator, "both")) %>% 
  filter(is_total == T) %>% 
  # filter(Place.of.birth %in% c("Foreign-born", "Native-Born")) %>%
  # filter(Place.of.birth %in% c("Native-Born")) %>%
  # filter(Place.of.birth %in% c("Foreign-born")) %>% 
  # filter(Place.of.birth == "Foreign-born from inside EU-27 countries") %>%
  group_by(Indicator, country_code, Region) %>%
  select(-c(VAR, TIME, ORIGIN)) %>% 
  pivot_wider(values_from = Value, names_from = c(Indicator, Place.of.birth)) %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  ungroup() %>% 
  select(-contains("country")) %>% 
  rename_with(~ paste0("migration_", .x)) %>% 
  select(year = migration_year, geo = migration_reg_id, 7:20)

df_elections <- read_csv("Processed_data/elections_extremes_nuts2.csv") %>% 
  select(-country, geo = nuts2, type, contains("sum")) #%>% 
  # filter(type == "EP")

df_nuts2 <- df_density %>% 
  full_join(df_pop_groups, by = c("year", "geo")) %>% 
  full_join(df_gdp, by = c("year", "geo")) %>% 
  full_join(df_migration_rate, by = c("year", "geo")) %>% 
  full_join(df_unemployment, by = c("year", "geo")) %>% 
  full_join(df_employment_migration_full, by = c("year", "geo")) %>% 
  inner_join(df_elections, by = c("year", "geo")) %>% 
  # Add lagged variables of economic factors and a social factor
  dplyr::mutate(
    gdp_lag_4 = gdp / dplyr::lag(gdp, n = 4),
    unemployment_rate_lag_4 = total / dplyr::lag(total, n = 4),
    migration_rate_lag_4 = migration_rate / dplyr::lag(migration_rate, n = 4)
  ) %>% 
  rename(unemployment_rate = total) %>% 
  # Remove infinite 
  mutate(migration_rate_lag_4 = ifelse(migration_rate_lag_4 %in% c("-Inf", "Inf"), migration_rate, migration_rate_lag_4)) %>% 
  # Remove unnecessary countries and years
  filter(country_code %in% eu_countries$code) %>%
  # I limit the data to 2009 upwards as before this date there's too much missing data
  filter(year > 2008) %>%
  select(year, country_name:unemployment_rate, "migration_unemployment_rate_15_64_year_olds_both_sex_foreign_born_from_outside_eu_27_countries", contains("sum"), contains("lag"))
  # filter(year %in% seq(2004, 2019, by = 5))

write_csv(df_nuts2, "Processed_data/df_nuts2.csv")

# Checking which countries can be analysed with the migration data
df_nas_count <- df_nuts2 %>% 
  select(-migration_unemployment_rate_15_64_year_olds_both_sex_foreign_born_from_outside_eu_27_countries) %>% 
  group_by(year, country_name) %>% 
  count() %>% 
  ungroup()

df_nas <- df_nuts2 %>% 
  select(-migration_unemployment_rate_15_64_year_olds_both_sex_foreign_born_from_outside_eu_27_countries) %>% 
  group_by(year, country_name) %>% 
  summarise(across(everything(), ~sum(is.na(.)), .names = "perc_is_na_{.col}"))# %>% 

df_nas <- df_nas %>% 
  group_by(year, country_name) %>% 
  bind_cols(df_nas_count %>% select(3)) %>%
  pivot_longer(cols = 3:14) %>% 
  group_by(year, country_name) %>% 
  summarise(n_total = sum(n), is_na_total = sum(value), perc_is_na = round(100 * is_na_total / n_total))


df_nas_plot <- df_nas %>% 
  filter(year >= 2004) %>%
  ungroup() %>% 
  group_by(country_name) %>% 
  mutate(is_color = max(perc_is_na) > 18)

df_nas_plot_color <- df_nas_plot %>% 
  filter(is_color == T)

palette <- wesanderson::wes_palette("Zissou1", n = df_nas_plot_color$country_name %>% unique() %>% length(), type = "continuous")

df_nas_plot_no_color <- df_nas_plot %>% 
  filter(!country_name %in% df_nas_plot_color$country_name)

# Plot the percentage of missing values in dataset (well, something is off here)
ggplot(df_nas_plot_no_color, aes(x = year, y = perc_is_na, color = country_name, group = country_name)) +
  geom_hline(yintercept = 0, size = 0.75) +
  geom_line(aes(color = "Other countries"), size = 0.75) +
  geom_point(aes(color = "Other countries"), size = 1.5, shape = 21, fill = "white", stroke = 1) +
  geom_line(data = df_nas_plot_color, aes(color = country_name), size = 0.75) +
  geom_point(data = df_nas_plot_color, aes(color = country_name), size = 1.5, shape = 21, fill = "white", stroke = 1) +
  scale_y_continuous(limits = c(-3, 103), expand = c(0, 0), labels = paste0(seq(0, 100, by = 25), "%")) +
  scale_x_continuous(expand = c(0, 0), limits = c(2008.5, 2019.5), breaks = seq(2009, 2019, by = 5)) +
  scale_color_manual(values = c(palette, "#e0e0e0"), breaks = c(df_nas_plot_color$country_name %>% unique(), "Other countries")) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    plot.subtitle = element_text(size = 8)
  ) +
  labs(
    x = "",
    y = "",
    title = "Percentage of missing values in dataset",
    subtitle = "Countries with the most missing values"
  )

ggsave("Plots/Percentage_of_missing_values_in_dataset_nuts2.png", dpi = 600, height = 15, width = 15, units = "cm", bg = "white")



