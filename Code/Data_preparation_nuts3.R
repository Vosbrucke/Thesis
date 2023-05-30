# Data preparation NUTS3

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

# Data on employees in regions 
df_employment <- read_csv("Raw_data/Nuts3/Employment_by_thousands_of_people.csv") %>% 
  mutate(country_code = str_sub(geo, 1, 2)) %>% 
  full_join(eu_countries, by = c("country_code" = "code")) %>% 
  group_by(unit, geo) %>%
  set_colnames(make_clean_names(colnames(.))) %>% 
  arrange(geo, time_period) %>% 
  ungroup() %>% 
  select(geo, time_period, employment = obs_value)

# Reading population data on feminity and elderly rates
df_pop_groups <- read_csv("Raw_data/Nuts3/Population_with_groups.csv") %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  dplyr::mutate(country_code = str_sub(geo, 1, 2)) %>% 
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
  full_join(df_employment, by = c("geo", "time_period")) %>% 
  mutate(employment_rate = employment * 1000 / total) %>% 
  select(country_code, geo, time_period, population = total, employment, employment_rate, ge65, elder_rate, fem_rate)

# GDP of regions
df_gdp <- read_csv("Raw_data/Nuts3/GDP.csv") %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  select(-obs_flag) %>% 
  full_join(df_pop_groups, by = c("geo", "time_period")) %>% 
  select(country_code, geo, time_period, gdp = obs_value, population, employment, employment_rate, ge65, elder_rate, fem_rate)

# Migration in regions
df_migration <- read_csv("Raw_data/Nuts3/Rate_of_migration.csv") %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  select(-obs_flag) %>% 
  # Filter for crude rate of net migration 
  filter(indic_de == "CNMIGRATRT") %>% 
  full_join(df_gdp, by = c("geo", "time_period")) %>% 
  select(country_code, geo, time_period, gdp, population, employment, employment_rate, ge65, elder_rate, fem_rate, migration_rate = obs_value)

# Population density in regions
df_density <- read_csv("Raw_data/Nuts3/Pop_density.csv") %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  select(-obs_flag) %>% 
  full_join(df_migration, by = c("geo", "time_period")) %>% 
  select(country_code, geo, time_period, gdp, population, employment, employment_rate, ge65, elder_rate, fem_rate, migration_rate, pop_density = obs_value) %>% 
  group_by(geo) %>%
  # Add lagged variables of economic factors and a social factor
  dplyr::mutate(
    gdp_lag_4 = gdp / dplyr::lag(gdp, n = 4),
    employment_rate_lag_4 = employment_rate / dplyr::lag(employment_rate, n = 4),
    migration_rate_lag_4 = migration_rate / dplyr::lag(migration_rate, n = 4)
  ) %>% 
  # Remove infinite 
  mutate(migration_rate_lag_4 = ifelse(migration_rate_lag_4 %in% c("-Inf", "Inf"), migration_rate, migration_rate_lag_4)) %>% 
  # Remove unnecessary countries and years
  filter(country_code %in% eu_countries$code) %>% 
  filter(time_period > 2003)

# Elections data frame
df_elections <- read_csv("Processed_data/elections_extremes.csv") %>% 
  filter(type == "EP", year >= 2004) %>% 
  filter(!year %in% c(2007, 2013)) %>% 
  group_by(nuts2016) %>% 
  # Add a column for growth rate
  dplyr::mutate(
    growth = dplyr::across(dplyr::contains("sum"), ~ ifelse(!is.na(. / dplyr::lag(.)), . / dplyr::lag(.), 0))
  ) %>% 
  ungroup() %>% 
  # Add a column for deviation from the mean
  dplyr::mutate(
    dev_mean = dplyr::across(dplyr::contains("sum"), ~ (. - mean(., na.rm=TRUE)) * 100)
  ) %>% 
  unnest(cols = c(growth, dev_mean), names_sep = "_") %>% 
  # Change 'Inf' values to the new value
  mutate(dplyr::across(dplyr::contains("growth"), ~ ifelse(. == "Inf", sum_eurosceptic, .))) %>% 
  ungroup()

# Join elections data frame with variables
df <- df_elections %>% 
  group_by(country, year) %>% 
  full_join(df_density, by = c("nuts2016" = "geo", "year" = "time_period")) %>% 
  group_by(nuts2016) %>% 
  mutate(
    growth_eurosceptic_p_perc = 100 * (sum_eurosceptic - dplyr::lag(sum_eurosceptic)),
    growth_populism_p_perc = 100 * (sum_populist - dplyr::lag(sum_populist)),
    growth_farright_p_perc = 100 * (sum_farright - dplyr::lag(sum_farright))
  ) %>% 
  # Remove abrod votes
  filter(regionname != "Abroad votes", nutslevel == 3) %>% 
  filter(!str_detect(nuts2016, "FRZZZ|FRY")) %>% 
  relocate(country_code, .before = country) %>% 
  # Change gdp value to gdp per capita
  mutate(gdp = gdp / population)

# Write the df
write_csv(df, "Processed_data/df.csv")

# Checking how many NAs there are in the data frame per year
df_lagged_na <- function(i) {
  df %>% 
    filter(year == i) %>% 
    select(-contains("dev")) %>% 
    ungroup() %>% 
    mutate(is_na = across(everything(), is.na)) %>% 
    select(contains("is_na")) %>% 
    dplyr::summarise(colSums(.)) %>% 
    mutate(name = colnames(df %>% select(-contains("dev")))) %>%
    rename(na_sum = "colSums(.)") %>% 
    select(name, na_sum) %>% 
    arrange(desc(na_sum))
}

# Apply function to find in which years we can find the most missing values
df_lagged_na_res <- lapply(seq(2004, 2019, by = 5), df_lagged_na)

# Apply names for lists
names(df_lagged_na_res) <- paste("Number of missing values for the year of", seq(2004, 2019, by = 5))

# View the list
df_lagged_na_res 

sink("Results/NA's_full_join.txt")
print(df_lagged_na_res)
sink()


# Adding missing data 
palette <- wesanderson::wes_palette("Zissou1", n = 5)[1, 5]

for (i in seq(2004, 2019, by = 5)) {
  
  df_loop <- df %>% filter(year == i)
  
  country_list <- df_loop %>% pull(country) %>% unique()
  
  nas_plot <- function(x) {
  png(file = paste0("Plots/Missing_data/", i, "/Missing_data_", i, "_", country_list[x], ".png"), res = 900, units = "cm", width = 30, height = 30)
  aggr(df_loop %>% ungroup() %>% filter(country == country_list[x]), col=c(palette[1], palette[5]),
                     numbers=TRUE, sortVars=TRUE,
                     labels=names(df), cex.axis=.7,
                     gap=3, ylab=c("Missing data","Pattern"), main = "Test")
  dev.off()
  }
  
  lapply(1:length(country_list), nas_plot)
  
}

df_nas_count <- df %>% 
  group_by(year, country) %>% 
  count() %>% 
  ungroup()

df_nas <- df %>% 
  group_by(year, country) %>% 
  summarise(across(everything(), ~sum(is.na(.)), .names = "perc_is_na_{.col}"))# %>% 

df_nas <- df_nas %>% 
  group_by(year, country) %>% 
  bind_cols(df_nas_count %>% select(3)) %>%
  pivot_longer(cols = 3:30) %>% 
  summarise(n_total = sum(n), is_na_total = sum(value), perc_is_na = round(100 * is_na_total / n_total))


# Plot the percentage of missing values in dataset
df_nas_plot <- df_nas %>% 
  filter(year >= 2004) %>%
  ungroup() %>% 
  group_by(country) %>% 
  mutate(is_color = max(perc_is_na) > 18)

palette <- wesanderson::wes_palette("Zissou1", n = sum(df_nas_plot[df_nas_plot$year == 2019, ]$is_color), type = "continuous")

df_nas_plot_color <- df_nas_plot %>% 
  filter(is_color == T)

df_nas_plot_no_color <- df_nas_plot %>% 
  filter(!country %in% df_nas_plot_color$country)


ggplot(df_nas_plot_no_color, aes(x = year, y = perc_is_na, color = country, group = country)) +
  geom_hline(yintercept = 0, size = 0.75) +
  geom_line(aes(color = "Other countries"), size = 0.75) +
  geom_line(data = df_nas_plot_color, aes(color = country), size = 0.75) +
  geom_point(aes(color = "Other countries"), size = 1.5, shape = 21, fill = "white", stroke = 1) +
  geom_point(data = df_nas_plot_color, aes(color = country), size = 1.5, shape = 21, fill = "white", stroke = 1) +
  scale_y_continuous(limits = c(-3, 103), expand = c(0, 0), labels = paste0(seq(0, 100, by = 25), "%")) +
  scale_x_continuous(expand = c(0, 0), limits = c(2003.5, 2019.5), breaks = seq(2004, 2019, by = 5)) +
  scale_color_manual(values = c(palette, "#e0e0e0"), breaks = c(df_nas_plot_color$country %>% unique(), "Other countries")) +
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

ggsave("Plots/Percentage_of_missing_values_in_dataset.png", dpi = 600, height = 15, width = 15, units = "cm", bg = "white")


# for (i in seq(2004, 2019, by = 5)) {
#   
#   df_loop <- df %>% filter(year == i)
#   
#   country_list <- df_loop %>% pull(country) %>% unique()
#   
#   nas_plot <- function(x) {
#     
#     aggr(df_loop %>% ungroup() %>% filter(country == country_list[x]), col=c(palette[1], palette[5]),
#          numbers=TRUE, sortVars=TRUE,
#          labels=names(df), cex.axis=.7,
#          gap=3, ylab=c("Missing data","Pattern"), main = "Test")
#     dev.off()
#   }
#   
#   lapply(1:length(country_list), nas_plot)
#   
# }


imputed_data <- mice(df, m=5, maxit = 50, method = 'pmm', seed = 500)

summary(imputed_data)


# Define right side of the modelling equation
r_side <- df %>% as.data.frame() %>% 
  select(15, 16, 18:26, 28) %>%
  colnames() 

r_side


# Check the structure and distribution of the variables
variable_distribution <- function(i) {
t <- df %>% 
  filter(year == i) %>% 
  ungroup() %>% 
  select(8:29, -contains("growth")) %>% 
  GGally::ggpairs(title = "Distribution and correlation among variables")
  
ggsave(paste0("Plots/Variables_distribution_in_", i, ".png"), t, dpi = 900, height = 30, width = 30, units = "cm")
}

lapply(seq(2004, 2019, by = 5), variable_distribution)

# Check distribution on a plot
qqplots_year <- function(i) {

  my_qqplot <- function(.data, .title) {
    ggplot(data = .data, mapping = aes(sample = value)) +
      stat_qq_band(alpha=0.5) +
      stat_qq_line() +
      stat_qq_point() +
      facet_wrap(~ name, scales = "free") +
      labs(
        x = "",
        y = "",
        title = ""
      )
  }

plot_list <- df %>% 
  ungroup() %>% 
  filter(year == i) %>% 
  select(-c(1:7)) %>% 
  pivot_longer(cols = everything()) %>% 
  split(.$name) %>% 
  imap(my_qqplot) %>% 
  append(plots, .)

plot <- cowplot::plot_grid(plotlist = plot_list, label_x = "Theoretical Quantiles", label_y = "Sample Quantiles")

ggsave(paste0("Plots/Normality_tests/plot_list_", i, ".png"), plot, dpi = 600, width = 30, height = 30, bg = "white")
}

lapply(seq(2004, 2019, by = 5), qqplots_year)


# Shapiro normality test
shapiro_test_per_year <- function(i) {
  
  if(i == 2004) {
    t <- df %>% ungroup() %>% filter(year == i) %>% select(8:29, -contains("growth")) %>% as.tibble()
  } else {
    t <- df %>% ungroup() %>% filter(year == i) %>% select(8:29) %>% as.tibble()
  }
  
  imap_dfr(t, 
           ~ shapiro.test(.x) %>% 
             (\(st) tibble(var = .y,
                           W = st$statistic,
                           p.value = st$p.value,
                           non_normal = p.value < 0.05))())
}

# Apply the function
vector_st <- lapply(seq(2004, 2019, by = 5), shapiro_test_per_year)

# Apply names
names(vector_st) <- paste("Shapiro normality test for", seq(2004, 2019, by = 5))

# Save results in txt format
sink("Results/Normality_test.txt")
for(i in vector_st){print(i, n = "all")}
sink()

# As we can see in the tests we should apply logarithmic transformation to all the variables
df_log <- df %>% 
  ungroup() %>% 
  mutate(across(8:29, log)) 

# Test 
shapiro.test(df_log$population)
shapiro.test(df$population)

# Test on a plot
ggplot(data = df_log, mapping = aes(sample = population)) +
  stat_qq_band(alpha=0.5) +
  stat_qq_line() +
  stat_qq_point() +
  labs(
    x = "",
    y = "",
    title = ""
  )

# Write the log df
write_csv(df_log, "Processed_data/df_log.csv")

