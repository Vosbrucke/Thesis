# Modelling 

# Load libraries
library(rvest)
library(tidyverse)
library(magrittr)
library(janitor)
library(rebus)
library(rio)
library(corrplot)
library(broom)


# Load full data set
joined_fully <- read_csv("Processed_data/joined_regional_data.csv")


# Load European countries codes
eu_countries <- read.csv("Raw_data/countries_eu_inflation.csv")


# Calculate correlation
correlation <- joined_fully %>% 
  select(year, geo, gdp_regional, disp_income) %>% 
  group_by(year, geo)


# Correlation with gdp_regional for econ_act_rate is equal 0.59, and for disposable_household_income it is equal to 0.83
correlation %>% 
  filter(!is.na(gdp_regional) && !is.na(disp_income)) %$% 
  cor(gdp_regional, disp_income)


# Create a corrplot
joined_fully %>% 
  drop_na() %>% 
  select(-c(1,2, contains("country"))) %>% 
  cor() %>% 
  corrplot(tl.cex = 0.3, type = "upper")


# Calculate missing values in each column
is_nas <- joined_fully %>% 
  filter(!is.na(country_name)) %>% 
  mutate(is_na = across(everything(), is.na)) %>% 
  select(contains("is_na")) %>% 
  summarise(colSums(.)) %>% 
  mutate(name = colnames(joined_fully)) %>% 
  rename(na_sum = "colSums(.)") %>%
  # In total there are 6563 observations. Remove those columns where there are more than 1641 missing observations (25%)
  filter(na_sum < 1641)

# Pick only columns with lower number of missing values
joined_non_na <- joined_fully %>% 
  select(is_nas$name)


# Load elections data set for nuts3 regions
elections_pop <- read_csv("Processed_data/elections_pop_nuts3.csv") %>% 
  filter(year >= 2000) %>% 
  filter(type == "EP")


# Calculate percentage votes given on extreme parties
elections_extremes <- elections_pop %>% 
  mutate(
    vote_perc_farright = farright * vote_perc,
    vote_perc_eurosceptic = eurosceptic * vote_perc, 
    vote_perc_populist = populist * vote_perc
    ) %>% 
  group_by(country, year, nuts2016, regionname) %>%
  summarise(
    sum_farright = sum(vote_perc_farright),
    sum_eurosceptic = sum(vote_perc_eurosceptic),
    sum_populist = sum(vote_perc_populist)
    )

write_csv(elections_extremes, "Processed_data/Extremes_nuts3.csv")

# All parties joined
joined_elections_pop <- elections_pop %>% 
  full_join(joined_non_na, by = c("nuts2" = "geo", "year")) %>% 
  arrange(nuts2, year)


# Extremes joined
joined_elections_extremes <- elections_extremes %>% 
  full_join(joined_non_na, by = c("nuts2" = "geo", "year")) %>% 
  filter(year < 2020, country_name != "Turkey") %>%
  arrange(nuts2, year) 

df_lagged <- joined_elections_extremes %>% 
  group_by(nuts2) %>% 
  mutate(
    # lag_1_gdp_regional = gdp_regional / lag(gdp_regional),
    # lag_2_gdp_regional = gdp_regional / lag(gdp_regional, n = 2),
    # lag_3_gdp_regional = gdp_regional / lag(gdp_regional, n = 3),
    lag_4_gdp_regional = gdp_regional / lag(gdp_regional, n = 4),
    # lag_1_econ_act_rate = econ_act_rate / lag(econ_act_rate),
    # lag_2_econ_act_rate = econ_act_rate / lag(econ_act_rate, n = 2),
    # lag_3_econ_act_rate = econ_act_rate / lag(econ_act_rate, n = 3),
    lag_4_econ_act_rate = econ_act_rate / lag(econ_act_rate, n = 4)
    ) %>% 
  select(
    1:9,
    HTC,
    12,
    15,
    16,
    21,
    22
  ) %>% 
  # Remove data with years that are not used in the analysis
  filter(!is.na(country))
  # mutate(lagged = across(.cols = c("gdp_regional", "econ_act_rate", "HRST", "HTC", "participation_in_formal_and_or_non_formal_training_25_to_64_year_olds_population", "FMEDAGEPOP", "MEDAGEPOP", "MMEDAGEPOP"), ~ lag(.x)))

# Save the data set
write_csv(df_lagged, "Processed_data/df_to_modelling.csv")


# Read the data set
df_lagged <- read_csv("Processed_data/df_to_modelling.csv")


# Modelling preparation
for (i in seq(2004, 2019, by = 5)) {
  df_lagged_model <- df_lagged %>% 
    filter(year == i)
  
  # Create a correlation plot
  cor_tibble <- cor(df_lagged_model[, c(7, 8:15)], use = "complete.obs")
  
  # Create a png file
  png(paste0("Plots/correlation_plot_", i,".png"), width = 20, height = 20, units = "cm", res = 600)
  
  p1 <- {
    corrplot(cor_tibble, 
             method = "number",
             tl.cex = 0.4,
             type = "upper"
             )
    recordPlot()
  }
  
  # Save the plot
  dev.off()
}

# Prepare a vector of independent variables
r_side <- colnames(df_lagged_model)[8:15]

# Column names to check in formula: sum_farright, sum_eurosceptic, sum_populist
fmla <- as.formula(paste("sum_populist ~ ", paste(r_side, collapse= "+")))


# Make a function to calculate r^2 for linear regression model per year
modelling_per_year_r <- function(i) {
  lm_model <- lm(fmla, df_lagged %>% 
                   filter(!is.na(country)) %>% 
                   filter(year == i)) 
  glance(summary(lm_model))$r.squared
}

# Apply the function
vector <- sapply(seq(2004, 2019, by = 5), modelling_per_year_r)


# Make a function to calculate linear regression model per year
modelling_per_year <- function(i) {
  lm_model <- lm(fmla, df_lagged %>% 
                   filter(year == i)) 
  summary(lm_model)
}

# Apply the function
vector_lr <- lapply(seq(2004, 2019, by = 5), modelling_per_year)

# Apply names
names(vector_lr) <- paste("Test for", seq(2004, 2019, by = 5))

# Save results in txt format
sink("Results/lr.txt")
print(vector_lr)
sink()


# Visualize the political extreme situation for Hungary
visualization_elections <- elections_extremes %>% 
  filter(country == "Ireland")

visualization_elections %>% 
  ggplot(aes(year, sum_eurosceptic)) +
  geom_line(aes(color = nuts2)) +
  geom_point(size = 1.5, aes(fill = nuts2), shape = 21, color = "white", stroke = 1) +
  labs(
    x = "",
    y = "",
    title = paste("Votes percentage on eurosceptic parties in", unique(visualization_elections$country), "\nin elections to European Parliament")
  ) +
  scale_x_continuous(breaks = seq(2004, 2019, by = 5)) +
  scale_y_continuous(limits = c(-0.01, 1.01), expand = c(0, 0)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    title = element_text(face = "bold")
  )

ggsave("Plots/Votes_percentage_Hungary.png", width = 15, height = 15, units = "cm", dpi = 600, bg = "white")

elections_extremes %>% 
  ggplot(aes(year, sum_populist, group = nuts2)) +
  geom_line(color = "grey") +
  geom_point(color = "white", fill = "grey", shape = 21, stroke = 0.5, size = 1) +
  labs(
    x = "",
    y = "",
    title = paste("Votes percentage on populism parties in elections to European Parliament")
  ) +
  scale_x_continuous(breaks = seq(2004, 2019, by = 5)) +
  scale_y_continuous(limits = c(-0.01, 1.01), expand = c(0, 0)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    title = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")
  ) +
  facet_wrap(~country, ncol = 9)

ggsave("Plots/Votes_percentage_by_country.png", width = 25, height = 15, units = "cm", dpi = 600, bg = "white")
