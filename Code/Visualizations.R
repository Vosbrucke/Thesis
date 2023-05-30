# Graph visualizations
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
library(lmtest)

# Read data with spatial geometry
df_shp <- st_read("Processed_data/df_shp.shp") %>% #, driver = "ESRI Shapefile", append = FALSE, geometry_column = "geometry", stringAsFactors = FALSE)
  # For some reason with sf package two columns get relocated to the end of the data frame
  dplyr::relocate(c(36, 37), .before = "LEVL_CO") 

# Read data with column names (shapefiles are limited to 10 characters- column names are abbreviated)
df_shp_colnames <- read_csv("Processed_data/df_shp_colnames.csv") %>% pull()

# Set correct column names
df_shp %<>% 
  set_colnames(df_shp_colnames) %>% 
  select(-contains("dev"), -country_code, -c(32:38), -c(5)) %>% 
  as.data.frame()

ggplot(df_shp, aes(x = year, y = 100 * sum_eurosceptic)) +
  stat_summary(fun = "mean", geom = "line") +
  stat_summary(fun = "mean", geom = "point") +
  # geom_line(color = "grey", alpha = 0.8) +
  # geom_point(size = 1.5, shape = 21, fill = "white", stroke = 0.75, color = "grey") +
  geom_hline(yintercept = 0, size = 1) +
  labs(
    x = "",
    y = "",
    title = "Support of eurosceptic parties in EU Member States"
  ) +
  # scale_fill_viridis_d() +
  # scale_color_viridis_d() +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(breaks = seq(2004, 2019, by = 5)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "none"
  ) +
  facet_wrap(~ country, ncol = 5)
  
