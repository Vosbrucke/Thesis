# Spatial visualization nuts 3

# Load libraries
library(tidyverse)
library(magrittr)
library(ggtext)
library(sf)
library(plm)
library(zoo)

# Load shapefile of regions- it's done so map contains shade of other countries not listed in data
shp_0 <- st_read("Raw_data/NUTS_RG_60M_2016_3035/NUTS_RG_60M_2016_3035.shp")

# Retrieve coordinates in matrix form
coordinates <- st_coordinates(shp_0)

# Construct neighbors list from polygon list
queen_neighbour <- poly2nb(shp_0, queen = T)

# Check which regions had no neighbors
which_regions <- data.frame(is_neighbor = lapply(queen_neighbour, sum) > 0)

# Remove regions with no neighbor in shapefile
shp_0 %<>% 
  bind_cols(which_regions) %>% 
  filter(is_neighbor == T)

# Write data with spatial geometry
df_shp <- st_read("Processed_data/df_shp.shp", driver = "ESRI Shapefile", append = FALSE)

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
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
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

# Mapping the variables
{
  # Mapping eurosceptism in the selected countries
  map_per_year_eu_scept_dv <- function(i) {
    ggplot(df_shp %>% filter(year == i)) +
      geom_sf(data = df_shp %>% filter(!CNTR_CODE %in% c("FR", "PT")), fill = "lightgrey", color = "white") +
      geom_sf(aes(fill = dev_mean$sum_eurosceptic), color = "white", linewidth = 0.1) +
      scale_fill_gradient2(limits = c(-40, 40), low = "#3B9AB2", mid = "#F5F5F2", high = "#F21A00") +
      # scale_fill_gradient2(low = "#3B9AB2", mid = "#F5F5F2", high = "#F21A00") +
      # (limits = c(-40,40), option = "magma", direction = -1) +
      labs(
        x = "", 
        y = "", 
        title = paste("Country level deviation from mean of votes on\neurosceptism parties in elections to European Parliament", "in", i),
      ) +
      theme_map() +
      guides(fill = guide_colourbar(title = "Deviation in ppt"))
    
    ggsave(paste0("Plots/Election_results_distribution/Eurosceptism_sd_spatial_distribution_in_", i, ".png"), bg = "white", dpi = 900, width = 20, height = 15, units = "cm")
  }
  
  # Create a plot for each elections separately
  lapply(seq(2004, 2019, by = 5), map_per_year_eu_scept_dv)
  
  # Plot without deviation
  map_per_year_eu_scept <- function(i) {
    ggplot(shp_1 %>% filter(year == i)) +
      geom_sf(data = shp_0 %>% filter(!CNTR_CODE %in% c("FR", "PT")), fill = "#ECECE9", color = "white") +
      geom_sf(aes(fill = sum_eurosceptic), color = "white", linewidth = 0.1) +
      scale_fill_viridis_c(limits = c(0,1), option = "magma", direction = -1) +
      labs(
        x = "", 
        y = "", 
        title = paste("Percentage of votes on eurosceptism parties\nin elections to European Parliament", "in", i),
      ) +
      theme_map() +
      guides(fill = guide_colourbar(title = "Percentage of votes"))
    
    ggsave(paste0("Plots/Election_results_distribution/Eurosceptism_spatial_distribution_in_", i, ".png"), dpi = 900, width = 20, height = 15, units = "cm")
  }
  
  lapply(seq(2004, 2019, by = 5), map_per_year_eu_scept)
  
  # Mapping farright in the selected countries
  map_per_year_farright <- function(i) {
    ggplot(shp %>% filter(year == i)) +
      geom_sf(data = shp_0 %>% filter(!CNTR_CODE %in% c("FR", "PT")), fill = "#ECECE9", color = "white") +
      geom_sf(aes(fill = sum_farright), color = "white", linewidth = 0.1) +
      scale_fill_viridis_c(limits = c(0,1), option = "magma", direction = -1) +
      labs(
        x = "", 
        y = "", 
        title = paste("Spatial distribution of percentage of votes on farright parties\nin elections to European Parliament", "in", i),
      ) +
      theme_map() +
      guides(fill = guide_colourbar(title = "Percentage of votes"))
    
    ggsave(paste0("Plots/Election_results_distribution/Farright_spatial_distribution_in_", i, ".png"), dpi = 900)
  }
  
  lapply(seq(2004, 2019, by = 5), map_per_year_farright)
  
  # Mapping populism in the selected countries
  map_per_year_populism <- function(i) {
    ggplot(shp %>% filter(year == i)) +
      geom_sf(data = shp_0 %>% filter(!CNTR_CODE %in% c("FR", "PT")), fill = "#ECECE9", color = "white") +
      geom_sf(aes(fill = sum_populist), color = "white", linewidth = 0.1) +
      scale_fill_viridis_c(limits = c(0,1), option = "magma", direction = -1) +
      labs(
        x = "", 
        y = "", 
        title = paste("Spatial distribution of percentage of votes on populist parties\nin elections to European Parliament", "in", i),
      ) +
      theme_map() +
      guides(fill = guide_colourbar(title = "Percentage of votes"))
    
    ggsave(paste0("Plots/Election_results_distribution/Populism_spatial_distribution_in_", i, ".png"), dpi = 900)
  }
  
  lapply(seq(2004, 2019, by = 5), map_per_year_populism)
}

# Other plots
{
  ggplot(shp_1) +
    geom_line(aes(x = year, y = dev_mean$sum_eurosceptic, color = NUTS_ID), show.legend = F, alpha = 0.25) +
    # scale_y_continuous(limits = c(0, 1)) + 
    facet_wrap(~country) +
    scale_color_viridis_d("plasma") +
    labs(x = "", y = "", title = "Percentage of votes on populist parties", subtitle = "Populist identification based on The PopuList ")
  
  
  pallet <- wesanderson::wes_palette("Darjeeling1", n = 4)
  ggplot(shp) +
    geom_violin(aes(x = as.factor(year), y = sum_populist, group = as.factor(year), color = as.factor(year)), fill = "white") +
    geom_violin(aes(x = as.factor(year), y = sum_populist, group = as.factor(year), fill = as.factor(year)), alpha = 0.3) +
    scale_y_continuous(limits = c(0, 1)) +
    facet_wrap(~country) +
    scale_color_manual(values = pallet) +
    scale_fill_manual(values = pallet)
}
