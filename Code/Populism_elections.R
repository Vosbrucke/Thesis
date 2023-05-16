# Regional electoral results by parties classification in European Union

# Load libraries
library(tidyverse)
library(rio)
library(ggplot2)

# Source: Schraff, Dominik, Vergioglou, Ioannis, and Demirci, Buket Buse. 2022. The European NUTS-Level Election Dataset: A Tool to Map the European Electoral Geography. Party Politics, Online First. DOI: 10.1177/13540688221083553
# Link: https://eu-ned.com/datasets/
# Load European countries codes
eu_countries <- read_csv("Raw_data/countries_eu_inflation.csv")

# Load election data
elections <- read_csv("Raw_data/eu_ned_joint.csv")

# Source: Rooduijn, M., Van Kessel, S., Froio, C., Pirro, A., De Lange, S., Halikiopoulou, D., Lewis, P., Mudde, C. & Taggart, P. (2019). The PopuList: An Overview of Populist, Far Right, Far Left and Eurosceptic Parties in Europe. 
# Link: www.popu-list.org.
# PopuList 2.0
populist <- import("https://popu-list.org/wp-content/uploads/2020/06/populist-2.0.xlsx")

# Add partyfacts_id's
populist_2b_fixed <- populist %>% 
  select(1:4, partyfacts_id, parlgov_id) %>% 
  filter(is.na(partyfacts_id)) %>% 
  as.tibble()

party_name_fixes <- tibble(
  party_name_short = c("Fi+KDNP", "LAM", "FIDESZ", "LR"),
  partyfacts_id = c(6366, 8647, 1691, 4871)
)

populist <- populist %>% 
  left_join(party_name_fixes, by = "party_name_short") %>% 
  mutate(partyfacts_id.x = ifelse(is.na(partyfacts_id.y), partyfacts_id.x, partyfacts_id.y)) %>% 
  select(-partyfacts_id.y) %>% 
  rename(partyfacts_id = partyfacts_id.x)


parties <- elections %>% 
  select(country, contains("party"), partyfacts_id) %>% 
  group_by(country, party_english, party_native, party_abbreviation, partyfacts_id) %>% 
  summarise() 


populist_2b_fixed %>% 
  left_join(parties %>% 
              group_by(party_english, party_abbreviation, partyfacts_id) %>% 
              summarise(), by = c("party_name_short" = "party_abbreviation"))


# Remove unnecessary variables
populist %<>%
  select(populist, populist:manifesto_id) %>% 
  filter(!is.na(partyfacts_id))


# Join regional (NUTS 2/3) election results with populist data set
elections_pop <- elections %>% 
  left_join(populist, by = "partyfacts_id") %>% 
  filter(country %in% c(eu_countries$country_name)) %>% 
  mutate(
    populist = ifelse(year >= populist_start & year <= populist_end, 1,0),
    farright = ifelse(year >= farright_start & year <= farright_end, 1,0),
    eurosceptic = ifelse(year >= eurosceptic_start & year <= eurosceptic_end, 1,0)
  ) %>% 
  select(1:15, populist, farright, eurosceptic) %>% 
  mutate(
    across(.cols = c(populist, farright, eurosceptic), ~ ifelse(is.na(.x), 0, .x)),
    vote_perc = partyvote / totalvote
  )

# Check which regions of Ireland need a fix
elections_pop %>% 
  filter(country == "Ireland", type == "EP") %>% 
  filter(year > 2003) %>% distinct(regionname)

# Fix Ireland regions code
elections_pop_ireland_fix <- elections_pop %>% 
  filter(country == "Ireland") %>% 
  select(-nuts2016, -nutslevel) %>% 
  inner_join(tibble(nutslevel = 3, nuts2016 = c("IE04", "IE05", "IE061", "IE062", "IE063"), regionname = c("North West", "South", "East", "Dublin", "Midlands North West")), by = "regionname") %>% 
  relocate(nuts2016, .after = nutslevel)

# Add correct Ireland data
elections_pop <- elections_pop %>% 
  filter(country != "Ireland") %>% 
  bind_rows(elections_pop_ireland_fix)

# Calculate sum different extremes among regions
elections_extremes <- elections_pop %>% 
  mutate(
    vote_perc_farright = farright * vote_perc,
    vote_perc_eurosceptic = eurosceptic * vote_perc, 
    vote_perc_populist = populist * vote_perc
  ) %>% 
  group_by(country, year, nuts2016, nutslevel, regionname, type) %>%
  summarise(
    sum_farright = sum(vote_perc_farright),
    sum_eurosceptic = sum(vote_perc_eurosceptic),
    sum_populist = sum(vote_perc_populist),
  )

# Make a palette for plot
palette <- wesanderson::wes_palette("Zissou1", n = elections_extremes %>% ungroup() %>% filter(country == "Poland", nutslevel == 2) %>% distinct(regionname) %>% nrow(), type = "continuous")

# Make a test for 2019 elections in Poland. The result is the percentage support of far right parties by nuts 2 regions
elections_extremes %>% 
  filter(country == "Poland") %>%
  # filter(nutslevel == 2) %>%  
  filter(type == "EP") %>%
  group_by(year) %>% 
  mutate(
    mean = mean(sum_eurosceptic)
    ) %>% 
  ggplot(aes(x = year, y = 100 * sum_eurosceptic)) +
  geom_line(aes(group = nuts2016, color = "1"), size = 0.25, alpha = 0.7) +
  geom_line(aes(y = 100 * mean, color = "2"), size = 0.5) + 
  # geom_hline(yintercept = 0, size = 1, color = "black") +
  scale_color_manual(values = c("lightgrey", "black"), labels = c("NUTS2 regions", "Year average"), name = "") +
  scale_y_continuous(limits = c(0, 105), expand = c(0, 0), labels = paste0(seq(0, 100, by = 25), "%")) +
  scale_x_continuous(breaks = seq(2004, 2019, by = 5), expand = c(0, 0)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x = element_line(color = "black", lineend = "round"),
    axis.line.x = element_line(color = "black", lineend = "round"),
    axis.ticks.length.x = unit(0.1, "in")
  ) +
  labs(
    x = "",
    y = "",
    title = "Percentage of eurosceptic votes by NUTS2 regions"
  )

write_csv(elections_extremes, "Processed_data/elections_extremes.csv")
