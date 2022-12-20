# Regional electoral results by parties classification in Europe Union

# Load regional election data set
elections <- read_csv("Raw_data/eu_ned_ep_nuts2.csv")


# Load European countries codes
eu_countries <- read.csv("Raw_data/countries_eu_inflation.csv")


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
  filter(
    year >= 1990,
    type == "EP"
  ) %>% 
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

# Make a test for 2019 elections in Poland. The result is the percentage support of far right parties by nuts 2 regions
elections_pop %>% 
  filter(country == "Poland") %>%
  group_by(country, year, nuts2, farright) %>% 
  summarise(sum = sum(vote_perc)) %>% 
  filter(farright == 1) %>% 
  ungroup() %>% 
  ggplot(aes(x = year, y = sum, color = nuts2)) +
  geom_line() +
  theme_minimal() +
  labs(
    x = "",
    y = "",
    title = "Percentage of farright votes, by nuts-2 regions"
  )

write_csv(elections_pop, "Processed_data/elections_pop.csv")
