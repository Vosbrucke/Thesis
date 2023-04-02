# Regional electoral results by parties classification in Europe Union

# Load regional election data set
elections <- read_csv("Raw_data/eu_ned_ep_nuts2.csv")

elections <- read_csv("/Users/mr.fox/Desktop/eu_ned_joint.csv")


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
    # type == "EP"
    # nutslevel == 2
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

# Fix Ireland regions code
elections_pop_ireland_fix <- elections_pop %>% 
  filter(country == "Ireland") %>% 
  # select(-nuts2) %>%
  inner_join(tibble(nuts2 = c("IE04", "IE05", "IE061", "IE062", "IE063"), regionname = c("North West", "South", "East", "Dublin", "Midlands North West")), by = "regionname") %>% 
  relocate(nuts2, .after = nutslevel)

elections_pop <- elections_pop %>% 
  filter(country != "Ireland") %>% 
  bind_rows(elections_pop_ireland_fix)

# write_csv(elections_pop, "Processed_data/elections_pop_nuts_3.csv")

elections_pop %>% 
  filter(type != "EP")

test <- elections_pop %>% 
  # filter(country == "Poland") %>%
  # filter(nutslevel == 2) %>% 
  group_by(country, year, populist, type, nutslevel) %>% 
  summarise(mean = mean(vote_perc)) %>% 
  filter(populist == 1) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "type", values_from = mean) %>% 
  drop_na()
test %$%
cor(Parliament, EP)

# Make a test for 2019 elections in Poland. The result is the percentage support of far right parties by nuts 2 regions
test <- elections_pop %>% 
  # filter(country == "Poland") %>%
  # filter(nutslevel == 2) %>% 
  group_by(country, year, populist, type, nutslevel) %>% 
  summarise(mean = mean(vote_perc)) %>% 
  filter(populist == 1) %>% 
  ungroup()

test %>% 
  ggplot(aes(x = as.factor(year), y = mean)) +
  geom_boxplot(aes(color = type)) +
  scale_y_continuous(limits = c(0,1)) +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  labs(
    x = "",
    y = "",
    title = "Średnia głosów oddana na populistyczne partie w wyborach\nkrajowych (Parliament) i do parlamentu europejskiego (EP)"
  ) +
  facet_wrap(~ country)

ggsave("Mean_populist_votes_1991-2021.png", dpi = 900, width = 25, height = 25, units = "cm")

write_csv(elections_pop, "Processed_data/elections_pop_nuts3.csv")
