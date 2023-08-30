## Set working directory ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load libraries
library(tidyverse)
library(tidycensus)

# Get county migration data
county_data <-
get_flows(geography = "county",
          year = 2019,
          variables = c("MOVEDIN", "MOVEDOUT"),
          output = "wide")

states_regions <-
tibble(state.name, state.region)

state_mig <-
county_data %>% 
  filter(str_detect(FULL2_NAME, states_regions$state.name)) %>%
  separate(FULL1_NAME, c("county_from", "state_from"), ",") %>%
  mutate(state_from = str_squish(state_from)) %>%
  separate(FULL2_NAME, c("county_to", "state_to"), ",") %>%
  mutate(state_to = str_squish(state_to)) %>%
  mutate_at(vars(starts_with("county")), str_remove_all, " County") %>%
  select(starts_with("state"), MOVEDIN, MOVEDOUT) %>%
  group_by(state_from, state_to) %>%
  summarise(across(everything(), sum)) %>%
  ungroup() %>%
  filter(!str_detect(state_to, "County")) %>%
  left_join(states_regions, by = c("state_from" = "state.name")) %>%
  rename(region = state.region)

movedin_data <- 
  state_mig %>%
  select(from = state_to,
         to = state_from,
         migcount = MOVEDIN,
         region) %>%
  arrange(from, to)

movedout_data <- 
  state_mig %>%
  select(from = state_from,
         to = state_to,
         migcount = MOVEDOUT,
         region) %>%
  arrange(from, to)

migration <- 
bind_rows(movedin_data, 
          movedout_data) %>%
  mutate(region = as.character(region)) %>%
  mutate(region = replace_na(region, "None")) %>%
  mutate_if(is.character, as.factor) 
