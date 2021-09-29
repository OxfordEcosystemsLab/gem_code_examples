install.packages("BIEN")
library(BIEN)
library(tidyverse)

## Vector of species names
species_vector <- c("Afzelia africana", "Albizia zygia", "Celtis tessmannii")
species_vector

## retrieve the trait values on BIEN
traits_bien <- BIEN_trait_species(species_vector)
View(traits_bien)

## compute the median trait value
traits_bien %>% 
  tibble() %>%
  filter(trait_name == "stem wood density") %>% 
  arrange(scrubbed_species_binomial) %>% 
  select(species = scrubbed_species_binomial, trait_value) %>%
  mutate(trait_value = as.numeric(trait_value)) %>%
  group_by(species) %>% 
  summarize(trait_median = median(trait_value, na.rm = T)) 

## Extract directly a specific trait value
wd <- BIEN_trait_traitbyspecies(species = species_vector, trait = "stem wood density")
View(wd)

wd %>% tibble() %>%
  filter(trait_name == "stem wood density") %>% 
  arrange(scrubbed_species_binomial) %>% 
  select(species = scrubbed_species_binomial, trait_value) %>%
  mutate(trait_value = as.numeric(trait_value)) %>%
  group_by(species) %>% 
  summarize(trait_median = median(trait_value, na.rm = T)) 

## Infer trait value for absent species
species_absent <- c("Cola sp1", "Brachystegia sp1")
mean_absent <- BIEN_trait_mean(species = species_absent, trait = "stem wood density")
View(mean_absent)
