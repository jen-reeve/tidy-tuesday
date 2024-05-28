library(tidytuesdayR)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyterra)

tt_data <- tt_load("2023-01-10")
counts <- tt_data[[1]]
sites <- tt_data[[2]]

species <- "amegfi"
species_counts <- counts %>%
  filter(species_code == species,
         Year == 2020)

ggplot() +
  geom_spatvector(data = coasts) +
  # geom_spatraster(data = world) +
  # scale_fill_wiki_c(guide = "none") +
  geom_point(data = species_counts,
             aes(x = longitude,
                 y = latitude,
                 size = how_many),
             alpha = 0.5) +
  scale_color_brewer(palette = "Dark2",
                     guide = guide_legend(title = "Yard Type")) +
  lims(x = c(-160, -50),
       y = c(20, 70)) +
  labs(x = "Longitude",
       y = "Latitude") +
  theme_classic()
 