library(tidytuesdayR)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyterra)

tt_data <- tt_load("2023-01-10")
counts <- tt_data[[1]]
sites <- tt_data[[2]]
world <- ne_download(type = "NE1_LR_LC",
                     scale = 10,
                     category = "raster")
if(!file.exists("2023-01-10/maps/ne_110m_coastline.shp")){
  coasts <- ne_download(type = "coastline",
                     category = "physical",
                     destdir = "2023-01-10/maps/",
                     load = FALSE)
}

site_locs <- counts %>%
  distinct(loc_id, .keep_all = T) %>%
  select(c(loc_id, latitude, longitude))

loc_info <- sites %>%
  pivot_longer(cols = starts_with("yard_type"),
               names_to = "yard_type",
               names_prefix = "yard_type_") %>%
  filter(value == 1) %>%
  select(c(loc_id, yard_type)) %>%
  distinct(loc_id, .keep_all = T) %>%
  left_join(y = site_locs,
            by = "loc_id") %>%
  filter(!is.na(longitude)) %>%
  mutate(yard_type = case_when(
    yard_type == "desert" ~ "Desert",
    yard_type == "garden" ~ "Garden",
    yard_type == "landsca" ~ "Landscape",
    yard_type == "pavement" ~ "Pavement",
    yard_type == "woods" ~ "Woods"
  ))

write_csv(loc_info,
          file = "2023-01-10/data/loc_info.csv")

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
 