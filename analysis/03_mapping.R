# Load Council district and Baltimore City boundary list from local source
load("~/Projects/2020_active-projects/packages/mapbaltimore/data/council_districts.rda")
load("~/Projects/2020_active-projects/packages/mapbaltimore/data/baltimore_city.rda")
load("~/Projects/2020_active-projects/packages/mapbaltimore/data/baltimore_city_detailed.rda")

library(ggplot2)

map_theme <- theme_minimal(base_size = 18,
                           base_family = "Roboto Condensed") +
  theme(
    panel.grid.major = element_line(color = "transparent"),
    axis.title = element_text(color = "transparent"),
    axis.text = element_text(color = "transparent"))

council_districts %>%
  left_join(property_summary_by_district_2021, by = c("name" = "area")) %>%
  ggplot() +
  geom_sf(aes(fill = num_advertised), color = "white", size = 1.25) +
  geom_sf(data = baltimore_city, color = "gray25", fill = NA, size = 1) +
  scale_fill_viridis_c() +
  labs(fill = "Number of properties\nadvertised for tax sale") +
  map_theme

ggsave("num_advertised_by_council_district_2021.png", width = 11, height = 8.5, dpi = 120)

council_districts %>%
  left_join(property_summary_by_district_2021, by = c("name" = "area")) %>%
  ggplot() +
  geom_sf(aes(fill = num_owner_occupied_not_vacant), color = "white", size = 1.25) +
  geom_sf(data = baltimore_city, color = "gray25", fill = NA, size = 1) +
  scale_fill_viridis_c() +
  labs(fill = "Number of owner occupied properties\nadvertised for tax sale") +
  map_theme

ggsave("num_owner_occupied_not_vacant_by_council_district_2021.png", width = 11, height = 8.5, dpi = 120)

council_districts %>%
  left_join(property_summary_by_district_2021, by = c("name" = "area")) %>%
  ggplot() +
  geom_sf(aes(fill = perc_properties_area), color = "white", size = 1.25) +
  geom_sf(data = baltimore_city, color = "gray25", fill = NA, size = 1) +
  scale_fill_viridis_c(label = scales::label_percent(scale = 1)) +
  labs(fill = "Properties advertised for tax sale\nas share of all district properties") +
  map_theme

ggsave("perc_properties_area_by_council_district_2021.png", width = 11, height = 8.5, dpi = 120)

advertised_properties_2021_sf %>%
# advertised_properties_20210406_sf %>%
  filter(owner_occupied %in% c("D", "H"),
         vacind == "N",
         no_imprv == "N") %>%
  mutate(lien_amount_quintile = cut_number(lien_amount/1000, n = 5)) %>%
  ggplot() +
  geom_sf(data = baltimore_city_detailed, color = "gray65", fill = NA) +
  geom_sf(data = council_districts, color = "gray85", fill = NA) +
  geom_sf(aes(color = lien_amount_quintile), alpha = 0.7) +
  geom_sf(data = baltimore_city, color = "gray15", fill = NA) +
  scale_color_viridis_d(end = 0.95) +
  map_theme +
  labs(title = "Home owners in tax sale by lien amount category",
    color = "Lien amount ($ thousands) category") #+
#  facet_wrap(~lien_amount_quintile)

# ggsave("homeowners_not_vacant_by_lien_amount_quintile_facet_20210406.png", width = 11, height = 8.5, dpi = 120)
# ggsave("homeowners_not_vacant_by_lien_amount_quintile_20210406.png", width = 11, height = 8.5, dpi = 120)
ggsave("homeowners_not_vacant_by_lien_amount_quintile.png", width = 11, height = 8.5, dpi = 120)
