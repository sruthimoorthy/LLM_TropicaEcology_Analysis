# Load necessary libraries
library(sf)      # For handling vector data
library(dplyr)   # For data manipulation
library(units)   # For area calculations
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
setwd("/Users/sruthikp/Work/Analysis/TropicalEcologyLitReview/DataFolder/")

# 1. Load the biome shapefile (replace with actual file path)
biomes <- st_read("/Users/sruthikp/Work/Analysis/TropicalEcologyLitReview/shpFiles/wwf_ecoregions/wwf_terr_ecos.shp")

# 2. Load the country boundaries shapefile (e.g., from Natural Earth or GADM)
countries <- st_read("/Users/sruthikp/Work/Analysis/TropicalEcologyLitReview/shpFiles/WB_countries_Admin0_10m/WB_countries_Admin0_10m.shp")

# 3. Ensure both layers use the same CRS (Coordinate Reference System)
biomes <- st_transform(biomes, st_crs(countries))
biomes <- st_make_valid(biomes)
# 4. Initialize an empty data frame to store results


tropical_countries <- read.csv("UtilFiles/tropicalISO.csv")
lanLongCountries <- read.csv("UtilFiles/countries_codes_and_coordinates.csv")
lanLongCountries$Alpha.3.code <- gsub(" ", "", lanLongCountries$Alpha.3.code)


# Prepare latISO DataFrame
latISO <- data.frame(ISO3 = lanLongCountries$Alpha.3.code,
                     lat = lanLongCountries$Latitude..average.)


latISO_unique <- latISO %>%
  group_by(ISO3) %>%
  summarize(lat = first(lat))

countries_with_lat <- countries %>%
  left_join(latISO_unique, by = c("ISO_A3" = "ISO3"))

notTropical <- countries_with_lat %>% filter(!(ISO_A3 %in% tropical_countries$ISO3))

notTropical <- notTropical %>% filter(lat > 23.5 | lat < -23.5)

all_tropical_countries <- countries %>% filter(!(ISO_A3 %in% notTropical$ISO_A3))

all_tropical_countries <- all_tropical_countries %>% filter(!is.na(ISO_A3))
all_tropical_countries <- st_make_valid(all_tropical_countries)

india_geom <- all_tropical_countries %>% 
  filter(ISO_A3 == "IND")

biomes_in_india <- st_intersection(biomes, india_geom)

biomes_in_india_b2 <- biomes_in_india %>% 
  filter(BIOME == 2)

ggplot() +
  # Plot India's boundary (white fill, black outline)
  geom_sf(data = india_geom, fill = "white", color = "black") +
  
  # Plot BIOME=2 ecoregions, color-filled by ECO_NAME
  geom_sf(data = biomes_in_india_b2, aes(fill = ECO_NAME), color = NA) +
  
  # Use a discrete color scale
  scale_fill_viridis_d(name = "Ecoregion Name") +
  
  # Some clean theme settings
  theme_minimal() +
  theme(legend.position = "bottom") +
  
  # Add a title
  ggtitle("Tropical dry forests ecoregions in India")

results <- data.frame()

# 5. Loop through each country
for (i in 1:nrow(all_tropical_countries)) {
  
  country_name <- all_tropical_countries$NAME_EN[i]
  iso3 <- all_tropical_countries$ISO_A3[i]
  
  print(iso3)# Adjust column name if needed
  country_geom <- all_tropical_countries[i, ]     # Extract country geometry
  
  # 6. Clip biomes to the country boundary
  biomes_in_country <- st_intersection(biomes, country_geom)
  
  # 7. Calculate the area of each biome within the country (in square kilometers)
  biomes_in_country$Area_km2 <- as.numeric(st_area(biomes_in_country) / 10^6)
  
  # 8. Select relevant columns and store results
  country_results <- biomes_in_country %>%
    st_drop_geometry() %>%    # Remove spatial data to keep only attributes
    select(BIOME, ECO_NAME, Area_km2) %>%
    mutate(Country = iso3)
  
  results <- bind_rows(results, country_results)
}
final_results <- results %>% filter(Country != -99)

#results_ind <- results %>% filter(Country == "IND")
#results_no_ind <- results %>% filter(Country != "IND")

#results_ind_corr <- results_ind %>% filter(!(grepl('Deccan', ECO_NAME, fixed=TRUE)))

#final_results <- rbind(results_ind_corr, results_no_ind)
#results <- read.csv("biome_area_by_country.csv")
biome_summary <- final_results %>%
  group_by(Country, BIOME) %>%
  summarise(total_area = sum(Area_km2, na.rm = TRUE), .groups = "drop")




all_tropical_savannah <- biome_summary %>% filter(BIOME == 7)# 9. Save results to a CSV file
all_tropical_forests <- biome_summary %>% filter(BIOME == 1 | BIOME == 2 )#| BIOME_NUM == 3)


global_forest_area <- sum(all_tropical_forests$total_area)
global_savanna_area <- sum(all_tropical_savannah$total_area)

all_tropical_forests_grouped <- all_tropical_forests %>%
  group_by(Country) %>%
  summarise(total_area = sum(total_area, na.rm = TRUE), .groups = "drop")

all_tropical_forests_grouped$area_prop <- (all_tropical_forests_grouped$total_area/global_forest_area) *100

all_tropical_savannah$area_prop <- (all_tropical_savannah$total_area/global_savanna_area) *100

write.csv(all_tropical_forests_grouped, "forest_biome_area_by_country.csv", row.names = FALSE)
write.csv(all_tropical_savannah, "grassy_biome_area_by_country.csv", row.names = FALSE)

#print("Biome area calculations completed and saved!")
top_10_savannah_countries <- all_tropical_savannah %>%
  top_n(10, area_prop) # Select the top 10 countries with the lowest (best) combined rank

top_10_forest_countries <- all_tropical_forests_grouped %>%
  top_n(10, area_prop) # Select the top 10 countries with the lowest (best) combined rank


# Load world country geometries for visualizations
world <- ne_countries(scale = "medium", returnclass = "sf")  # Get country boundaries from natural earth
invalid_geometries <- st_is_valid(world, reason = TRUE)  # Check for any invalid geometries in the world data
world <- st_make_valid(world)  # Repair any invalid geometries

# Select relevant columns from the world data for further analysis
world_data <- world %>% select(name, adm0_a3, continent, pop_est)  # Extract country name, ISO3 code, continent, and population estimate

# Filter world data to include only countries that are in the top 10 list based on their ISO3 codes
savanna_countryNames <- world_data %>% 
  filter(adm0_a3 %in% top_10_savannah_countries$Country)  # Keep only countries that match the top 10 countries

forest_countryNames <- world_data %>% 
  filter(adm0_a3 %in% top_10_forest_countries$Country)  # Keep only countries that match the top 10 countries

# Create a dataframe with only ISO3 codes and corresponding country names for merging
savanna_CountryNamesOnly <- data.frame(iso3 = savanna_countryNames$adm0_a3,
                               name = savanna_countryNames$name)  # Prepare for a left join to associate missing data

forest_countryNamesOnly <- data.frame(iso3 = forest_countryNames$adm0_a3,
                                       name = forest_countryNames$name)  

top_10_savannah_countries <- top_10_savannah_countries %>% 
  left_join(savanna_CountryNamesOnly, by = c("Country" = "iso3"))

top_10_forest_countries <- top_10_forest_countries %>% 
  left_join(forest_countryNamesOnly, by = c("Country" = "iso3"))


fixed_colors <- c("Brazil" = "#1f77b4",
                  "United States of America" = "#6a3d9a",
                  "China" = "#a0522d",
                  "Nigeria" = "#ffd700",
                  "India" = "#e377c2",
                  "Panama" = "#bcbd22",
                  "Indonesia" = "#3cb372",
                  "Australia" = "#d62728",
                  "Costa Rica" = "#00FFFF",
                  "South Africa" = "#a0567d",
                  "Colombia" ="#ff7f0e",
                  "Malaysia" = "#5361bd",
                  "Mexico" = "#003300",
                  "Tanzania" = "#add8e6"
)

random_colors <- c(
  "#8c164b", "#7f7f7f", "#ff99b4", "#17becf", "#deb887", "#d2691e",
  "#ff4500", "#32cd30", "#4682b4", "#b22222",
  "#5f9ea0", "#3f5f5f", "#e91111", "#a227c6") 


# Identify remaining countries in the top countries trend data that need colors
savanna_remaining_countries <- top_10_savannah_countries %>% filter(!(name %in% c("Brazil",
                                                                    "United States of America",
                                                                    "China",
                                                                    "Nigeria" ,
                                                                    "India",
                                                                    "Panama",
                                                                    "Australia",
                                                                    "Costa Rica",
                                                                    "South Africa")))

# Retrieve unique remaining countries that require random color assignments
savanna_remaining_countries <- unique(savanna_remaining_countries)  # Get a vector of unique country names
names(random_colors) <- savanna_remaining_countries  # Assign names to the random colors based on the remaining country names

# Combine fixed and random colors into a single palette for visualization
savannah_country_colors <- c(fixed_colors, random_colors)  # Merge fixed color mappings with random colors

# Create a bar plot to visualize average citations per publication for the top countries
ggplot(top_10_savannah_countries, aes(x = reorder(name, area_prop), y = area_prop, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use position = "dodge" for side-by-side bars
  coord_flip()+
  labs(x = "Year", y = "% of tropical savanna area", title = "Average Citations per Publication by Country (2010-2023)") +
  theme_minimal() +
  scale_fill_manual(name = "Country", values = savannah_country_colors) + # Use your custom colors
  theme(legend.position = "none",
        plot.title = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 20, size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 14))# Use your custom colors

random_colors <- c(
  "#8c164b", "#7f7f7f", "#bcbd22", "#17becf", "#ff99b4", "#deb887", "#d2691e",
  "#ff4500", "#32cd30", "#4682b4", "#b22222",
  "#5f9ea0", "#3f5f5f", "#e91111", "#a227c6") 

forest_remaining_countries <- top_10_forest_countries %>% filter(!(name %in% c("Brazil",
                                                                               "United States of America",
                                                                               "China",
                                                                               "Nigeria" ,
                                                                               "India",
                                                                               "Panama",
                                                                               "Australia",
                                                                               "Costa Rica",
                                                                               "South Africa")))
forest_remaining_countries <- unique(forest_remaining_countries)  # Get a vector of unique country names
names(random_colors) <- forest_remaining_countries  # 
# Combine fixed and random colors into a single palette for visualization # Merge fixed color mappings with random colors
forest_country_colors <- c(fixed_colors, random_colors) 

ggplot(top_10_forest_countries, aes(
  x = reorder(name, area_prop), y = area_prop, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip()+# Use position = "dodge" for side-by-side bars
  labs(y = "% of tropical forest area", title = "Average Citations per Publication by Country (2010-2023)") +
  theme_minimal() +
  scale_fill_manual(name = "Country", values = forest_country_colors) + # Use your custom colors
  theme(legend.position = "none",
        plot.title = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 20, size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 14))# Use your custom colors


sum(all_forests_savannah_grouped$total_area)
sum(all_tropical_savannah$total_area)
