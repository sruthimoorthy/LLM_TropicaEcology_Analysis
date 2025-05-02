library(tidyr)
library(dplyr)
library(countrycode)
library(ggplot2)
library(maps)
library(mapdata)
library("rnaturalearth")
library("rnaturalearthdata")
library(sf)
library(tmap)
library(lwgeom)
library(ggpubr)
library(hrbrthemes)
library(viridis)

setwd("/Users/sruthikp/Work/Analysis/TropicalEcologyLitReview/DataFolder/")


publications <- read.csv("Tropical_filtered_publications.csv")

# Prepare publications_site DataFrame
publications_site <- data.frame(ID = publications$ID,
                                year = publications$year,
                                ISO3 = publications$ISO3,
                                biome = publications$biome)

publications_site <- publications_site %>%
  separate_rows(ISO3, sep = ",") %>%
  mutate(sepISO3 = trimws(ISO3))


tropical_countries <- read.csv("UtilFiles/tropicalISO.csv")
lanLongCountries <- read.csv("UtilFiles/countries_codes_and_coordinates.csv")
lanLongCountries$Alpha.3.code <- gsub(" ", "", lanLongCountries$Alpha.3.code)


# Prepare latISO DataFrame
latISO <- data.frame(ISO3 = lanLongCountries$Alpha.3.code,
                     lat = lanLongCountries$Latitude..average.)


latISO_unique <- latISO %>%
  group_by(ISO3) %>%
  summarize(lat = first(lat))


# Join publications_site with latISO to add latitude
publications_site_with_lat <- publications_site %>%
  left_join(latISO_unique, by = c("sepISO3" = "ISO3"))

notTropical <- publications_site_with_lat %>% filter(!(sepISO3 %in% tropical_countries$ISO3 | sepISO3 == "Other"))

notTropical <- notTropical %>% filter(lat > 23.5 | lat < -23.5)

publications_site_with_lat <- publications_site_with_lat %>% filter(!(sepISO3 %in% notTropical$sepISO3))



summary <- publications_site_with_lat %>%
  mutate(yearCategory = case_when(
    year < 1980 ~ "pre 1980s",
    year >= 1980 & year < 1990 ~ "1980s",
    year >= 1990 & year < 2000 ~ "1990s",
    year >= 2000 & year < 2010 ~ "2000s",
    year >= 2010  ~ "post 2010s"
  )) 

summary$yearCategory <- as.factor(summary$yearCategory)

summary_no_other <- summary %>% filter(!(sepISO3 == "Other"))

world <- ne_countries(scale = "medium", returnclass = "sf")
invalid_geometries <- st_is_valid(world, reason = TRUE)
world <- st_make_valid(world)

# Select relevant columns
world_data <- world %>% select(name, iso_a3, continent)

publications_site_with_lat_cont <- summary_no_other %>%
  left_join(world_data, by = c("sepISO3" = "iso_a3"))

publications_split <- publications_site_with_lat_cont %>%
  separate_rows(biome, sep = ",") %>%
  mutate(biomeCat = trimws(biome)) 

publications_split <- publications_split %>%
  mutate(standardized_biome = case_when(
    str_detect(biomeCat, regex("Seagrass|Marine", ignore_case = TRUE)) ~ "Marine",
    str_detect(biomeCat, regex("Estuar|Saltmarsh|Coastal", ignore_case = TRUE)) ~ "Coastal",
    str_detect(biomeCat, regex("Coral|reef", ignore_case = TRUE)) ~ "Coral reef",
    str_detect(biomeCat, regex("freshwater|Wetlands", ignore_case = TRUE)) ~ "Wetlands",
    str_detect(biomeCat, regex("grassland", ignore_case = TRUE)) ~ "Grasslands",
    str_detect(biomeCat, regex("Savannas", ignore_case = TRUE)) ~ "Savannas",
    str_detect(biomeCat, regex("Mangroves", ignore_case = TRUE)) ~ "Mangroves",
    str_detect(biomeCat, regex("Forests", ignore_case = TRUE)) ~ "Forests",
    str_detect(biomeCat, regex("Shrublands", ignore_case = TRUE)) ~ "Shrublands",
    TRUE ~ "Other"
  ))

publications_split <- publications_split %>%
  mutate(standardized_biome = case_when(
    str_detect(standardized_biome, regex("Marine|Coral reef", ignore_case = TRUE)) ~ "Ocean",
    str_detect(standardized_biome, regex("Wetlands", ignore_case = TRUE)) ~ "Freshwater",
    str_detect(standardized_biome, regex("Mangroves|Coastal", ignore_case = TRUE)) ~ "Coastal",
    str_detect(standardized_biome, regex("Grasslands|Savannas", ignore_case = TRUE)) ~ "Grassy",
    str_detect(standardized_biome, regex("Forests", ignore_case = TRUE)) ~ "Forests",
    TRUE ~ "Other"
  ))

#getIDs <- publications_split %>% filter(standardized_biome == 'Other' | standardized_biome == 'Multi-biomes')

#write.csv(getIDs,"unknownBiomes.csv")

publications_filter_biomes <- publications_split %>% filter(!(standardized_biome == 'Other'))

otherBiomes <- publications_split %>% filter(standardized_biome == 'Other')
print(length(unique(otherBiomes$ID)))

#terrestrialBiomes <- publications_filter_biomes %>% filter(standardized_biome != "Marine")


summary <- publications_filter_biomes %>%
  
  mutate(yearCategory = case_when(
    year < 1980 ~ "pre 1980s",
    year >= 1980 & year < 1990 ~ "1980s",
    year >= 1990 & year < 2000 ~ "1990s",
    year >= 2000 & year < 2010 ~ "2000s",
    year >= 2010  ~ "post 2010s"
  )) 

summary$yearCategory <- as.factor(summary$yearCategory)

summary$standardized_biome <- as.factor(summary$standardized_biome)

#summary_no_other <- summary %>% filter(standardized_biome != 'other')
#print(summary)

biome_counts <- summary %>%
  group_by(yearCategory, standardized_biome) %>%
  summarise(unique_publications = n_distinct(ID), .groups = 'drop')

biome_totals <- summary %>%
  group_by(yearCategory) %>%
  summarise(total_publications = n_distinct(ID), .groups = 'drop') %>%
  arrange(desc(total_publications)) 

# Recalculate the proportions with "Other" category
biome_proportions <- biome_counts %>%
  left_join(biome_totals, by = "yearCategory") %>%
  mutate(proportion = (unique_publications / total_publications) * 100) %>%
  arrange(yearCategory, desc(proportion))

biome_proportions$yearCategory <- factor(biome_proportions$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))
biome_proportions$standardized_biome <- factor(biome_proportions$standardized_biome, levels=c('Forests', 'Grassy','Coastal','Freshwater','Ocean'))#'Marine','Grasslands','Coastal','Wetlands','Shrublands'))



globalBiomePlot <- ggplot(biome_proportions) +
  geom_line(size=2,aes(x=yearCategory, y=proportion, group=standardized_biome)) +
  geom_point(aes(x=yearCategory, y=proportion,fill=standardized_biome),shape=21, color="black", size=6) +
  scale_fill_brewer(name = "Biome",palette = "Paired")+
  labs(subtitle = "Pantropical", x = "Decades" , y = "Proportion of publications (%)") + 
  theme_ipsum() +
  theme(axis.title = element_text(size=14),
        legend.title = element_blank(),
        legend.text =  element_text(size=14),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))


##################### Africa biome plot #####################

africaPubs <- publications_site_with_lat_cont %>% filter(continent == "Africa")

publications_split <- africaPubs %>%
  separate_rows(biome, sep = ",") %>%
  mutate(biomeCat = trimws(biome)) 

publications_split <- publications_split %>%
  mutate(standardized_biome = case_when(
    str_detect(biomeCat, regex("Seagrass|Marine", ignore_case = TRUE)) ~ "Marine",
    str_detect(biomeCat, regex("Estuar|Saltmarsh|Coastal", ignore_case = TRUE)) ~ "Coastal",
    str_detect(biomeCat, regex("Coral|reef", ignore_case = TRUE)) ~ "Coral reef",
    str_detect(biomeCat, regex("freshwater|Wetlands", ignore_case = TRUE)) ~ "Wetlands",
    str_detect(biomeCat, regex("grassland", ignore_case = TRUE)) ~ "Grassy",
    str_detect(biomeCat, regex("Savannas", ignore_case = TRUE)) ~ "Savannas",
    str_detect(biomeCat, regex("Mangroves", ignore_case = TRUE)) ~ "Mangroves",
    str_detect(biomeCat, regex("Forests", ignore_case = TRUE)) ~ "Forests",
    str_detect(biomeCat, regex("Shrublands", ignore_case = TRUE)) ~ "Shrublands",
    TRUE ~ "Other"
  ))


publications_split <- publications_split %>%
  mutate(standardized_biome = case_when(
    str_detect(standardized_biome, regex("Marine|Coral reef", ignore_case = TRUE)) ~ "Ocean",
    str_detect(standardized_biome, regex("Wetlands", ignore_case = TRUE)) ~ "Freshwater",
    str_detect(standardized_biome, regex("Mangroves|Coastal", ignore_case = TRUE)) ~ "Coastal",
    str_detect(standardized_biome, regex("Grassy|Savannas", ignore_case = TRUE)) ~ "Grassy",
    str_detect(standardized_biome, regex("Forests", ignore_case = TRUE)) ~ "Forests",
    TRUE ~ "Other"
  ))

#getIDs <- publications_split %>% filter(standardized_biome == 'Other' | standardized_biome == 'Multi-biomes')

#write.csv(getIDs,"unknownBiomes.csv")

publications_filter_biomes <- publications_split %>% filter(!(standardized_biome == 'Other'))

otherBiomes <- publications_split %>% filter(standardized_biome == 'Other')
print(length(unique(otherBiomes$ID)))

#terrestrialBiomes <- publications_filter_biomes %>% filter(standardized_biome != "Marine")


summary <- publications_filter_biomes %>%
  
  mutate(yearCategory = case_when(
    year < 1980 ~ "pre 1980s",
    year >= 1980 & year < 1990 ~ "1980s",
    year >= 1990 & year < 2000 ~ "1990s",
    year >= 2000 & year < 2010 ~ "2000s",
    year >= 2010  ~ "post 2010s"
  )) 

summary$yearCategory <- as.factor(summary$yearCategory)

summary$standardized_biome <- as.factor(summary$standardized_biome)

#summary_no_other <- summary %>% filter(standardized_biome != 'other')
#print(summary)

biome_counts <- summary %>%
  group_by(yearCategory, standardized_biome) %>%
  summarise(unique_publications = n_distinct(ID), .groups = 'drop')

biome_totals <- summary %>%
  group_by(yearCategory) %>%
  summarise(total_publications = n_distinct(ID), .groups = 'drop') %>%
  arrange(desc(total_publications)) 

# Recalculate the proportions with "Other" category
biome_proportions <- biome_counts %>%
  left_join(biome_totals, by = "yearCategory") %>%
  mutate(proportion = (unique_publications / total_publications) * 100) %>%
  arrange(yearCategory, desc(proportion))

biome_proportions$yearCategory <- factor(biome_proportions$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))
biome_proportions$standardized_biome <- factor(biome_proportions$standardized_biome, levels=c('Forests', 'Grassy','Coastal','Ocean', 'Freshwater'))

africaPlot <- ggplot(biome_proportions) +
  geom_line(size=2,aes(x=yearCategory, y=proportion, group=standardized_biome)) +
  geom_point(aes(x=yearCategory, y=proportion,fill=standardized_biome),shape=21, color="black", size=6) +
  scale_fill_brewer(name = "Biome",palette = "Paired")+
  labs(subtitle = "Africa" , x = "Decades" , y = "Proportion of publications (%)") + 
  theme_ipsum() +
  theme(axis.title = element_text(size=14),
        legend.title = element_blank(),
        legend.text =  element_text(size=14),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))

################################### America Biome #######################

americaPubs <- publications_site_with_lat_cont %>% filter(continent == "South America" | continent == "North America")

publications_split <- americaPubs %>%
  separate_rows(biome, sep = ",") %>%
  mutate(biomeCat = trimws(biome)) 

publications_split <- publications_split %>%
  mutate(standardized_biome = case_when(
    str_detect(biomeCat, regex("Seagrass|Marine", ignore_case = TRUE)) ~ "Marine",
    str_detect(biomeCat, regex("Estuar|Saltmarsh|Coastal", ignore_case = TRUE)) ~ "Coastal",
    str_detect(biomeCat, regex("Coral|reef", ignore_case = TRUE)) ~ "Coral reef",
    str_detect(biomeCat, regex("freshwater|Wetlands", ignore_case = TRUE)) ~ "Wetlands",
    str_detect(biomeCat, regex("grassland", ignore_case = TRUE)) ~ "Grasslands",
    str_detect(biomeCat, regex("Savannas", ignore_case = TRUE)) ~ "Savannas",
    str_detect(biomeCat, regex("Mangroves", ignore_case = TRUE)) ~ "Mangroves",
    str_detect(biomeCat, regex("Forests", ignore_case = TRUE)) ~ "Forests",
    str_detect(biomeCat, regex("Shrublands", ignore_case = TRUE)) ~ "Shrublands",
    TRUE ~ "Other"
  ))

publications_split <- publications_split %>%
  mutate(standardized_biome = case_when(
    str_detect(standardized_biome, regex("Marine|Coral reef", ignore_case = TRUE)) ~ "Ocean",
    str_detect(standardized_biome, regex("Wetlands", ignore_case = TRUE)) ~ "Freshwater",
    str_detect(standardized_biome, regex("Mangroves|Coastal", ignore_case = TRUE)) ~ "Coastal",
    str_detect(standardized_biome, regex("Grassy|Savannas", ignore_case = TRUE)) ~ "Grassy",
    str_detect(standardized_biome, regex("Forests", ignore_case = TRUE)) ~ "Forests",
    TRUE ~ "Other"
  ))
#getIDs <- publications_split %>% filter(standardized_biome == 'Other' | standardized_biome == 'Multi-biomes')

#write.csv(getIDs,"unknownBiomes.csv")

publications_filter_biomes <- publications_split %>% filter(!(standardized_biome == 'Other'))

otherBiomes <- publications_split %>% filter(standardized_biome == 'Other')
print(length(unique(otherBiomes$ID)))

#terrestrialBiomes <- publications_filter_biomes %>% filter(standardized_biome != "Marine")


summary <- publications_filter_biomes %>%
  
  mutate(yearCategory = case_when(
    year < 1980 ~ "pre 1980s",
    year >= 1980 & year < 1990 ~ "1980s",
    year >= 1990 & year < 2000 ~ "1990s",
    year >= 2000 & year < 2010 ~ "2000s",
    year >= 2010  ~ "post 2010s"
  )) 

summary$yearCategory <- as.factor(summary$yearCategory)

summary$standardized_biome <- as.factor(summary$standardized_biome)

#summary_no_other <- summary %>% filter(standardized_biome != 'other')
#print(summary)

biome_counts <- summary %>%
  group_by(yearCategory, standardized_biome) %>%
  summarise(unique_publications = n_distinct(ID), .groups = 'drop')

biome_totals <- summary %>%
  group_by(yearCategory) %>%
  summarise(total_publications = n_distinct(ID), .groups = 'drop') %>%
  arrange(desc(total_publications)) 

# Recalculate the proportions with "Other" category
biome_proportions <- biome_counts %>%
  left_join(biome_totals, by = "yearCategory") %>%
  mutate(proportion = (unique_publications / total_publications) * 100) %>%
  arrange(yearCategory, desc(proportion))

biome_proportions$yearCategory <- factor(biome_proportions$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))
biome_proportions$standardized_biome <- factor(biome_proportions$standardized_biome, levels=c('Forests', 'Grassy','Coastal','Ocean', 'Freshwater'))


americaBiomePlot <- ggplot(biome_proportions) +
  geom_line(size=2,aes(x=yearCategory, y=proportion, group=standardized_biome)) +
  geom_point(aes(x=yearCategory, y=proportion,fill=standardized_biome),shape=21, color="black", size=6) +
  scale_fill_brewer(name = "Biome",palette = "Paired")+
  labs(subtitle = "Americas" , x = "Decades" , y = "Proportion of publications (%)") + 
  theme_ipsum() +
  theme(axis.title = element_text(size=14),
        legend.title = element_blank(),
        legend.text =  element_text(size=14),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))


################################### America Biome #######################

asiaOceaniaPubs <- publications_site_with_lat_cont %>% filter(continent == "Asia" | continent == "Oceania")

publications_split <- asiaOceaniaPubs %>%
  separate_rows(biome, sep = ",") %>%
  mutate(biomeCat = trimws(biome)) 

publications_split <- publications_split %>%
  mutate(standardized_biome = case_when(
    str_detect(biomeCat, regex("Seagrass|Marine", ignore_case = TRUE)) ~ "Marine",
    str_detect(biomeCat, regex("Estuar|Saltmarsh|Coastal", ignore_case = TRUE)) ~ "Coastal",
    str_detect(biomeCat, regex("Coral|reef", ignore_case = TRUE)) ~ "Coral reef",
    str_detect(biomeCat, regex("freshwater|Wetlands", ignore_case = TRUE)) ~ "Wetlands",
    str_detect(biomeCat, regex("grassland", ignore_case = TRUE)) ~ "Grasslands",
    str_detect(biomeCat, regex("Savannas", ignore_case = TRUE)) ~ "Savannas",
    str_detect(biomeCat, regex("Mangroves", ignore_case = TRUE)) ~ "Mangroves",
    str_detect(biomeCat, regex("Forests", ignore_case = TRUE)) ~ "Forests",
    str_detect(biomeCat, regex("Shrublands", ignore_case = TRUE)) ~ "Shrublands",
    TRUE ~ "Other"
  ))

publications_split <- publications_split %>%
  mutate(standardized_biome = case_when(
    str_detect(standardized_biome, regex("Marine|Coral reef", ignore_case = TRUE)) ~ "Ocean",
    str_detect(standardized_biome, regex("Wetlands", ignore_case = TRUE)) ~ "Freshwater",
    str_detect(standardized_biome, regex("Mangroves|Coastal", ignore_case = TRUE)) ~ "Coastal",
    str_detect(standardized_biome, regex("Grassy|Savannas", ignore_case = TRUE)) ~ "Grassy",
    str_detect(standardized_biome, regex("Forests", ignore_case = TRUE)) ~ "Forests",
    TRUE ~ "Other"
  ))
#getIDs <- publications_split %>% filter(standardized_biome == 'Other' | standardized_biome == 'Multi-biomes')

#write.csv(getIDs,"unknownBiomes.csv")

publications_filter_biomes <- publications_split %>% filter(!(standardized_biome == 'Other'))

otherBiomes <- publications_split %>% filter(standardized_biome == 'Other')
print(length(unique(otherBiomes$ID)))

#terrestrialBiomes <- publications_filter_biomes %>% filter(standardized_biome != "Marine")


summary <- publications_filter_biomes %>%
  
  mutate(yearCategory = case_when(
    year < 1980 ~ "pre 1980s",
    year >= 1980 & year < 1990 ~ "1980s",
    year >= 1990 & year < 2000 ~ "1990s",
    year >= 2000 & year < 2010 ~ "2000s",
    year >= 2010  ~ "post 2010s"
  )) 

summary$yearCategory <- as.factor(summary$yearCategory)

summary$standardized_biome <- as.factor(summary$standardized_biome)

#summary_no_other <- summary %>% filter(standardized_biome != 'other')
#print(summary)

biome_counts <- summary %>%
  group_by(yearCategory, standardized_biome) %>%
  summarise(unique_publications = n_distinct(ID), .groups = 'drop')

biome_totals <- summary %>%
  group_by(yearCategory) %>%
  summarise(total_publications = n_distinct(ID), .groups = 'drop') %>%
  arrange(desc(total_publications)) 

# Recalculate the proportions with "Other" category
biome_proportions <- biome_counts %>%
  left_join(biome_totals, by = "yearCategory") %>%
  mutate(proportion = (unique_publications / total_publications) * 100) %>%
  arrange(yearCategory, desc(proportion))

biome_proportions$yearCategory <- factor(biome_proportions$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))
biome_proportions$standardized_biome <- factor(biome_proportions$standardized_biome, levels=c('Forests', 'Grassy','Coastal','Ocean', 'Freshwater'))


asiaOceaniaBiomePlot <- ggplot(biome_proportions) +
  geom_line(size=2,aes(x=yearCategory, y=proportion, group=standardized_biome)) +
  geom_point(aes(x=yearCategory, y=proportion,fill=standardized_biome),shape=21, color="black", size=6) +
  scale_fill_brewer(name = "Biome",palette = "Paired")+
  labs(subtitle = "Asia/Oceania" , x = "Decades" , y = "Proportion of publications (%)") + 
  theme_ipsum() +theme(axis.title = element_text(size=14),
                       legend.title = element_blank(),
                       legend.text =  element_text(size=14),
                       axis.title.x = element_blank(),
                       axis.text.x = element_text(size=12),
                       axis.text.y = element_text(size=12))

########################## Combining figures #########################

# 1. Make an empty "dummy" plot
emptyPlot <- ggplot() + theme_void()

# 2. Place your globalBiomePlot in the middle of a 3-column arrangement
topCentered <- ggarrange(
  emptyPlot,       # left column (blank)
  globalBiomePlot, # center column (your real plot)
  emptyPlot,       # right column (blank)
  ncol = 3, 
  widths = c(1, 2, 1)  # center plot gets more space
)

# 3. Use that new topCentered as the first row, and biomeBottomPlot as the second

biomePlotList <- c(list(africaPlot), list(americaBiomePlot), list(asiaOceaniaBiomePlot))

biomeBottomPlot <- ggarrange(plotlist = biomePlotList, ncol = 3, nrow = 1, common.legend=TRUE, legend="none")

combinedBiomePlot <- ggarrange(
  topCentered,
  biomeBottomPlot,
  ncol = 1, nrow = 2,
  common.legend = TRUE, legend = "bottom"
)

ggsave("combinedBiomePlot.png", plot=combinedBiomePlot, width = 12, height = 6, dpi = 600)
