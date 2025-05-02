library(tidyr)
library(ggplot2)
library(dplyr)
library(stringr)
library(stringdist)
library(ggpubr)
library(rnaturalearth)
library(rnaturalearthdata)
library(hrbrthemes)
library(ggpubr)
library(gridExtra)
library(cowplot)
library(patchwork)
library(ggrepel)
library(tidyverse)
library(gridGraphics)
library(sf)
library(sp)
library(scales)

setwd("/Users/sruthikp/Work/Analysis/TropicalEcologyLitReview/DataFolder/")

publications <- read.csv("Tropical_filtered_publications.csv")

unique(publications$biome)
publications_split <- publications %>%
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
    str_detect(standardized_biome, regex("Grasslands|Savannas", ignore_case = TRUE)) ~ "Grasslands",
    str_detect(standardized_biome, regex("Forests", ignore_case = TRUE)) ~ "Forests",
    TRUE ~ "Other"
  ))

publications_filter_biomes <- publications_split %>% filter(!(standardized_biome == 'Other'))

publications_site <- data.frame(ID = publications_filter_biomes$ID,
                                year = publications_filter_biomes$year,
                                ISO3 = publications_filter_biomes$ISO3,
                                biome = publications_filter_biomes$standardized_biome)

publications_site <- publications_site %>%
  separate_rows(ISO3, sep = ",") %>%
  mutate(sepISO3 = trimws(ISO3))

#publications_site$sepISO3 <- gsub("\\[|\\]|\\'", "", publications_site$sepISO3)

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

biome_counts <- summary_no_other %>%
  group_by(yearCategory, biome) %>%
  summarise(unique_publications = n_distinct(ID), .groups = 'drop')

biome_totals <- summary_no_other %>%
  group_by(yearCategory) %>%
  summarise(total_publications = n_distinct(ID), .groups = 'drop') %>%
  arrange(desc(total_publications)) 

# Recalculate the proportions with "Other" category
biome_proportions <- biome_counts %>%
  left_join(biome_totals, by = "yearCategory") %>%
  mutate(proportion = (unique_publications / total_publications) * 100) %>%
  arrange(yearCategory, desc(proportion))

biome_proportions$yearCategory <- factor(biome_proportions$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))

total_publications_per_decade_biome <- summary_no_other %>%
  group_by(yearCategory, biome) %>%
  summarise(total_publications = n_distinct(ID), .groups = 'drop')


# Calculate publications per country per decade and biome
publications_per_country_per_decade_biome <- summary_no_other %>%
  group_by(yearCategory, biome, sepISO3) %>%
  summarise(unique_publications = n_distinct(ID), .groups = 'drop')

# Calculate proportions
site_proportions_biome <- publications_per_country_per_decade_biome %>%
  left_join(total_publications_per_decade_biome, by = c("yearCategory", "biome")) %>%
  mutate(proportion = (unique_publications / total_publications) * 100) %>%
  arrange(yearCategory, biome, desc(proportion))



# Step 1: Create ranks for each decade
site_proportions_biome <- site_proportions_biome %>%
  group_by(yearCategory, biome) %>%
  mutate(rank = rank(-proportion, ties.method = "min"))

# Step 2: Count how many decades each country is present in
country_decade_count <- site_proportions_biome %>%
  group_by(biome, sepISO3) %>%
  summarise(decade_count = n_distinct(yearCategory), .groups = 'drop')

# Step 3: Sum ranks across all decades for each country within each biome,
# but only include those with more than one decade
combined_ranks_biome <- site_proportions_biome %>%
  group_by(biome, sepISO3) %>%
  summarise(combined_rank = sum(rank, na.rm = TRUE), .groups = 'drop') %>%
  inner_join(country_decade_count %>% filter(decade_count > 1), by = c("biome", "sepISO3")) %>%
  arrange(biome, combined_rank)

# Step 2: Sum ranks across all decades for each country within each biome
#combined_ranks_biome <- site_proportions_biome %>%
#  group_by(biome, sepISO3) %>%
#  summarise(combined_rank = sum(rank, na.rm = TRUE), .groups = 'drop') %>%
#  arrange(biome, combined_rank)

# Step 4: Select the top countries by combined rank within each biome (optional if you want to filter top countries)
top_countries_biome <- combined_ranks_biome %>%
  group_by(biome) %>%
  top_n(-5, combined_rank) # Select top 10 countries per biome (negative for ascending order)

top_countries_biome <- site_proportions_biome %>%
  filter(yearCategory == 'post 2010s') %>%
  group_by(biome) %>%
  top_n(-5, rank) # Select top 10 countries per biome (negative for ascending order)



# Step 5: Create a consistent color palette for countries

world <- ne_countries(scale = "medium", returnclass = "sf")
invalid_geometries <- st_is_valid(world, reason = TRUE)
world <- st_make_valid(world)

# Select relevant columns
world_data <- world %>% select(name, iso_a3, continent)

countryNames <- world_data %>% filter(iso_a3 %in% top_countries_biome$sepISO3)

countryNamesOnly <- data.frame(iso3 = countryNames$iso_a3,
                               name = countryNames$name) 

site_proportions_biome <- site_proportions_biome %>% left_join(countryNamesOnly, by = c("sepISO3" = "iso3"))

top_countries_biome <- top_countries_biome %>% left_join(countryNamesOnly, by = c("sepISO3" = "iso3"))



#country_colors <-colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(length(unique(top_countries_biome$sepISO3)))
#country_colors <- viridis(length(unique(top_countries_biome$sepISO3)))
#country_colors <- paletteer_d("dutchmasters::milkmaid", n =length(unique(top_countries_biome$sepISO3)))

fixed_colors <- c("Brazil" = "#1f77b4",
                  "United States of America" = "#6a3d9a",
                  "China" = "#a0522d",
                  "Nigeria" = "#ffd700",
                  "India" = "#e377c2",
                  "Panama" = "#ff99b4",
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
  "#8c164b", "#7f7f7f", "#bcbd22", "#17becf", "#deb887", "#d2691e",
  "#ff4500", "#32cd30", "#4682b4", "#b22222",
  "#5f9ea0", "#3f5f5f", "#e91111", "#a227c6") 
remaining_countries <- top_countries_biome %>% filter(!(name %in% c("Brazil",
                                                              "United States of America",
                                                              "China",
                                                              "Nigeria" ,
                                                              "India",
                                                              "Panama",
                                                              "Australia",
                                                              "Costa Rica",
                                                              "South Africa")))
remaining_countries <-  unique(remaining_countries$name)
names(random_colors) <- remaining_countries

country_colors <- c(fixed_colors, random_colors)


top5_sites_forests <- top_countries_biome %>% filter(biome == "Forests")
plotTimeSeriesForests <- site_proportions_biome %>% filter(biome == "Forests") %>% filter(sepISO3 %in% top5_sites_forests$sepISO3)

plotTimeSeriesForests$yearCategory <- factor(plotTimeSeriesForests$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))

#forest_prop <- biome_proportions %>% filter(biome =="Forests") %>%
#  mutate(ypos = cumsum(proportion)- 0.5*proportion )
#inset_forest <- ggplot(forest_prop, aes(x=yearCategory, y=proportion, fill=yearCategory)) +
#  geom_bar(stat="identity", width=0.7, color="white") + 
#  theme_minimal() + 
#  theme(axis.text.x = element_text(angle = 45, hjust = 1),
#    legend.position="none") +
#  scale_fill_brewer(palette ="Greys") +
#  labs(x = "",y = "")

forest_prop <-  biome_proportions %>% filter(biome =="Forests") %>%
  mutate(csum = rev(cumsum(rev(proportion))), 
         pos = proportion/2 + lead(csum, 1),
         pos_ad = if_else(is.na(pos), proportion/2, pos))

forest_prop$yearCategory <- factor(forest_prop$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))

inset_forest <- ggplot(forest_prop, aes(x = "" , y = proportion, fill = (yearCategory))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_text(aes(label = paste0(round(proportion,1),"%")),
            size = 3,
            position = position_stack(vjust = 0.5)) +
  #guides(fill = guide_legend(title = "Group")) +
  #scale_y_continuous(breaks = forest_prop$pos_ad, labels = forest_prop$yearCategory)+#c("pre 1980s", "1980s","1990s", "2000s", "post\n2010s")) +
  #ggtitle("African")+
  theme_void()+
  theme(legend.text = element_text(size = 14),
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none")

forestTimeSeries <- ggplot(plotTimeSeriesForests) +
  geom_line(size=1, color="grey",aes(x=yearCategory, y=log(proportion), group=name)) +
  geom_point(aes(x=yearCategory, y=log(proportion),fill=name),shape=21, color="black", size=6) +
  #scale_fill_brewer(name = "Country",palette = "Paired")+
  scale_fill_manual(name = "Country", values = country_colors) +  # Apply consistent country colors for points
  scale_color_manual(name = "Country", values = country_colors) +
  scale_y_continuous(trans = 'log', 
                     labels = function(x) format(round(exp(x),0), scientific = FALSE)) + 
  labs(x = "Decades" , y = "Publications (%)") + 
  theme_ipsum(plot_title_size = 12) +
  ggtitle("Forests")+
  theme(legend.text = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none", # Removes the legend
        plot.title = element_text(hjust=0.5))

#forestTimeSeries <- forestTimeSeries + inset_element(inset_forest, 0, 0.6, 0.4, 1) 

top5_sites_ocean <- top_countries_biome %>% filter(biome == "Ocean")
plotTimeSeriesOcean <- site_proportions_biome %>% filter(biome == "Ocean") %>% filter(sepISO3 %in% top5_sites_ocean$sepISO3)
#plotTimeSeriesOcean <- plotTimeSeriesOcean %>% left_join(countryNamesOnly, by = c("sepISO3" = "iso3"))
plotTimeSeriesOcean$yearCategory <- factor(plotTimeSeriesOcean$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))

ocean_prop <- biome_proportions %>% filter(biome =="Ocean")  %>%
  mutate(csum = rev(cumsum(rev(proportion))), 
         pos = proportion/2 + lead(csum, 1),
         pos_ad = if_else(is.na(pos), proportion/2, pos))

ocean_prop$yearCategory <- factor(ocean_prop$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))

inset_ocean <- ggplot(ocean_prop,aes(x = "" , y = proportion, fill = (yearCategory))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_text(aes(label = paste0(round(proportion,1),"%")),
            size = 3,
            position = position_stack(vjust = 0.5)) +
  #guides(fill = guide_legend(title = "Group")) +
  #scale_y_continuous(breaks = forest_prop$pos_ad, labels = forest_prop$yearCategory)+#c("pre 1980s", "1980s","1990s", "2000s", "post\n2010s")) +
  #ggtitle("African")+
  theme_void()+
  theme(legend.text = element_text(size = 14),
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none")
  
oceanTimeSeries <- ggplot(plotTimeSeriesOcean) +
  geom_line(size=1, color="grey",aes(x=yearCategory, y=log(proportion), group=name)) +
  geom_point(aes(x=yearCategory, y=log(proportion),fill=name),shape=21, color="black", size=6) +
  #scale_fill_brewer(name = "Country",palette = "Paired")+
  scale_fill_manual(name = "Country", values = country_colors) +  # Apply consistent country colors for points
  scale_color_manual(name = "Country", values = country_colors) +
  scale_y_continuous(trans = 'log', 
                     labels = function(x) format(round(exp(x),0), scientific = FALSE)) + 
  labs(x = "Decades" , y = "Publications (%)") + 
  theme_ipsum(plot_title_size = 12) +
  ggtitle("Ocean")+
  theme(legend.text = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none", # Removes the legend
        plot.title = element_text(hjust=0.5))

#oceanTimeSeries <- oceanTimeSeries + inset_element(inset_ocean, 0, 0.6, 0.4, 1) 

top5_sites_grass <- top_countries_biome %>% filter(biome == "Grasslands")
plotTimeSeriesGrass <- site_proportions_biome %>% filter(biome == "Grasslands") %>% filter(sepISO3 %in% top5_sites_grass$sepISO3)
#plotTimeSeriesGrass <- plotTimeSeriesGrass %>% left_join(countryNamesOnly, by = c("sepISO3" = "iso3"))
plotTimeSeriesGrass$yearCategory <- factor(plotTimeSeriesGrass$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))

grass_prop <- biome_proportions %>% filter(biome =="Grasslands")%>%
  mutate(csum = rev(cumsum(rev(proportion))), 
         pos = proportion/2 + lead(csum, 1),
         pos_ad = if_else(is.na(pos), proportion/2, pos))

grass_prop$yearCategory <- factor(grass_prop$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))

inset_grass <- ggplot(grass_prop, aes(x = "" , y = proportion, fill = yearCategory)) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_text(aes(label = paste0(round(proportion,1),"%")),
            size = 3,
            position = position_stack(vjust = 0.5)) +
  #guides(fill = guide_legend(title = "Group")) +
  #scale_y_continuous(breaks = forest_prop$pos_ad, labels = forest_prop$yearCategory)+#c("pre 1980s", "1980s","1990s", "2000s", "post\n2010s")) +
  #ggtitle("African")+
  theme_void()+
  theme(legend.text = element_text(size = 14),
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none")

grassTimeSeries <- ggplot(plotTimeSeriesGrass) +
  geom_line(size=1, color="grey",aes(x=yearCategory, y=log(proportion), group=name)) +
  geom_point(aes(x=yearCategory, y=log(proportion),fill=name),shape=21, color="black", size=6) +
  #scale_fill_brewer(name = "Country",palette = "Paired")+
  scale_fill_manual(name = "Country", values = country_colors) +  # Apply consistent country colors for points
  scale_color_manual(name = "Country", values = country_colors) +
  scale_y_continuous(trans = 'log', 
                     labels = function(x) format(round(exp(x),0), scientific = FALSE)) + 
  labs(x = "Decades" , y = "Publications (%)") + 
  theme_ipsum(plot_title_size = 12) +
  ggtitle("Grassy")+
  theme(legend.text = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none", # Removes the legend
        plot.title = element_text(hjust=0.5))

#grassTimeSeries <- grassTimeSeries + inset_element(inset_grass, 0, 0.6, 0.4, 1) 

top5_sites_wet <- top_countries_biome %>% filter(biome == "Freshwater")
plotTimeSeriesWet <- site_proportions_biome %>% filter(biome == "Freshwater") %>% filter(sepISO3 %in% top5_sites_wet$sepISO3)
#plotTimeSeriesWet <- plotTimeSeriesWet %>% left_join(countryNamesOnly, by = c("sepISO3" = "iso3"))
plotTimeSeriesWet$yearCategory <- factor(plotTimeSeriesWet$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))

wet_prop <- biome_proportions %>% filter(biome =="Freshwater")%>%
  mutate(csum = rev(cumsum(rev(proportion))), 
         pos = proportion/2 + lead(csum, 1),
         pos_ad = if_else(is.na(pos), proportion/2, pos))

wet_prop$yearCategory <- factor(wet_prop$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))

inset_wet <- ggplot(wet_prop, aes(x = "" , y = proportion, fill = yearCategory)) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_text(aes(label = paste0(round(proportion,1),"%")),
            size = 3,
            position = position_stack(vjust = 0.5)) +
  #guides(fill = guide_legend(title = "Group")) +
  #scale_y_continuous(breaks = forest_prop$pos_ad, labels = forest_prop$yearCategory)+#c("pre 1980s", "1980s","1990s", "2000s", "post\n2010s")) +
  #ggtitle("African")+
  theme_void()+
  theme(legend.text = element_text(size = 14),
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none")

wetTimeSeries <- ggplot(plotTimeSeriesWet) +
  geom_line(size=1, color="grey",aes(x=yearCategory, y=log(proportion), group=name)) +
  geom_point(aes(x=yearCategory, y=log(proportion),fill=name),shape=21, color="black", size=6) +
  #scale_fill_brewer(name = "Country",palette = "Paired")+
  scale_fill_manual(name = "Country", values = country_colors) +  # Apply consistent country colors for points
  scale_color_manual(name = "Country", values = country_colors) +
  scale_y_continuous(trans = 'log', 
                     labels = function(x) format(round(exp(x),0), scientific = FALSE)) + 
  labs(x = "Decades" , y = "Publications (%)") + 
  theme_ipsum(plot_title_size = 12) +
  ggtitle("Freshwater")+
  theme(legend.text = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none", # Removes the legend
        plot.title = element_text(hjust=0.5))

#wetTimeSeries <- wetTimeSeries + inset_element(inset_wet, 0, 0.6, 0.4, 1) 

top5_sites_coastal <- top_countries_biome %>% filter(biome == "Coastal")
plotTimeSeriesCoastal <- site_proportions_biome %>% filter(biome == "Coastal") %>% filter(sepISO3 %in% top5_sites_coastal$sepISO3)
#plotTimeSeriesCoastal <- plotTimeSeriesCoastal %>% left_join(countryNamesOnly, by = c("sepISO3" = "iso3"))
plotTimeSeriesCoastal$yearCategory <- factor(plotTimeSeriesCoastal$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))

coast_prop <- biome_proportions %>% filter(biome =="Coastal") %>%
  mutate(csum = rev(cumsum(rev(proportion))), 
         pos = proportion/2 + lead(csum, 1),
         pos_ad = if_else(is.na(pos), proportion/2, pos))

coast_prop$yearCategory <- factor(coast_prop$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))

inset_coast <- ggplot(coast_prop, aes(x = "" , y = proportion, fill = yearCategory)) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_text(aes(label = paste0(round(proportion,1),"%")),
            size = 3,
            position = position_stack(vjust = 0.5)) +
  #guides(fill = guide_legend(title = "Group")) +
  #scale_y_continuous(breaks = forest_prop$pos_ad, labels = forest_prop$yearCategory)+#c("pre 1980s", "1980s","1990s", "2000s", "post\n2010s")) +
  #ggtitle("African")+
  theme_void()+
  theme(legend.text = element_text(size = 14),
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none")

coastalTimeSeries <- ggplot(plotTimeSeriesCoastal) +
  geom_line(size=1, color="grey",aes(x=yearCategory, y=log(proportion), group=name)) +
  geom_point(aes(x=yearCategory, y=log(proportion),fill=name),shape=21, color="black", size=6) +
  #scale_fill_brewer(name = "Country",palette = "Paired")+
  scale_fill_manual(name = "Country", values = country_colors) +  # Apply consistent country colors for points
  scale_color_manual(name = "Country", values = country_colors) +
  scale_y_continuous(trans = 'log', 
                     labels = function(x) format(round(exp(x),0), scientific = FALSE)) + 
  labs(x = "Decades" , y = "Publications (%)") + 
  theme_ipsum(plot_title_size = 12) +
  ggtitle("Coastal") +  
  theme(legend.text = element_text(size = 14),
                              axis.title.x = element_blank(),
                              axis.title.y = element_text(size = 14),
                              axis.text.x = element_text(size = 12),
                              axis.text.y = element_text(size = 12),
                              legend.position = "none", # Removes the legend
                              plot.title = element_text(hjust=0.5))
#coastalTimeSeries <- coastalTimeSeries + inset_element(inset_coast, 0, 0.6, 0.4, 1) 


combined_data <- bind_rows(plotTimeSeriesForests, plotTimeSeriesGrass,plotTimeSeriesWet, plotTimeSeriesCoastal,plotTimeSeriesOcean)

legend_plot <- ggplot(combined_data) +
  geom_line(size=2,aes(x=yearCategory, y=log(proportion), group=name)) +
  geom_point(aes(x=yearCategory, y=log(proportion),fill=name),shape=21, color="black", size=6) +
  scale_fill_manual(name = "Country", values = country_colors)+
  scale_color_manual(name = "Country", values = country_colors) +
  theme_void() +
  theme(legend.position = "bottom",legend.title = element_blank(),
        legend.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

coast_prop$yearCategory <- factor(coast_prop$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))



pie_legend_plot <- ggplot(coast_prop, aes(x = "" , y = proportion, fill = yearCategory)) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  guides(fill = guide_legend(title = "Decade")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

#country_legend <- ggdraw(cowplot::get_legend(legend_plot))

country_legend = ggdraw(get_plot_component(legend_plot, 'guide-box-bottom', return_all = TRUE))
pie_legend <- ggdraw(get_plot_component(pie_legend_plot, 'guide-box-bottom', return_all = TRUE))


pie_plot <- plot_grid(inset_forest,  inset_grass, inset_wet,  inset_coast, inset_ocean,
                      ncol = 5, nrow = 1, 
                      align = 'v',
                      labels = c('A', 'B', 'C', 'D', 'E'))

pie_plot <- plot_grid(pie_plot,pie_legend,ncol = 1, nrow = 2)
# Extract the legend from the legend plot

comb_legend <- ggdraw(plot_grid(country_legend, pie_plot, ncol=1, nrow=2,align = 'v'))

forestTimeSeries <- forestTimeSeries + theme(legend.position = "none")
grassTimeSeries <- grassTimeSeries + theme(legend.position = "none")

combined_plots1 <- plot_grid(forestTimeSeries, grassTimeSeries, comb_legend,
                            ncol = 3, nrow = 1, 
                            align = 'v', 
                            labels = c('A', 'B'))

combined_plots2 <- plot_grid(wetTimeSeries, coastalTimeSeries , oceanTimeSeries,
                             ncol = 3, nrow = 1, 
                             align = 'v', 
                             labels = c('C', 'D','E'))

combined_plots <- plot_grid(combined_plots1, combined_plots2,
          ncol =1, nrow = 2, align = 'v')

quartz(width = 24, height = 10)
print(combined_plots)
ggsave("Top5StudySitesByBiomeCombinedRankFinal.png", plot = combined_plots, width = 23, height = 9, dpi = 800)

# Add the legend to the combined plot
#final_plot <- plot_grid(combined_plots, legend, ncol = 1, rel_heights = c(1, 0.1))

# Print or save the final plot
#print(final_plot)
#ggarrange(forestTimeSeries,  oceanTimeSeries, grassTimeSeries,  coastalTimeSeries, wetTimeSeries, ncol=3, nrow=2)
#ggarrange(forestTimeSeries, savannaTimeSeries, mangTimeSeries, marineTimeSeries, ncol=2, nrow=2)
