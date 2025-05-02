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
library(hrbrthemes)
library(viridis)
library(plyr)

setwd("/Users/sruthikp/Work/Analysis/TropicalEcologyLitReview/DataFolder/")


publications <- read.csv("Tropical_filtered_publications.csv")


# Prepare publications_site DataFrame
publications_site <- data.frame(ID = publications$ID,
                                year = publications$year,
                                ISO3 = publications$ISO3,
                                biome = publications$biome)


publications_site <- publications_site %>% filter(ISO3 != "Other")

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
  dplyr::group_by(ISO3) %>%
  dplyr::summarize(lat = first(lat))


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

africaPubs <- publications_site_with_lat_cont %>% filter(continent == "Africa")


site_counts <- africaPubs %>%
  dplyr::group_by(yearCategory, sepISO3) %>%
  dplyr::summarise(unique_publications = n_distinct(ID), .groups = 'drop')

site_totals <- africaPubs %>%
  dplyr::group_by(yearCategory) %>%
  dplyr::summarise(total_publications = n_distinct(ID), .groups = 'drop') %>%
  dplyr::arrange(desc(total_publications)) 

# Recalculate the proportions with "Other" category
site_proportions <- site_counts %>%
  dplyr::left_join(site_totals, by = "yearCategory") %>%
  dplyr::mutate(proportion = (unique_publications / total_publications) * 100) 

site_proportions <- site_proportions %>%
  dplyr::group_by(yearCategory) %>%
  dplyr::mutate(rank = rank(-proportion, ties.method = "min")) %>% dplyr::ungroup()

# Step 4: Select the top 10 countries leading post 2010s
top_10_countries_now <- site_proportions %>%
  filter(yearCategory == 'post 2010s') %>%
  arrange(rank) %>%
  slice(1:10)

# Step 4: Select the top 10 countries leading in the 60s & 70s
top_10_countries_1960s<- site_proportions %>%
  filter(yearCategory == 'pre 1980s') %>%
  arrange(rank) %>%
  slice(1:10)

# Step 5: Filter the site_proportions data for the top 10 countries
site_proportions_top_10_now <- site_proportions %>%
  filter(sepISO3 %in% top_10_countries_now$sepISO3)

# Step 5: Filter the site_proportions data for the top 10 countries
site_proportions_top_10_old <- site_proportions %>%
  filter(sepISO3 %in% top_10_countries_1960s$sepISO3)

world <- ne_countries(scale = "medium", returnclass = "sf")
invalid_geometries <- st_is_valid(world, reason = TRUE)
world <- st_make_valid(world)

# Select relevant columns
world_data <- world %>% select(name, iso_a3, continent)

countryNames <- world_data %>% filter(iso_a3 %in% top_10_countries_now$sepISO3 | iso_a3 %in% top_10_countries_1960s$sepISO3)

countryNamesOnly <- data.frame(iso3 = countryNames$iso_a3,
                               name = countryNames$name) 

site_proportions_top_10_now <- site_proportions_top_10_now %>% left_join(countryNamesOnly, by = c("sepISO3" = "iso3"))

site_proportions_top_10_now$yearCategory <- factor(site_proportions_top_10_now$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))

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
                  "Mexico" = "#003300"
)

random_colors <- c(
  "#8c164b", "#7f7f7f", "#bcbd22", "#17becf", "#deb887", "#d2691e",
  "#ff4500", "#32cd30", "#4682b4", "#b22222",
  "#5f9ea0", "#add8e6", "#3f5f5f", "#e91111", "#a227c6") 

now_remaining_countries <- site_proportions_top_10_now %>% filter(!(name %in% c("Brazil",
                                                                        "United States of America",
                                                                        "China",
                                                                        "Nigeria" ,
                                                                        "India",
                                                                        "Panama",
                                                                        "Australia",
                                                                        "Costa Rica",
                                                                        "South Africa")))

site_proportions_top_10_old <- site_proportions_top_10_old %>% left_join(countryNamesOnly, by = c("sepISO3" = "iso3"))

site_proportions_top_10_old$yearCategory <- factor(site_proportions_top_10_old$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))


old_remaining_countries <- site_proportions_top_10_old %>% filter(!(name %in% c("Brazil",
                                                                            "United States of America",
                                                                            "China",
                                                                            "Nigeria" ,
                                                                            "India",
                                                                            "Panama",
                                                                            "Australia",
                                                                            "Costa Rica",
                                                                            "South Africa")))
all_remaining_countries <-  unique(c(now_remaining_countries$name, old_remaining_countries$name))
names(random_colors) <- all_remaining_countries
country_colors <- c(fixed_colors, random_colors)


africaISOCurrentLeadPlot <- ggplot(site_proportions_top_10_now) +
  geom_line(size=1,aes(x=yearCategory, y=proportion, color = 'grey',group=name)) +
  geom_point(aes(x=yearCategory, y=proportion,fill=name),shape=21, color="black", size=6) +
  scale_fill_manual(name = "Country", values = country_colors) +  # Apply consistent country colors for points
  scale_color_manual(name = "Country", values = country_colors) +
  labs(x = "Decades" , y = "Publications (%)") + 
  theme_ipsum(plot_title_size = 12) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 14),
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))


africaISOOldLeadPlot <- ggplot(site_proportions_top_10_old) +
  geom_line(size=1,aes(x=yearCategory, y=proportion, color = 'grey',group=name)) +
  geom_point(aes(x=yearCategory, y=proportion,fill=name),shape=21, color="black", size=6) +
  scale_fill_manual(name = "Country", values = country_colors) +  # Apply consistent country colors for points
  scale_color_manual(name = "Country", values = country_colors) +
  labs(x = "Decades" , y = "Publications (%)") + 
  theme_ipsum(plot_title_size = 12) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 14),
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

############################### combining the plots ####################

combined_data <- bind_rows(site_proportions_top_10_now, site_proportions_top_10_old)

legend_plot <- ggplot(combined_data) +
  geom_line(size=2,aes(x=yearCategory, y=proportion, group=name)) +
  geom_point(aes(x=yearCategory, y=proportion,fill=name),shape=21, color="black", size=6) +
  scale_fill_manual(name = "Country", values = country_colors)+
  scale_color_manual(name = "Country", values = country_colors) +
  theme_void() +
  theme(legend.position = "bottom",legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

country_legend = ggdraw(get_plot_component(legend_plot, 'guide-box-bottom', return_all = TRUE))

africaISOCurrentLeadPlot <- africaISOCurrentLeadPlot + theme(legend.position = "none")
africaISOOldLeadPlot <- africaISOOldLeadPlot + theme(legend.position = "none")
library(ggpubr)
africaTopResearchHosts2010sVs1960s <- ggarrange(africaISOCurrentLeadPlot, africaISOOldLeadPlot,
                                          nrow = 1, ncol = 2, labels = c("a", "b") )

africaTopResearchHosts2010sVs1960sLegend <- ggarrange(africaTopResearchHosts2010sVs1960s, country_legend, 
                                                nrow = 2, ncol = 1, heights = c(2,1))
ggsave("AfricaStudySiteCurrentVsOldLead.png",plot = africaTopResearchHosts2010sVs1960sLegend, width = 10, height = 7, dpi = 600)


######################### Africa decadal map of all study sites #################

afr_data <- world_data %>% filter(continent == "Africa")
#world_data <- world_data %>% filter(Y < 25 & Y > -25)
#world_smpl <- World[World$name != "Antarctica",c("iso_a3", "name", "continent")]

summary_pro_pre80s <- site_proportions %>% filter(yearCategory == 'pre 1980s')
summary_pro_80s <- site_proportions %>% filter(yearCategory == '1980s')
summary_pro_90s <- site_proportions %>% filter(yearCategory == '1990s')
summary_pro_00s <- site_proportions %>% filter(yearCategory == '2000s')
summary_pro_post10s <- site_proportions %>% filter(yearCategory == 'post 2010s')




merged_data_smpl_pre80s <- afr_data %>%
  left_join(summary_pro_pre80s, by = c("iso_a3" = "sepISO3")) %>%
  mutate(pub = ifelse(is.na(proportion),NA, proportion)) %>%
  mutate(log_proportion = log1p(proportion)) 


merged_data_smpl_80s <- afr_data %>%
  left_join(summary_pro_80s, by = c("iso_a3" = "sepISO3")) %>%
  mutate(pub = ifelse(is.na(proportion), NA, proportion)) %>%
  mutate(log_proportion = log1p(proportion))

merged_data_smpl_90s <- afr_data %>%
  left_join(summary_pro_90s, by = c("iso_a3" = "sepISO3")) %>%
  mutate(pub = ifelse(is.na(proportion),NA, proportion))%>%
  mutate(log_proportion = log1p(proportion))

merged_data_smpl_00s <- afr_data %>%
  left_join(summary_pro_00s, by = c("iso_a3" = "sepISO3")) %>%
  mutate(pub = ifelse(is.na(proportion), NA, proportion))%>%
  mutate(log_proportion = log1p(proportion))

merged_data_smpl_post10s <- afr_data %>%
  left_join(summary_pro_post10s, by = c("iso_a3" = "sepISO3")) %>%
  mutate(pub = ifelse(is.na(proportion), NA, proportion))%>%
  mutate(log_proportion = log1p(proportion))



cholor_pre80s <- ggplot(merged_data_smpl_pre80s) +
  geom_sf(aes(fill = proportion), linewidth = 0, alpha = 0.9) +
  theme_void() +
  scale_fill_viridis(option="magma", na.value = "grey50",
                     name = "Proportion (%)") +
  labs(
    title = "Pre 1980s",
    #subtitle = "Number of publications per country",
  ) 

cholor_80s <- ggplot(merged_data_smpl_80s) +
  geom_sf(aes(fill = proportion), linewidth = 0, alpha = 0.9) +
  theme_void() +
  scale_fill_viridis(option="magma", na.value = "grey50",
                     name = "Proportion (%)") +
  labs(
    title = "1980s",
    #subtitle = "Number of publications per country",
  ) 

cholor_90s <- ggplot(merged_data_smpl_90s) +
  geom_sf(aes(fill = proportion), linewidth = 0, alpha = 0.9) +
  theme_void() +
  scale_fill_viridis(option="magma", na.value = "grey50",
                     name = "Proportion (%)") +
  labs(
    title = "1990s",
    #subtitle = "Number of publications per country",
  ) 

cholor_00s <- ggplot(merged_data_smpl_00s) +
  geom_sf(aes(fill = proportion), linewidth = 0, alpha = 0.9) +
  theme_void() +
  scale_fill_viridis(option="magma", na.value = "grey50",
                     name = "Proportion (%)") +
  labs(
    title = "2000s",
    #subtitle = "Number of publications per country",
  ) 

cholor_post10s <- ggplot(merged_data_smpl_post10s) +
  geom_sf(aes(fill = proportion), linewidth = 0, alpha = 0.9) +
  theme_void() +
  scale_fill_viridis(option="magma", na.value = "grey50",
                     name = "Proportion (%)") +
  labs(
    title = "Post 2010s",
    #subtitle = "Number of publications per country",
  ) 

library(patchwork)
africaDecadalMap <- 
  (cholor_pre80s + cholor_80s + cholor_90s) /            # top row (3 plots)
  (cholor_00s + cholor_post10s + plot_spacer())

ggsave("africaDecadalMap.png", plot = africaDecadalMap, width = 10, height = 8, dpi = 600)
#############################################Biomes in Africa###########################


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

afr_biome_counts <- summary %>%
  group_by(yearCategory, standardized_biome) %>%
  summarise(unique_publications = n_distinct(ID), .groups = 'drop')

afr_biome_totals <- summary %>%
  group_by(yearCategory) %>%
  summarise(total_publications = n_distinct(ID), .groups = 'drop') %>%
  arrange(desc(total_publications)) 

# Recalculate the proportions with "Other" category
afr_biome_proportions <- afr_biome_counts %>%
  left_join(afr_biome_totals, by = "yearCategory") %>%
  mutate(proportion = (unique_publications / total_publications) * 100) %>%
  arrange(yearCategory, desc(proportion))



##################### Biome by country ##########################################
summary$biome <- summary$standardized_biome

all_biome_proportions <- afr_biome_counts %>%
  left_join(biome_totals, by = "yearCategory") %>%
  mutate(proportion = (unique_publications / total_publications) * 100) %>%
  arrange(yearCategory, desc(proportion))

all_biome_proportions$biome <- all_biome_proportions$standardized_biome
afr_biome_proportions$biome <- afr_biome_proportions$standardized_biome

total_publications_per_decade_biome <- summary %>%
  group_by(yearCategory, biome) %>%
  summarise(total_publications = n_distinct(ID), .groups = 'drop')


# Calculate publications per country per decade and biome
publications_per_country_per_decade_biome <- summary %>%
  group_by(yearCategory, biome, sepISO3) %>%
  summarise(unique_publications = n_distinct(ID), .groups = 'drop')

# Calculate proportions
site_proportions_biome <- publications_per_country_per_decade_biome %>%
  left_join(total_publications_per_decade_biome, by = c("yearCategory", "biome")) %>%
  mutate(proportion = (unique_publications / total_publications) * 100) %>%
  arrange(yearCategory, biome, desc(proportion))

site_proportions_biome <- site_proportions_biome %>%
  group_by(yearCategory, biome) %>%
  mutate(rank = rank(-proportion, ties.method = "min"))

# Step 2: Sum ranks across all decades for each country within each biome
combined_ranks_biome <- site_proportions_biome %>%
  group_by(biome, sepISO3) %>%
  summarise(combined_rank = sum(rank, na.rm = TRUE), .groups = 'drop') %>%
  arrange(biome, combined_rank)

# Step 3: Select the top countries by combined rank within each biome (optional if you want to filter top countries)
top_countries_biome <- combined_ranks_biome %>%
  group_by(biome) %>%
  top_n(-5, combined_rank) # Select top 10 countries per biome (negative for ascending order)



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



country_colors <-colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(length(unique(top_countries_biome$sepISO3)))
#country_colors <- viridis(length(unique(top_countries_biome$sepISO3)))
#country_colors <- paletteer_d("dutchmasters::milkmaid", n =length(unique(top_countries_biome$sepISO3)))
country_colors <- c(
  "#1f77b4", "#ff7f0e", "#00FFFF", "#d62728", "#9467bd", 
  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
  "#6a3d9a", "#ff69b4")#, "#a0522d", "#deb887", "#d2691e",
#  "#ff4500", "#32cd30", "#4682b4", "#b22222", "#003300",
#  "#5f9ea0", "#ff6347", "#3cb372", "#ffd700", "#daa520",
#  "#c71585", "#f4a460", "#add8e6", "#ff1493", "#ff8c00"
#)
names(country_colors) <- unique(top_countries_biome$name)



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

forest_prop <-  all_biome_proportions %>% filter(biome =="Forests") %>%
  mutate(csum = rev(cumsum(rev(proportion))), 
         pos = proportion/2 + lead(csum, 1),
         pos_ad = if_else(is.na(pos), proportion/2, pos))

forest_prop$yearCategory <- factor(forest_prop$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))
#forest_prop$yearCategory <- plyr::revalue(forest_prop$yearCategory, c('pre 1980s' = 'pre 1980s', '1980s' ='1980s','1990s' = '1990s','2000s' = '2000s','post 2010s' = 'post\n2010s'))


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

forestTimeSeries <-ggplot(plotTimeSeriesForests) +
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


afr_forest_prop <-  afr_biome_proportions %>% filter(biome =="Forests") %>%
  mutate(csum = rev(cumsum(rev(proportion))), 
         pos = proportion/2 + lead(csum, 1),
         pos_ad = if_else(is.na(pos), proportion/2, pos))

afr_forest_prop$yearCategory <- factor(afr_forest_prop$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))
#afr_forest_prop$yearCategory <- plyr::revalue(afr_forest_prop$yearCategory, c('pre 1980s' = 'pre 1980s', '1980s' ='1980s','1990s' = '1990s','2000s' = '2000s','post 2010s' = 'post\n2010s'))



afr_inset_forest <- ggplot(afr_forest_prop, aes(x = "" , y = proportion, fill = (yearCategory))) +
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



#forestTimeSeries <- forestTimeSeries + inset_element(inset_forest, 0, 0.6, 0.4, 1) +
#  inset_element(afr_inset_forest, 0.6, 0.6, 1, 1) 

top5_sites_ocean <- top_countries_biome %>% filter(biome == "Ocean")
plotTimeSeriesOcean <- site_proportions_biome %>% filter(biome == "Ocean") %>% filter(sepISO3 %in% top5_sites_ocean$sepISO3)
#plotTimeSeriesOcean <- plotTimeSeriesOcean %>% left_join(countryNamesOnly, by = c("sepISO3" = "iso3"))
plotTimeSeriesOcean$yearCategory <- factor(plotTimeSeriesOcean$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))

ocean_prop <- all_biome_proportions %>% filter(biome =="Ocean")  %>%
  mutate(csum = rev(cumsum(rev(proportion))), 
         pos = proportion/2 + lead(csum, 1),
         pos_ad = if_else(is.na(pos), proportion/2, pos))

ocean_prop$yearCategory <- factor(ocean_prop$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))
#ocean_prop$yearCategory <- plyr::revalue(ocean_prop$yearCategory, c('pre 1980s' = 'pre 1980s', '1980s' ='1980s','1990s' = '1990s','2000s' = '2000s','post 2010s' = 'post\n2010s'))


inset_ocean <- ggplot(ocean_prop,aes(x = "" , y = proportion, fill = fct_inorder(yearCategory))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_text(aes(label = paste0(round(proportion,1),"%")),
            size = 2,
            position = position_stack(vjust = 0.5)) +
  #guides(fill = guide_legend(title = "Group")) +
  #scale_y_continuous(breaks = ocean_prop$pos_ad, labels = ocean_prop$yearCategory)+#c("pre 1980s", "1980s","1990s", "2000s", "post\n2010s")) +
  #ggtitle("All")+
  theme_void()+
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        #axis.text = element_text(size = 8), 
        legend.position = "none", # Removes the legend
        #panel.background = element_blank(),
        plot.title = element_text(hjust=0.5))

afr_ocean_prop <-  afr_biome_proportions %>% filter(biome =="Ocean") %>%
  mutate(csum = rev(cumsum(rev(proportion))), 
         pos = proportion/2 + lead(csum, 1),
         pos_ad = if_else(is.na(pos), proportion/2, pos))

afr_ocean_prop$yearCategory <- factor(afr_ocean_prop$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))
#afr_ocean_prop$yearCategory <- plyr::revalue(afr_ocean_prop$yearCategory, c('pre 1980s' = 'pre 1980s', '1980s' ='1980s','1990s' = '1990s','2000s' = '2000s','post 2010s' = 'post\n2010s'))


afr_inset_ocean <- ggplot(afr_ocean_prop, aes(x = "" , y = proportion, fill = fct_inorder(yearCategory))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_text(aes(label = paste0(round(proportion,1),"%")),
            size = 2,
            position = position_stack(vjust = 0.5)) +
  #guides(fill = guide_legend(title = "Group")) +
  #scale_y_continuous(breaks = afr_ocean_prop$pos_ad, labels = afr_ocean_prop$yearCategory)+#c("pre 1980s", "1980s","1990s", "2000s", "post\n2010s")) +
  #ggtitle("African")+
  theme_void()+
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        #axis.text = element_text(size = 8), 
        legend.position = "none", # Removes the legend
        #panel.background = element_blank(),
        plot.title = element_text(hjust=0.5))

oceanTimeSeries <- ggplot(plotTimeSeriesOcean) +
  geom_line(size=2,aes(x=yearCategory, y=proportion, group=name)) +
  geom_point(aes(x=yearCategory, y=proportion,fill=name),shape=21, color="black", size=6) +
  #scale_fill_brewer(name = "Country",palette = "Paired")+
  scale_fill_manual(name = "Country", values = country_colors) +  # Apply consistent country colors for points
  scale_color_manual(name = "Country", values = country_colors) +
  labs(x = "Decades" , y = "Proportion of publications (%)") + 
  theme_ipsum(plot_title_size = 12) +
  ggtitle("Ocean ecosystem study sites - 1960s to now")+
  theme(legend.position = "none")

#oceanTimeSeries <- oceanTimeSeries + inset_element(inset_ocean, 0, 0.6, 0.4, 1) +
#  inset_element(afr_inset_ocean, 0.6, 0.6, 1, 1) 

top5_sites_grass <- top_countries_biome %>% filter(biome == "Grassy")
plotTimeSeriesGrass <- site_proportions_biome %>% filter(biome == "Grassy") %>% filter(sepISO3 %in% top5_sites_grass$sepISO3)
#plotTimeSeriesGrass <- plotTimeSeriesGrass %>% left_join(countryNamesOnly, by = c("sepISO3" = "iso3"))
plotTimeSeriesGrass$yearCategory <- factor(plotTimeSeriesGrass$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))

grass_prop <- all_biome_proportions %>% filter(biome =="Grassy")%>%
  mutate(csum = rev(cumsum(rev(proportion))), 
         pos = proportion/2 + lead(csum, 1),
         pos_ad = if_else(is.na(pos), proportion/2, pos))

grass_prop$yearCategory <- factor(grass_prop$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))
#grass_prop$yearCategory <- plyr::revalue(grass_prop$yearCategory, c('pre 1980s' = 'pre 1980s', '1980s' ='1980s','1990s' = '1990s','2000s' = '2000s','post 2010s' = 'post\n2010s'))


inset_grass <- ggplot(grass_prop, aes(x = "" , y = proportion, fill = fct_inorder(yearCategory))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_text(aes(label = paste0(round(proportion,1),"%")),
            size = 2,
            position = position_stack(vjust = 0.5)) +
  #guides(fill = guide_legend(title = "Group")) +
  #scale_y_continuous(breaks = grass_prop$pos_ad, labels = grass_prop$yearCategory)+#c("pre 1980s", "1980s","1990s", "2000s", "post\n2010s")) +
  #ggtitle("All")+
  theme_void()+
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        #axis.text = element_text(size = 8), 
        legend.position = "none", # Removes the legend
        #panel.background = element_blank(),
        plot.title = element_text(hjust=0.5))

afr_grass_prop <-  afr_biome_proportions %>% filter(biome =="Grassy") %>%
  mutate(csum = rev(cumsum(rev(proportion))), 
         pos = proportion/2 + lead(csum, 1),
         pos_ad = if_else(is.na(pos), proportion/2, pos))

afr_grass_prop$yearCategory <- factor(afr_grass_prop$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))
#afr_grass_prop$yearCategory <- plyr::revalue(afr_grass_prop$yearCategory, c('pre 1980s' = 'pre 1980s', '1980s' ='1980s','1990s' = '1990s','2000s' = '2000s','post 2010s' = 'post\n2010s'))


afr_inset_grass <- ggplot(afr_grass_prop, aes(x = "" , y = proportion, fill = fct_inorder(yearCategory))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_text(aes(label = paste0(round(proportion,1),"%")),
            size = 2,
            position = position_stack(vjust = 0.5)) +
  #guides(fill = guide_legend(title = "Group")) +
  #scale_y_continuous(breaks = afr_grass_prop$pos_ad, labels = afr_grass_prop$yearCategory)+#c("pre 1980s", "1980s","1990s", "2000s", "post\n2010s")) +
  #ggtitle("African")+
  theme_void()+
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        #axis.text = element_text(size = 8), 
        legend.position = "none", # Removes the legend
        #panel.background = element_blank(),
        plot.title = element_text(hjust=0.5))


grassTimeSeries <- ggplot(plotTimeSeriesGrass) +
  geom_line(size=2,aes(x=yearCategory, y=proportion, group=name)) +
  geom_point(aes(x=yearCategory, y=proportion,fill=name),shape=21, color="black", size=6) +
  #scale_fill_brewer(name = "Country",palette = "Paired")+
  scale_fill_manual(name = "Country", values = country_colors) +  # Apply consistent country colors for points
  scale_color_manual(name = "Country", values = country_colors) +
  labs(x = "Decades" , y = "Proportion of publications (%)") + 
  theme_ipsum(plot_title_size = 12) +
  ggtitle("Grassy ecosystem study sites - 1960s to now")+
  theme(legend.position = "none")

#grassTimeSeries <- grassTimeSeries + inset_element(inset_grass, 0, 0.6, 0.4, 1) +
#  inset_element(afr_inset_grass, 0.6, 0.6, 1, 1) 

top5_sites_wet <- top_countries_biome %>% filter(biome == "Freshwater")
plotTimeSeriesWet <- site_proportions_biome %>% filter(biome == "Freshwater") %>% filter(sepISO3 %in% top5_sites_wet$sepISO3)
#plotTimeSeriesWet <- plotTimeSeriesWet %>% left_join(countryNamesOnly, by = c("sepISO3" = "iso3"))
plotTimeSeriesWet$yearCategory <- factor(plotTimeSeriesWet$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))

wet_prop <- all_biome_proportions %>% filter(biome =="Freshwater")%>%
  mutate(csum = rev(cumsum(rev(proportion))), 
         pos = proportion/2 + lead(csum, 1),
         pos_ad = if_else(is.na(pos), proportion/2, pos))

wet_prop$yearCategory <- factor(wet_prop$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))
#wet_prop$yearCategory <- plyr::revalue(wet_prop$yearCategory, c('pre 1980s' = 'pre 1980s', '1980s' ='1980s','1990s' = '1990s','2000s' = '2000s','post 2010s' = 'post\n2010s'))


inset_wet <- ggplot(wet_prop, aes(x = "" , y = proportion, fill = fct_inorder(yearCategory))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_text(aes(label = paste0(round(proportion,1),"%")),
            size = 2,
            position = position_stack(vjust = 0.5)) +
  #guides(fill = guide_legend(title = "Group")) +
  #scale_y_continuous(breaks = wet_prop$pos_ad, labels = wet_prop$yearCategory)+#c("pre 1980s", "1980s","1990s", "2000s", "post\n2010s")) +
  #ggtitle("All")+
  theme_void()+
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        #axis.text = element_text(size = 8), 
        legend.position = "none", # Removes the legend
        #panel.background = element_blank(),
        plot.title = element_text(hjust=0.5))

afr_wet_prop <-  afr_biome_proportions %>% filter(biome =="Freshwater") %>%
  mutate(csum = rev(cumsum(rev(proportion))), 
         pos = proportion/2 + lead(csum, 1),
         pos_ad = if_else(is.na(pos), proportion/2, pos))

afr_wet_prop$yearCategory <- factor(afr_wet_prop$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))
#afr_wet_prop$yearCategory <- plyr::revalue(afr_wet_prop$yearCategory, c('pre 1980s' = 'pre 1980s', '1980s' ='1980s','1990s' = '1990s','2000s' = '2000s','post 2010s' = 'post\n2010s'))


afr_inset_wet <- ggplot(afr_wet_prop, aes(x = "" , y = proportion, fill = fct_inorder(yearCategory))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_text(aes(label = paste0(round(proportion,1),"%")),
            size = 2,
            position = position_stack(vjust = 0.5)) +
  #guides(fill = guide_legend(title = "Group")) +
  #scale_y_continuous(breaks = afr_wet_prop$pos_ad, labels = afr_wet_prop$yearCategory)+#c("pre 1980s", "1980s","1990s", "2000s", "post\n2010s")) +
 #ggtitle("African")+
  theme_void()+
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        #axis.text = element_text(size = 8), 
        legend.position = "none", # Removes the legend
        #panel.background = element_blank(),
        plot.title = element_text(hjust=0.5))

wetTimeSeries <- ggplot(plotTimeSeriesWet) +
  geom_line(size=2,aes(x=yearCategory, y=proportion, group=name)) +
  geom_point(aes(x=yearCategory, y=proportion,fill=name),shape=21, color="black", size=6) +
  #scale_fill_brewer(name = "Country",palette = "Paired")+
  scale_fill_manual(name = "Country", values = country_colors) +  # Apply consistent country colors for points
  scale_color_manual(name = "Country", values = country_colors) +
  labs(x = "Decades" , y = "Proportion of publications (%)") + 
  theme_ipsum(plot_title_size = 12) +
  ggtitle("Freshwater ecosystem study sites - 1960s to now")+
  theme(legend.position = "none")

#wetTimeSeries <- wetTimeSeries + inset_element(inset_wet, 0, 0.6, 0.4, 1)  +
#  inset_element(afr_inset_wet, 0.6, 0.6, 1, 1) 

top5_sites_coastal <- top_countries_biome %>% filter(biome == "Coastal")
plotTimeSeriesCoastal <- site_proportions_biome %>% filter(biome == "Coastal") %>% filter(sepISO3 %in% top5_sites_coastal$sepISO3)
#plotTimeSeriesCoastal <- plotTimeSeriesCoastal %>% left_join(countryNamesOnly, by = c("sepISO3" = "iso3"))
plotTimeSeriesCoastal$yearCategory <- factor(plotTimeSeriesCoastal$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))

coast_prop <- all_biome_proportions %>% filter(biome =="Coastal") %>%
  mutate(csum = rev(cumsum(rev(proportion))), 
         pos = proportion/2 + lead(csum, 1),
         pos_ad = if_else(is.na(pos), proportion/2, pos))

coast_prop$yearCategory <- factor(coast_prop$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))
#coast_prop$yearCategory <- plyr::revalue(coast_prop$yearCategory, c('pre 1980s' = 'pre 1980s', '1980s' ='1980s','1990s' = '1990s','2000s' = '2000s','post 2010s' = 'post\n2010s'))

inset_coast <- ggplot(coast_prop, aes(x = "" , y = proportion, fill = fct_inorder(yearCategory))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_text(aes(label = paste0(round(proportion,1),"%")),
            size = 2,
            position = position_stack(vjust = 0.5)) +
  #guides(fill = guide_legend(title = "Group")) +
  #scale_y_continuous(breaks = coast_prop$pos_ad, labels = coast_prop$yearCategory)+#c("pre 1980s", "1980s","1990s", "2000s", "post\n2010s")) +
  #ggtitle("All")+
  theme_void()+
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        #axis.text = element_text(size = 8), 
        legend.position = "none", # Removes the legend
        #panel.background = element_blank(),
        plot.title = element_text(hjust=0.5))

afr_coast_prop <-  afr_biome_proportions %>% filter(biome =="Coastal") %>%
  mutate(csum = rev(cumsum(rev(proportion))), 
         pos = proportion/2 + lead(csum, 1),
         pos_ad = if_else(is.na(pos), proportion/2, pos))

afr_coast_prop$yearCategory <- factor(afr_coast_prop$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))
#afr_coast_prop$yearCategory <- plyr::revalue(afr_coast_prop$yearCategory, c('pre 1980s' = 'pre 1980s', '1980s' ='1980s','1990s' = '1990s','2000s' = '2000s','post 2010s' = 'post\n2010s'))


afr_inset_coast <- ggplot(afr_coast_prop, aes(x = "" , y = proportion, fill = fct_inorder(yearCategory))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_text(aes(label = paste0(round(proportion,1),"%")),
            size = 2,
            position = position_stack(vjust = 0.5)) +
  #guides(fill = guide_legend(title = "Group")) +
  #scale_y_continuous(breaks = afr_coast_prop$pos_ad, labels = afr_coast_prop$yearCategory)+#c("pre 1980s", "1980s","1990s", "2000s", "post\n2010s")) +
  #ggtitle("African")+
  theme_void()+
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        #axis.text = element_text(size = 8), 
        legend.position = "none", # Removes the legend
        #panel.background = element_blank(),
        plot.title = element_text(hjust=0.5))
  
  #geom_label_repel(data = afr_coast_prop,
  #                 aes(y = pos, label = paste0(yearCategory, "\n(",round(proportion,1), "%)")),
  #                 size = 1.5, nudge_x = 1, show.legend = FALSE, fill = NA) +
  #theme_void()+
  #theme(legend.position = "none") +
  

coastalTimeSeries <- ggplot(plotTimeSeriesCoastal) +
  geom_line(size=2,aes(x=yearCategory, y=proportion, group=name)) +
  geom_point(aes(x=yearCategory, y=proportion,fill=name),shape=21, color="black", size=6) +
  #scale_fill_brewer(name = "Country",palette = "Paired")+
  scale_fill_manual(name = "Country", values = country_colors) +  # Apply consistent country colors for points
  scale_color_manual(name = "Country", values = country_colors) +
  labs(x = "Decades" , y = "Proportion of publications (%)") + 
  theme_ipsum(plot_title_size = 12) +
  ggtitle("Coastal study sites - 1960s to now") +
  theme(legend.position = "none")

#coastalTimeSeries <- coastalTimeSeries + inset_element(inset_coast, 0, 0.6, 0.4, 1)  +
#  inset_element(afr_inset_coast, 0.6, 0.6, 1, 1) 

pie_legend_plot <- ggplot(afr_coast_prop, aes(x = "" , y = proportion, fill = fct_inorder(yearCategory))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  guides(fill = guide_legend(title = "Decade")) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))


pie_legend <- ggdraw(cowplot::get_legend(pie_legend_plot))


all_pie_plot <- plot_grid(inset_forest,  inset_grass, inset_wet,  inset_coast, inset_ocean,
                      ncol = 5, nrow = 1, 
                      align = 'v',
                      labels = c('A', 'B', 'C', 'D', 'E'))

all_pie_plot_title <- ggdraw() + 
  draw_label("All",
             fontfamily = "Avenir", size = 12,
             x = 0, hjust = 0) 

afr_pie_plot <- plot_grid(afr_inset_forest,  afr_inset_grass, afr_inset_wet,  afr_inset_coast, afr_inset_ocean,
                          ncol = 5, nrow = 1, 
                          align = 'v',
                          labels = c('A', 'B', 'C', 'D', 'E'))
afr_pie_plot_title <- ggdraw() + 
  draw_label("Africa",
             fontfamily = "Avenir", size = 12,
             x = 0, hjust = 0) 

pie_plot <- plot_grid(all_pie_plot_title,all_pie_plot,afr_pie_plot_title,afr_pie_plot,pie_legend,
                      ncol = 1,
                      rel_heights = c(0.2, 1, 0.1,1,0.25))

combined_data <- bind_rows(plotTimeSeriesForests, plotTimeSeriesGrass,plotTimeSeriesWet, plotTimeSeriesCoastal,plotTimeSeriesOcean)

legend_plot <- ggplot(combined_data) +
  geom_line(size=2,aes(x=yearCategory, y=proportion, group=name)) +
  geom_point(aes(x=yearCategory, y=proportion,fill=name),shape=21, color="black", size=6) +
  scale_fill_manual(name = "Country", values = country_colors)+
  scale_color_manual(name = "Country", values = country_colors) +
  theme_void() +
  theme(legend.position = "bottom",legend.title = element_blank(),
        legend.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

afr_coast_prop$yearCategory <- factor(afr_coast_prop$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))


# Extract the legend from the legend plot
country_legend <- cowplot::get_legend(legend_plot)

comb_legend <- ggdraw(plot_grid(country_legend, pie_plot, ncol=1, nrow=2,align = 'v'))
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
windows(width = 20, height = 10)
print(combined_plots)
ggsave("AfricanSitesByBiome2.png", plot = combined_plots, width = 24, height = 10)
