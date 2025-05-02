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
                                ISO3 = publications$ISO3)


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

asiaOceaniaPubs <- publications_site_with_lat_cont %>% filter(continent == "Asia" | continent == "Oceania")


site_counts <- asiaOceaniaPubs %>%
  dplyr::group_by(yearCategory, sepISO3) %>%
  dplyr::summarise(unique_publications = n_distinct(ID), .groups = 'drop')

site_totals <- asiaOceaniaPubs %>%
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


asiaOceaniaISOCurrentLeadPlot <- ggplot(site_proportions_top_10_now) +
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


asiaOceaniaISOOldLeadPlot <- ggplot(site_proportions_top_10_old) +
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

asiaOceaniaISOCurrentLeadPlot <- asiaOceaniaISOCurrentLeadPlot + theme(legend.position = "none")
asiaOceaniaISOOldLeadPlot <- asiaOceaniaISOOldLeadPlot + theme(legend.position = "none")
library(ggpubr)
asiaOceaniaTopResearchHosts2010sVs1960s <- ggarrange(asiaOceaniaISOCurrentLeadPlot, asiaOceaniaISOOldLeadPlot,
                                                 nrow = 1, ncol = 2, labels = c("a", "b") )

asiaOceaniaTopResearchHosts2010sVs1960sLegend <- ggarrange(asiaOceaniaTopResearchHosts2010sVs1960s, country_legend, 
                                                       nrow = 2, ncol = 1, heights = c(2,1))
ggsave("asiaOceaniaStudySiteCurrentVsOldLead.png",plot = asiaOceaniaTopResearchHosts2010sVs1960sLegend, width = 10, height = 7, dpi = 600)


######################### asiaOceania decadal map of all study sites #################

afr_data <- world_data %>% filter(continent == "Asia" | continent == "Oceania")
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
  coord_sf(xlim = c(22, 180))+
  scale_fill_viridis(option="magma", na.value = "grey50",
                     name = "Proportion (%)") +
  labs(
    title = "Pre 1980s",
    #subtitle = "Number of publications per country",
  ) 

cholor_80s <- ggplot(merged_data_smpl_80s) +
  geom_sf(aes(fill = proportion), linewidth = 0, alpha = 0.9) +
  theme_void() +
  coord_sf(xlim = c(22, 180))+
  scale_fill_viridis(option="magma", na.value = "grey50",
                     name = "Proportion (%)") +
  labs(
    title = "1980s",
    #subtitle = "Number of publications per country",
  ) 

cholor_90s <- ggplot(merged_data_smpl_90s) +
  geom_sf(aes(fill = proportion), linewidth = 0, alpha = 0.9) +
  theme_void() +
  coord_sf(xlim = c(22, 180))+
  scale_fill_viridis(option="magma", na.value = "grey50",
                     name = "Proportion (%)") +
  labs(
    title = "1990s",
    #subtitle = "Number of publications per country",
  ) 

cholor_00s <- ggplot(merged_data_smpl_00s) +
  geom_sf(aes(fill = proportion), linewidth = 0, alpha = 0.9) +
  theme_void() +
  coord_sf(xlim = c(22, 180))+
  scale_fill_viridis(option="magma", na.value = "grey50",
                     name = "Proportion (%)") +
  labs(
    title = "2000s",
    #subtitle = "Number of publications per country",
  ) 

cholor_post10s <- ggplot(merged_data_smpl_post10s) +
  geom_sf(aes(fill = proportion), linewidth = 0, alpha = 0.9) +
  theme_void() +
  coord_sf(xlim = c(22, 180))+
  scale_fill_viridis(option="magma", na.value = "grey50",
                     name = "Proportion (%)") +
  labs(
    title = "Post 2010s",
    #subtitle = "Number of publications per country",
  ) 

library(patchwork)
asiaOceaniaDecadalMap <- 
  (cholor_pre80s + cholor_80s + cholor_90s) /            # top row (3 plots)
  (cholor_00s + cholor_post10s + plot_spacer())

ggsave("asiaOceaniaDecadalMap.png", plot = asiaOceaniaDecadalMap, width = 10, height = 8, dpi = 600)