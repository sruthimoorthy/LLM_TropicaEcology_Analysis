library(tidyr)
library(dplyr)
library(countrycode)
library(ggplot2)
library(maps)
library("rnaturalearth")
library("rnaturalearthdata")
library(sf)
library(tmap)
library(lwgeom)
library(ggpubr)
library(viridis)
library(hrbrthemes)
library(cowplot)
library(patchwork)

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


site_counts <- summary_no_other %>%
  group_by(yearCategory, sepISO3) %>%
  summarise(unique_publications = n_distinct(ID), .groups = 'drop')

site_totals <- summary_no_other %>%
  group_by(yearCategory) %>%
  summarise(total_publications = n_distinct(ID), .groups = 'drop') %>%
  arrange(desc(total_publications)) 

# Recalculate the proportions with "Other" category
site_proportions <- site_counts %>%
  left_join(site_totals, by = "yearCategory") %>%
  mutate(proportion = (unique_publications / total_publications) * 100) %>%
  arrange(desc(proportion))

country_decade_count <- site_proportions %>%
  group_by(sepISO3) %>%
  summarise(decade_count = n_distinct(yearCategory), .groups = 'drop')  # Count distinct years

min_decade_threshold <- 1 # Define the minimum threshold for decade count (e.g., 3 decades)

eligible_countries <- country_decade_count %>%
  filter(decade_count >= min_decade_threshold)

# Step 3: Rank only those countries based on their publication proportion
site_proportions <- site_proportions %>%
  filter(sepISO3 %in% eligible_countries$sepISO3) %>%  # Keep only countries that pass the threshold
  group_by(yearCategory) %>%
  mutate(rank = rank(-proportion, ties.method = "min"))

# Step 4: Select the top 10 countries leading post 2010s
top_10_countries<- site_proportions %>%
  filter(yearCategory == 'post 2010s') %>%
  arrange(rank) %>%
  slice(1:10)

# Step 4: Select the top 10 countries leading in the 60s & 70s
top_10_countries_1960s<- site_proportions %>%
  filter(yearCategory == 'pre 1980s') %>%
  arrange(rank) %>%
  slice(1:10)



# Step 5: Filter the site_proportions data for the top 10 countries
site_proportions_top_10 <- site_proportions %>%
  filter(sepISO3 %in% top_10_countries$sepISO3)

site_proportions_top_10_1960s <- site_proportions %>%
  filter(sepISO3 %in% top_10_countries_1960s$sepISO3)

world <- ne_countries(scale = "medium", returnclass = "sf")
invalid_geometries <- st_is_valid(world, reason = TRUE)
world <- st_make_valid(world)

# Select relevant columns
world_data <- world %>% select(name, iso_a3, continent)

countryNames <- world_data %>% filter(iso_a3 %in% top_10_countries$sepISO3)

countryNamesOnly <- data.frame(iso3 = countryNames$iso_a3,
                               name = countryNames$name)

site_proportions_top_10 <- site_proportions_top_10 %>% left_join(countryNamesOnly, by = c("sepISO3" = "iso3"))

site_proportions_top_10$yearCategory <- factor(site_proportions_top_10$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))

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

remaining_countries <- site_proportions_top_10 %>% filter(!(name %in% c("Brazil",
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

currentLeadISOPlot <- ggplot(site_proportions_top_10) +
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



currentLeadISOLogPlot <- ggplot(site_proportions_top_10) +
  geom_line(size=1,aes(x=yearCategory, y=log(proportion), color = 'grey',group=name)) +
  geom_point(aes(x=yearCategory, y=log(proportion),fill=name),shape=21, color="black", size=6) +
  scale_fill_manual(name = "Country", values = country_colors) +  # Apply consistent country colors for points
  scale_color_manual(name = "Country", values = country_colors) +
  scale_y_continuous(trans = 'log', 
                     labels = function(x) format(round(exp(x),0), scientific = FALSE)) + 
  labs(x = "Decades" , y = "Publications (%)") + 
  theme_ipsum(plot_title_size = 12)  +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 14),
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

countryNames <- world_data %>% filter(iso_a3 %in% top_10_countries_1960s$sepISO3)

countryNamesOnly <- data.frame(iso3 = countryNames$iso_a3,
                               name = countryNames$name) 

site_proportions_top_10_1960s <- site_proportions_top_10_1960s %>% left_join(countryNamesOnly, by = c("sepISO3" = "iso3"))

site_proportions_top_10_1960s$yearCategory <- factor(site_proportions_top_10_1960s$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))

remaining_countries <- site_proportions_top_10_1960s %>% filter(!(name %in% c("Brazil",
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

oldLeadISOPlot <- ggplot(site_proportions_top_10_1960s) +
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

oldLeadISOLogPlot <- ggplot(site_proportions_top_10_1960s) +
  geom_line(size=1,aes(x=yearCategory, y=log(proportion), color = 'grey',group=name)) +
  geom_point(aes(x=yearCategory, y=log(proportion),fill=name),shape=21, color="black", size=6) +
  scale_fill_manual(name = "Country", values = country_colors) +  # Apply consistent country colors for points
  scale_color_manual(name = "Country", values = country_colors) +
  scale_y_continuous(trans = 'log', 
                     labels = function(x) format(round(exp(x),0), scientific = FALSE)) + 
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

combined_data <- bind_rows(site_proportions_top_10, site_proportions_top_10_1960s)

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

currentLeadISOPlot <- currentLeadISOPlot + theme(legend.position = "none")
oldLeadISOPlot <- oldLeadISOPlot + theme(legend.position = "none")

topResearchHosts2010sVs1960s <- ggarrange(currentLeadISOPlot, oldLeadISOPlot,
                                          nrow = 1, ncol = 2, labels = c("a", "b") )

topResearchHosts2010sVs1960sLegend <- ggarrange(topResearchHosts2010sVs1960s, country_legend, 
                                                nrow = 2, ncol = 1, heights = c(2,1))

currentLeadISOLogPlot <- currentLeadISOLogPlot + theme(legend.position = "none")
oldLeadISOLogPlot <- oldLeadISOLogPlot + theme(legend.position = "none")

topResearchHosts2010sVs1960sLog <- ggarrange(currentLeadISOLogPlot, oldLeadISOLogPlot,
                                          nrow = 1, ncol = 2, labels = c("a", "b") )

topResearchHosts2010sVs1960sLogLegend <- ggarrange(topResearchHosts2010sVs1960sLog, country_legend, 
                                                nrow = 2, ncol = 1, heights = c(2,1))

ggsave("StudySiteCurrentVsOldLead.png",plot = topResearchHosts2010sVs1960sLegend, width = 10, height = 7, dpi = 600)


############################## Map showing the data for all countries #####################

summary_pro_pre80s <- site_proportions %>% filter(yearCategory == 'pre 1980s')
summary_pro_80s <- site_proportions %>% filter(yearCategory == '1980s')
summary_pro_90s <- site_proportions %>% filter(yearCategory == '1990s')
summary_pro_00s <- site_proportions %>% filter(yearCategory == '2000s')
summary_pro_post10s <- site_proportions %>% filter(yearCategory == 'post 2010s')


merged_data_smpl_pre80s <- world_data %>%
  left_join(summary_pro_pre80s, by = c("iso_a3" = "sepISO3")) %>%
  mutate(pub = ifelse(is.na(proportion),NA, proportion)) %>%
  mutate(log_proportion = log1p(proportion)) 

top_10_sites_pre80s <- merged_data_smpl_pre80s %>%
  group_by(yearCategory) %>%
  slice_max(order_by = proportion, n = 10) %>%
  ungroup()

merged_data_smpl_80s <- world_data %>%
  left_join(summary_pro_80s, by = c("iso_a3" = "sepISO3")) %>%
  mutate(pub = ifelse(is.na(proportion), NA, proportion)) %>%
  mutate(log_proportion = log1p(proportion))

merged_data_smpl_90s <- world_data %>%
  left_join(summary_pro_90s, by = c("iso_a3" = "sepISO3")) %>%
  mutate(pub = ifelse(is.na(proportion),NA, proportion))%>%
  mutate(log_proportion = log1p(proportion))

merged_data_smpl_00s <- world_data %>%
  left_join(summary_pro_00s, by = c("iso_a3" = "sepISO3")) %>%
  mutate(pub = ifelse(is.na(proportion), NA, proportion))%>%
  mutate(log_proportion = log1p(proportion))

merged_data_smpl_00s <- world_data %>%
  left_join(summary_pro_00s, by = c("iso_a3" = "sepISO3")) %>%
  mutate(pub = ifelse(is.na(proportion), NA, proportion))%>%
  mutate(log_proportion = log1p(proportion))

merged_data_smpl_post10s <- world_data %>%
  left_join(summary_pro_post10s, by = c("iso_a3" = "sepISO3")) %>%
  mutate(pub = ifelse(is.na(proportion), NA, proportion))%>%
  mutate(log_proportion = log1p(proportion))


cholor_pre80s <- ggplot(merged_data_smpl_pre80s) +
  geom_sf(aes(fill = proportion), linewidth = 0, alpha = 0.9) +
  theme_void() +
  scale_fill_viridis(option="magma", na.value = "grey50",
                     name = "Proportion (%)") +
  labs(
    subtitle = "1960s & 1970s"
  
  ) 

cholor_80s <- ggplot(merged_data_smpl_80s) +
  geom_sf(aes(fill = proportion), linewidth = 0, alpha = 0.9) +
  theme_void() +
  scale_fill_viridis(option="magma", na.value = "grey50",
                     name = "Proportion (%)") +
  labs(
    subtitle = "1980s"
  ) 

cholor_90s <- ggplot(merged_data_smpl_90s) +
  geom_sf(aes(fill = proportion), linewidth = 0, alpha = 0.9) +
  theme_void() +
  scale_fill_viridis(option="magma", na.value = "grey50",
                     name = "Proportion (%)") +
  labs(
    subtitle = "1990s"
    #subtitle = "Number of publications per country",
  ) 

cholor_00s <- ggplot(merged_data_smpl_00s) +
  geom_sf(aes(fill = proportion), linewidth = 0, alpha = 0.9) +
  theme_void() +
  scale_fill_viridis(option="magma", na.value = "grey50",
                     name = "Proportion (%)") +
  labs(
    subtitle = "2000s"
  ) 

cholor_post10s <- ggplot(merged_data_smpl_post10s) +
  geom_sf(aes(fill = proportion), linewidth = 0, alpha = 0.9) +
  theme_void() +
  scale_fill_viridis(option="magma", na.value = "grey50",
                     name = "Proportion (%)") +
  labs(
    subtitle = "Post 2010s"
  ) 


decadealPopularStudySiteMap <- ggarrange(cholor_pre80s, cholor_80s, cholor_90s, cholor_00s, cholor_post10s, ncol = 2, nrow = 3)

ggsave("decadealPopularStudySiteMap.png", plot = decadealPopularStudySiteMap, width = 10, height = 14, dpi = 800)


