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
library(viridis)
library(ggpubr)
library(patchwork)
library(hrbrthemes)
library(cowplot)
# Set the working directory to where your data files are located
setwd("/Users/sruthikp/Work/Analysis/TropicalEcologyLitReview/BiotropicaManuscriptFiles/Datasets")

publications <- read.csv("Tropical_filtered_publications.csv")

author_aff <- read.csv("AuthorAffiliationResults.csv")

author_aff <- author_aff %>% filter(UT..Unique.WOS.ID. %in% publications$ID)

author_aff_data <- author_aff %>% distinct(UT..Unique.WOS.ID.,.keep_all = TRUE) 

tropical_studies <- data.frame(ID = author_aff_data$UT..Unique.WOS.ID.,
                              year = author_aff_data$Publication.Year,
                              first_author_countries = author_aff_data$First.Author.ISO3,
                              all_author_countries = author_aff_data$All.Author.ISO3,
                              studySite = author_aff_data$ISO3_std)

rm(publications)

# Clean the author country lists by removing unwanted characters (e.g., brackets)
tropical_studies$first_author_countries <- gsub("\\[|\\]|\\{|\\}|\\'", "", tropical_studies$first_author_countries)

# Filter out studies with empty or irrelevant study country information
tropical_studies <- tropical_studies %>%
  filter(!(grepl("not found", first_author_countries) | first_author_countries == "" | is.na(first_author_countries)))

publications_site <- tropical_studies %>%
  separate_rows(first_author_countries, sep = ",") %>%
  mutate(sepISO3 = trimws(first_author_countries))

summary <- publications_site %>%
  mutate(yearCategory = case_when(
    year < 1980 ~ "pre 1980s",
    year >= 1980 & year < 1990 ~ "1980s",
    year >= 1990 & year < 2000 ~ "1990s",
    year >= 2000 & year < 2010 ~ "2000s",
    year >= 2010  ~ "post 2010s"
  )) 

summary$yearCategory <- as.factor(summary$yearCategory)

summary_no_other <- summary %>% filter(!(sepISO3 == "" | is.na(sepISO3)))

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


site_proportions <- site_proportions %>%
  group_by(yearCategory) %>%
  mutate(rank = rank(-proportion, ties.method = "min"))

#combined_ranks <- site_proportions %>%
#  group_by(sepISO3) %>%
#  summarise(combined_rank = sum(rank, na.rm = TRUE), .groups = 'drop')


# Step 2: Count how many decades each country is present in
country_decade_count <- site_proportions %>%
  group_by(sepISO3) %>%
  summarise(decade_count = n_distinct(yearCategory), .groups = 'drop')

# Step 3: Sum ranks across all decades for each country within each biome,
# but only include those with more than one decade
combined_ranks <- site_proportions %>%
  group_by(sepISO3) %>%
  summarise(combined_rank = sum(rank, na.rm = TRUE), .groups = 'drop') %>%
  inner_join(country_decade_count %>% filter(decade_count > 3), by = c("sepISO3")) 

# Step 4: Select the top 10 countries based on the combined rank
top_10_countries <- site_proportions %>%
  filter(yearCategory == "post 2010s") %>%
  arrange(rank) %>%
  slice(1:10)


# Step 5: Filter the site_proportions data for the top 10 countries
site_proportions_top_10 <- site_proportions %>%
  filter(sepISO3 %in% top_10_countries$sepISO3)

world <- ne_countries(scale = "medium", returnclass = "sf")
invalid_geometries <- st_is_valid(world, reason = TRUE)
world <- st_make_valid(world)

# Select relevant columns
world_data <- world %>% select(name, adm0_a3, continent,pop_est)

countryNames <- world_data %>% filter(adm0_a3 %in% top_10_countries$sepISO3)

countryNamesOnly <- data.frame(iso3 = countryNames$adm0_a3,
                               name = countryNames$name
                               ) 

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

custom_breaks <- c(0, 1, 2, 3, 4)  # Define specific positions for ticks
custom_labels <- c(round(exp(0),0), round(exp(1),0), round(exp(2),0), round(exp(3),0), round(exp(4),0))  

currentLeadingISO <- ggplot(site_proportions_top_10) +
  geom_line(size=1,color = 'grey',aes(x=yearCategory, y=log(proportion), group=name)) +
  geom_point(aes(x=yearCategory, y=log(proportion),fill=name),shape=21, color="black", size=6) +
  scale_fill_manual(name = "Country", values = country_colors) +  # Apply consistent country colors for points
  scale_color_manual(name = "Country", values = country_colors) +
  scale_y_continuous(breaks = custom_breaks, labels = custom_labels) + 
  
  labs(x = "Decades" , y = "Publications (%)") + 
  theme_ipsum(plot_title_size = 14) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 14),
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

top_10_countries_hist <- site_proportions %>%
  filter(yearCategory == "pre 1980s") %>%
  arrange(rank) %>%
  slice(1:10)


# Step 5: Filter the site_proportions data for the top 10 countries
site_proportions_top_10_hist <- site_proportions %>%
  filter(sepISO3 %in% top_10_countries_hist$sepISO3)

world <- ne_countries(scale = "medium", returnclass = "sf")
invalid_geometries <- st_is_valid(world, reason = TRUE)
world <- st_make_valid(world)

# Select relevant columns
world_data <- world %>% select(name, adm0_a3, continent,pop_est)

countryNames <- world_data %>% filter(adm0_a3 %in% top_10_countries_hist$sepISO3)

countryNamesOnly <- data.frame(iso3 = countryNames$adm0_a3,
                               name = countryNames$name
) 

site_proportions_top_10_hist <- site_proportions_top_10_hist %>% left_join(countryNamesOnly, by = c("sepISO3" = "iso3"))

site_proportions_top_10_hist$yearCategory <- factor(site_proportions_top_10_hist$yearCategory, levels=c('pre 1980s', '1980s','1990s','2000s','post 2010s'))

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

remaining_countries <- site_proportions_top_10_hist %>% filter(!(name %in% c("Brazil",
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

custom_breaks <- c(0, 1, 2, 3, 4)  # Define specific positions for ticks
custom_labels <- c(round(exp(0),0), round(exp(1),0), round(exp(2),0), round(exp(3),0), round(exp(4),0))  

oldLeadingISO <- ggplot(site_proportions_top_10_hist) +
  geom_line(size=1,color = 'grey',aes(x=yearCategory, y=log(proportion), group=name)) +
  geom_point(aes(x=yearCategory, y=log(proportion),fill=name),shape=21, color="black", size=6) +
  scale_fill_manual(name = "Country", values = country_colors) +  # Apply consistent country colors for points
  scale_color_manual(name = "Country", values = country_colors) +
  scale_y_continuous(breaks = custom_breaks, labels = custom_labels) + 
  
  labs(x = "Decades" , y = "Publications (%)") + 
  theme_ipsum(plot_title_size = 14) +
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

currentLeadingISO <- currentLeadingISO + theme(legend.position = "none")
oldLeadingISO <- oldLeadingISO + theme(legend.position = "none")

topResearchLeads2010sVs1960s <- ggarrange(currentLeadingISO, oldLeadingISO,
                                          nrow = 1, ncol = 2, labels = c("a", "b") )

topResearchLeads2010sVs1960sLegend <- ggarrange(topResearchHosts2010sVs1960s, country_legend, 
                                                nrow = 2, ncol = 1, heights = c(2,1))

ggsave("ResearchLeadCurrentVsOld.png",plot = topResearchLeads2010sVs1960sLegend, width = 10, height = 7, dpi = 600)
