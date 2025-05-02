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

# Set the working directory to where your data files are located
setwd("/Users/sruthikp/Work/Analysis/TropicalEcologyLitReview/BiotropicaManuscriptFiles/Datasets")

# Read the publication data from a CSV file into a dataframe
publications <- read.csv("sourceDatasetForAnalysis.csv")

# Create a dataframe for tropical studies, selecting relevant columns
tropical_studies <- data.frame(
  ID = publications$ID,
  year = publications$Year,
  first_author_countries = publications$FirstAuthorCountry
)

rm(publications)

# Clean the author country lists by removing unwanted characters (e.g., brackets)
tropical_studies$first_author_countries <- gsub("\\[|\\]|\\{|\\}|\\'", "", tropical_studies$first_author_countries)

# Filter out studies with empty or irrelevant study country information
tropical_studies <- tropical_studies %>%
  filter(!(study_country == "" | is.na(study_country) | study_country == "Other")) %>%
  filter(!(grepl("not found", first_author_countries) | first_author_countries == "" | is.na(first_author_countries)))


publications_site <- tropical_studies %>%
  separate_rows(first_author_countries, sep = ",") %>%
  mutate(sepISO3 = trimws(first_author_countries))


publications_site <- publications_site %>% filter(year >= 2010)

publications_site <- publications_site %>% filter(!(sepISO3 =="not found" | sepISO3 == "" | is.na(sepISO3)))

# Assume publications_site is already created and contains the filtered data
# Step 1: Count the number of publications per year for Brazil and the US
yearly_publications <- publications_site %>%
  filter(sepISO3 %in% c("BRA", "USA")) %>%
  group_by(year, sepISO3) %>%
  summarise(publication_count = n(), .groups = 'drop')

# Step 2: Calculate total publications per year
total_publications <- yearly_publications %>%
  group_by(year) %>%
  summarise(total_count = sum(publication_count), .groups = 'drop')

# Step 3: Merge to calculate proportions
proportions <- yearly_publications %>%
  left_join(total_publications, by = "year") %>%
  mutate(proportion = publication_count / total_count) %>%
  select(year, sepISO3, proportion)

# Step 4: Reshape data for plotting
proportions_long <- proportions %>%
  pivot_wider(names_from = sepISO3, values_from = proportion, values_fill = 0)

# Step 5: Create the plot with ggplot2
brazilUSAPlot <- ggplot(proportions_long, aes(x = year)) +
  geom_line(aes(y = BRA*100, color = "Brazil"), size = 1) +
  geom_line(aes(y = USA*100, color = "United States"), size = 1) +
  labs(title = "Proportion of Publications in Tropical Ecology (Brazil vs. USA)",
       x = "Year",
       y = "Lead Author Publications (%)",
       color = "Country") +
  scale_color_manual(values = c("Brazil" = "#1f77b4", "United States" = "#6a3d9a")) +
  theme_minimal()+
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

##################### Combined plot ###########################

emptyPlot <- ggplot() + theme_void()

# 2. Place your globalBiomePlot in the middle of a 3-column arrangement
bottomCentered <- ggarrange(
  emptyPlot,       # left column (blank)
  brazilUSAPlot, # center column (your real plot)
  emptyPlot,       # right column (blank)
  ncol = 3, 
  widths = c(1, 2, 1),
  labels = c("","c","") # center plot gets more space
)

leadAuthorCombPlot <- ggarrange(topResearchLeads2010sVs1960sLegend, bottomCentered, nrow = 2,ncol = 1)

ggsave("leadAuthorCombPlot.png", plot = leadAuthorCombPlot, width = 12, height = 10, dpi = 800)
