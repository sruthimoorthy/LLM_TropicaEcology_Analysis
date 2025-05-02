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
library(stringr)

setwd("/Users/sruthikp/Work/Analysis/TropicalEcologyLitReview/DataFolder/")


publications <- read.csv("Tropical_filtered_publications.csv")

manualValidation <- read.csv('/Users/sruthikp/Work/Analysis/TropicalEcologyLitReview/BiotropicaManuscriptFiles/Datasets/ISOBiomeValidation.csv')

chatgpt_result <- data.frame(ID = publications$ID,
                         ChatGPT.Biome.Raw = publications$biome)

chatgpt_result <- chatgpt_result %>% filter(ID %in% manualValidation$ID)


publications_split <- chatgpt_result %>%
  separate_rows(ChatGPT.Biome.Raw, sep = ",") %>%
  mutate(biomeCat = trimws(ChatGPT.Biome.Raw)) 

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

publications_final <- publications_split %>%
  group_by(ID) %>%
  # Combine all standardized_biomes into a single string
  summarise(
    # If you want each biome listed once only:
    all_biomes = paste(unique(standardized_biome), collapse = ", ")
  ) %>%
  ungroup()

full_validation_dataset <- merge(manualValidation, publications_final, by = "ID")
write.csv(full_validation_dataset, "ISOBiomeValidationFinal.csv")
